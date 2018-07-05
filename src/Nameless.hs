{-# LANGUAGE DeriveFunctor,FlexibleContexts,GeneralizedNewtypeDeriving,LambdaCase,NoMonomorphismRestriction,TemplateHaskell #-}

module Main where

import Control.Monad.State.Strict
import Data.Char
import Data.Int
import Data.List
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Numeric
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random
import Text.Parsec

import qualified Control.Exception          as E
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C


data Stmt i = R | L | Inc | Dec | Out | In | Add i | Sub i | Rnd | ReC | ReP
            | Loop [Stmt i]
  deriving (Eq,Functor,Show)

enum :: a -> [Stmt a]
enum n = [R,L,Inc,Dec,Out,In,R,R,Add n,Sub n,Rnd,ReC,ReP]

data Zip m a = Zip { _lft :: [a], _cell :: a, _rgt :: [a], _meta :: m }

instance Show a => Show (Zip b a) where
  show (Zip xs y zs _) = init (show $ reverse xs) ++ a ++ show [y] ++ b
                      ++ tail (show zs)
    where a | null xs = "" | otherwise = ","
          b | null zs = "" | otherwise = ","

data Env = Env { _tape :: Zip Integer Int8, _input :: Handle }

data Opts = Opts
  { _expr    :: Bool
  , _fmt     :: Fmt
  , _inp     :: Maybe String
  , _trans   :: Maybe String
  , _verbose :: Bool
  }

data Fmt = S | N | B deriving Read

makeLenses ''Zip
makeLenses ''Env
makeLenses ''Opts

newtype NL a = NL { runNL :: StateT Env IO a }
  deriving (Functor, Applicative, Monad, MonadState Env, MonadIO)

normal = mapM (pure "01") [1..4]
short  = "><+-.,[]asrcp"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile ((==n) . length) . unfoldr (Just . splitAt n)

bin :: (Integral a,Show a) => a -> String
bin x | b <- showIntAtBase 2 ("01"!!) x "" = replicate (8-length b) '0' ++ b

data Token = T Int Int

instance Show Token where
  show (T i _) = str ++ " (" ++ drop 4 (bin i) ++ ")"
    where str | 0 <= i && i <= 12 = show [short !! i]
              | otherwise = show i ++ " (" ++ drop 4 (bin i) ++ ")"

parseSrc :: String -> Either ParseError [Stmt Int]
parseSrc = parse (many stmtP <* eof) "code"
      . filter ((`elem` [0..12]) . (\(T i _) -> i))
      . (zipWith T <*> (++[0]).tail)
      . map (fst . head . readInt 2 (`elem` "01") (read . pure))
      . chunksOf 4
      . filter (`elem` "01") where

  stmtP = (parenP 6 >> Loop <$> manyTill stmtP (try $ eof <|> parenP 7))
       <|> simpleP

  tokenParser = tokenPrim show (\pos _ _ -> incSourceColumn pos 1)

  simpleP = tokenParser test where
    test (T i n) | i == 6 || i == 7  = Nothing
                 | otherwise = Just (enum n !! i)

  parenP i = tokenParser test where
    test (T j _) | i == j = Just ()
                 | otherwise = Nothing

showAs :: Fmt -> [Stmt Int] -> String
showAs f src = case f of
  S -> flatten (map pure short !!)
  N -> flatten (normal !!)
  B -> [chr $ a*16 + b | [a,b] <- chunksOf 2 . pad $ concatMap ints src]

  where flatten by = concatMap (concatMap by . ints) src

        ints (Loop xs) = 6 : concatMap ints xs ++ [7]
        ints x = take 1 [i | (i,y) <- zip [0..] $ enum (), y == (()<$x)]

        pad xs | even (length xs) = xs | otherwise = xs ++ [0]

transpile :: Fmt -> String -> String
transpile B = transpileBytes
transpile S = transpileShort
transpile N = id

transpileBytes, transpileShort :: String -> String
transpileBytes = concatMap bin . B.unpack . C.pack
transpileShort = concatMap c2b . filter (`elem` short) where
  c2b c  = head [b|(x,b)<-zip short normal, x==c]

eval :: Bool -> [Stmt Int] -> Handle -> IO Env
eval v src h = execStateT (runNL $ mapM_ stmt' src) init where

  init = Env (Zip [] 0 [] 0) h

  stmt' s = do
    when v (use tape >>= io . hPutStrLn stderr . (show s++) . ('\t':) . show)
    stmt s

  stmt R   = tape %= stepR
  stmt L   = tape %= stepL
  stmt Inc = tape . cell %= (+1)
  stmt Dec = tape . cell %= subtract 1
  stmt Out = use (tape . cell) >>= io . putChar . chr . (`mod` 128) . fi
  stmt In = use input >>= getChar >>= (tape . cell .=) . fi . ord
  stmt (Add n) = tape . cell %= (+fi n)
  stmt (Sub n) = tape . cell %= subtract (fi n)
  stmt Rnd = io (randomRIO (0,1)) >>= (tape . cell .=)
  stmt ReC = tape . cell .= 0
  stmt ReP = tape %= reset
  stmt l@(Loop bs) = use (tape . cell) >>= \case
    0 -> return ()
    _ -> mapM_ stmt' bs >> stmt l

  getChar h = io (E.try $ hGetChar h) >>= \case
    Left e -> io (hPrint stderr (e :: E.SomeException)) >> return '\0'
    Right c -> return c

  reset z@(Zip _ _ _ ix)
    | ix > 0 = reset $ stepL z
    | ix < 0 = reset $ stepR z
    | otherwise = z

  stepR (Zip xs y (z:zs) ix) = Zip (y:xs) z zs (ix+1)
  stepR (Zip xs y [    ] ix) = Zip (y:xs) 0 [] (ix+1)

  stepL (Zip (x:xs) y zs ix) = Zip xs x (y:zs) (ix-1)
  stepL (Zip [    ] y zs ix) = Zip [] 0 (y:zs) (ix-1)

  fi = fromIntegral
  io = liftIO


main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (args,src:_,[]) -> evalArgs (foldr ($) defaults args) src
  (_,[],_)        -> die "missing file/expression"
  (_,_,err)       -> die $ concat err

  where

    evalArgs (Opts e f i t v) s = do
      hSetBuffering stderr NoBuffering
      hSetBuffering stdout NoBuffering
      (parseSrc . transpile f <$> if e then return s else readFile s) >>= \case
        Left err -> ioError . userError $ show err
        Right stmts -> do
          case map toUpper <$> t of
            Nothing -> inputHandle i >>= eval v stmts >>= hPrint stderr . (^. tape)
            Just cs
              | cs `elem` map pure "SNB" -> putStr $ showAs (read cs) stmts
              | otherwise -> die "format must be one of S,N or B"

    inputHandle (Just "-") = return stdin
    inputHandle Nothing = return stdin
    inputHandle (Just fname) = openFile fname ReadMode

    defaults = Opts False N Nothing Nothing False

    options =
      [ Option "e" ["expr"] (NoArg (& expr .~ True)) "supply source via command"
      , Option "b" ["bytes"] (NoArg (& fmt .~ B)) "read source as bytestream"
      , Option "s" ["short"] (NoArg (& fmt .~ S)) "use shortcuts in source"
      , Option "i" ["input"] (ReqArg (($).(inp.~).Just) "file") "read inputs from file"
      , Option "t" ["trans"] (ReqArg (($).(trans.~).Just) "format") "translate shortcuts"
      , Option "v" ["verbose"] (NoArg (& verbose .~ True)) "print debug information"
      ]

    usage = "usage: nameless (-e expr | file) [-b | -s] [-i file] [-t (S|N|B)]"

    die m = ioError . userError $ m ++ "\n" ++ usageInfo usage options
