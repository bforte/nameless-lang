# nameless

Simple brainfuck derivative, [here](https://esolangs.org/wiki/Nameless_language)'s the Esolangs wiki entry.

| Command | Short | Action |
|:-------:|:-----:|--------|
| `0000`  |	`>`   | Move pointer to the right. |
| `0001`  |	`<`   | Move pointer to the left. |
| `0010`  |	`+`   | Incriment value under pointer. |
| `0011`  |	`-`   | Decrement value under pointer. |
| `0100`  |	`.`   | Output current value as ascii. |
| `0101`  |	`,`   | Ask the user for input as ascii. |
| `0110`  |	`[`   | If the byte under the pointer is zero, jump the pointer forward to the next matching 0111 command. |
| `0111`  |	`]`   | If the byte under the pointer is zero, jump the pointer back to the matching 0110 command. |
| `1000`  |	`a`   | Adds the number represented in binary of the next command. |
| `1001`  |	`s`   | Subtracts the number represented in binary of the next command. |
| `1010`  |	`r`   | Sets the current cell to either 1 or 0. |
| `1011`  |	`c`   | Resets the current cell to value 0. |
| `1100`  |	`p`   | Resets the pointer to 0. |


## Interpreter

Additionally to being able to parse strings of *0*s and *1*s the interpreter is also able to parse shortcuts for these (see the above table) and parsing half-bytes as described above from raw bytestrings. These three modes (*S*hortcuts, *N*ormal and *B*ytestrings) can be used for either reading the source or translating another format to that one (`-t` flag).

```
usage: nameless (-e expr | file) [-b | -s] [-i file] [-t (S|N|B)]
  -e         --expr          supply source via command
  -b         --bytes         read source as bytestream
  -s         --short         use shortcuts in source
  -i file    --input=file    read inputs from file
  -t format  --trans=format  translate shortcuts
  -v         --verbose       print debug information
```
