# Inf2D-cw1--autotester

Autotester for Inf2D - coursework 1: graph search

## Notes
The tester assumes you do not make use of the numNodes variable (tbf, there are no variables in Haskell). It does not make sense to use it, and I think this means they'll only test the `next` function with 4x4 graphs.

Anyways, I do not because edge cases are important, so if you use `numNodes`, you should replace it with `(ceiling . sqrt . fromIntegral . length $ graph)` wherever you use it (assuming your graph is called `graph` and is in scope).

## Installation

Just git clone where your coursework files are:

```bash
git clone https://github.com/lollobaldo/Inf2D-cw1--autotester.git
```

## Usage

Just `cd` into the repository and run the `runTests` command (remember to give executable permission on Linux):

### Linux/DICE
```bash
cd Inf2D-cw1--autotester
chmod +x runTests.sh
./runTests
```
---
### Windows
```batch
cd Inf2D-cw1--autotester
runTests.bat
```
**Note:** needs Git bash to run on Windows, VSCode terminal not supported.

## --no-timeout
The Tester currently stops a function if it does not return any value after 5 seconds. Pass it a `--no-timeout` flag if you want to run it indefinitely.

:heavy_exclamation_mark:**NOTE:** this may result in a stack/heap overflow error

## --no-meme
Pass it a `--no-meme` flag if you don't like to see LambdaMan :cry:

## Bugs
Shoot me a text on Facebook if there's anything wrong.

## License
[MIT](https://choosealicense.com/licenses/mit/)
