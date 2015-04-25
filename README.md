# racket
MSc CS, PPL Racket assignment 2015

This is a sudoku solving programming which, at the moment, solves "easy" puzzles. To run it open the file *sudoku.rkt* in the Dr Racket IDE. You then pass a sudoku puzzle to the ```SOLVE``` function. The steps are:

1. In the interactions window, type ```(solve <puzzle>)``` where ```<puzzle>``` is either pre-defined or a "puzzle literal";
2. Whether pre-defined or literal, a puzzle has the following form: ```'((<row 1>) (<row 2>) ... (<row 9>))```;
3. A ```<row>``` is a Racket list where unknown row numbers are represented by zeros, e.g. ```'(1 4 0 0 9 0 0 0 7)```;
4. After running ```(solve puzzle)``` the output is a list of lists with all the unknown row positions solved. 
