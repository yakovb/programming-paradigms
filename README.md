#MSc CS, PPL Ruby assignment 2015
## ruby
This is a simple library management programme. It is meant to help a librarian manage library members and the books lent to them. Most methods in the library.rb file are public so this source file functions as the API (the method names are self explanatory). The key is that before any action can be taken, the library must be open and the customer must be a member of the library. 

The programme isn't run from the command line - you'd have to use it from within your own Ruby session. However, if you want to give it a test drive with the test suite I've written for it you can do this by opening a Ruby terminal from the folder where the source files are located. Then type ```ruby library_test.rb```. This should run 75 tests, all passing (they do on my system!). 

## erlang
This is a simple temperature conversion programme, from Celsius to Fahrenheit and vice versa. To run it you must:

1. Open an Erlang terminal from the folder holding the source files;
2. Compile each file with the command ```c(<source name without extension>).```;
3. Run with the test data by typing: ```control:tester().```.

Alternatively you can spawn your own control actor and give it temperatures to convert:

1. Spawn a control: ```Ctrl = spawn(fun control:loop/0).```;
2. Activate the control: ```Ctrl ! kick_off.```;
3. Send it a message: ```Ctrl ! {convert_C, <Celsius temp>}.``` or ```Ctrl ! {convert_F, <Fahrenheit temp>}.```;
4. Shut it down with ```Ctrl ! shutdown.```.

Hours of fun can be had this way. 

## prolog
This is a maze solving programming that currently works on mazes of dimension 5 rows x 9 columns. The maze has barriers (you could call them walls, too) which are identified by cartesian coordinates. The barriers are listed in the file maze.pl. To run the maze solver:

1. Open you Prolog terminal from the folder in which the source files are located;
2. Compile the files with the command: ```consult([maze, 'maze-solver']).```;
3. Start the path search with the command: ```solve([<startRow>, <startCol>], [<endRow>, <endCol>], Path).```;
4. To find an alternative path, enter a semicolon ```;``` or to stop searching enter a full stop ```.```.

Happy path-finding! 

## racket
This is a sudoku solving programming which, at the moment, solves "easy" puzzles. To run it open the file *sudoku.rkt* in the Dr Racket IDE. You then pass a sudoku puzzle to the ```SOLVE``` function. The steps are:

1. In the interactions window, type ```(solve <puzzle>)``` where ```<puzzle>``` is either pre-defined or a "puzzle literal";
2. Whether pre-defined or literal, a puzzle has the following form: ```'((<row 1>) (<row 2>) ... (<row 9>))```;
3. A ```<row>``` is a Racket list where unknown row numbers are represented by zeros, e.g. ```'(1 4 0 0 9 0 0 0 7)```;
4. After running ```(solve puzzle)``` the output is a list of lists with all the unknown row positions solved. 

## scala
This programme encodes plain text using the Playfair cipher. It also decodes enciphered text back into the original text. Text is encoded and decoded using a keyword. For all of the test files in the test package, the keyword is *Pennsylvania*. To run the programme compile the Code and Playfair files and run the Playfair class, which has a ```main``` method. You will then be presented with text options on how to proceed. 

If you want to run the tests, you need to use Scalatest 2.10.
