# prolog
MSc CS, PPL Prolog assignment 2015

This is a maze solving programming that currently works on mazes of dimension 5 rows x 9 columns. The maze has barriers (you could call them walls, too) which are identified by cartesian coordinates. The barriers are listed in the file maze.pl. To run the maze solver:

1. Open you Prolog terminal from the folder in which the source files are located;
2. Compile the files with the command: ```consult([maze, 'maze-solver']).```;
3. Start the path search with the command: ```solve([<startRow>, <startCol>], [<endRow>, <endCol>], Path).```;
4. To find an alternative path, enter a semicolon ```;``` or to stop searching enter a full stop ```.```.

Happy path-finding! 
