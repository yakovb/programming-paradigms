module(print-maze, [printMaze/1]).
use_module(maze).


buildLine(Row, _, _, _) :-
	mazeSize(_, Ylim) ,
	Row > Ylim + 1.

buildLine(Row, Column, reverse(Line), Path) :-
	mazeSize(Xlim, _) ,
	Column > Xlim + 1 ,
	format(Line) ,
	NewRow is Row + 1 ,
	buildLine(NewRow, 0, [], Path).


buildLine(Row, Column, Line, Path) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [+|Line], Path).

buildLine(Row, Column, Line, Path) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [-|Line], Path).

buildLine(Row, Column, Line, Path) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ["|"|Line], Path).

buildLine(Row, Column, Line, Path) :-
	barrier(Row, Column) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ["X"|Line], Path).

buildLine(Row, Column, Line, Path) :-
	member([Row, Column], Path) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [*|Line], Path).

buildLine(Row, Column, Line, Path) :-
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [.|Line], Path).


printMaze(Path) :-
	buildLine(0, 0, [], Path).
