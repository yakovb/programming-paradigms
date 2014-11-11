use_module(maze).



buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [+|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [-|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ["|"|Acc], Line).


printMaze(Path) :-
	buildLine(0, 0, [], Line) ,
	format(Line).
