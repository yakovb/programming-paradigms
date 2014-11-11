use_module(maze).




buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewRow is Row - 1 ,
	NewCol is Column - 1 ,
	buildLine(NewRow, NewCol, [+|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	NewRow is Row - 1 ,
	NewCol is Column - 1 ,
	buildLine(NewRow, NewCol, [-|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewRow is Row - 1 ,
	NewCol is Column - 1 ,
	buildLine(NewRow, NewCol, ["|"|Acc], Line).


printMaze(Path) :-
	mazeSize(Xlim, Ylim) ,
	RowCountDown is Xlim + 1 ,
	ColCountDown is Ylim + 1 ,
	buildLine(RowCountDown, ColCountDown, [], Line) ,
	format(Line).
