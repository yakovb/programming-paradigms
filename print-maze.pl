use_module(maze).

inRange(X,Y) :-
	mazeSize(Xlim, Ylim) ,
	X >= 0 ,
	X =< (Xlim + 1) ,
	Y >= 0 ,
	Y =< (Ylim + 1).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewRow is Row + 1 ,
	NewCol is Column + 1 ,
	buildLine(NewRow, NewCol, [+|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	NewRow is Row + 1 ,
	NewCol is Column + 1 ,
	buildLine(NewRow, NewCol, [-|Acc], Line).

buildLine(Row, Column, Acc, Line) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewRow is Row + 1 ,
	NewCol is Column + 1 ,
	buildLine(NewRow, NewCol, ["|"|Acc], Line).


printLine(Row, Column) :-
	buildLine(Row, Column, [], Line) ,
	format(Line).
