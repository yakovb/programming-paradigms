use_module(maze).

inRange(X,Y) :-
	mazeSize(Xlim, Ylim) ,
	X >= 0 ,
	X =< (Xlim + 1) ,
	Y >= 0 ,
	Y =< (Ylim + 1).

buildLine(Row, Column, [+|_], _) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)).

buildLine(Row, _, [-|_], _) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)).

buildLine(Row, Column, ["|"|_], _) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)).


printLine(Row, Column) :-
	buildLine(Row, Column, [], Line) ,
	format(Line).
