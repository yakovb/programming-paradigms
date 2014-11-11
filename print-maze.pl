use_module(maze).

inRange([X,Y]) :-
	mazeSize(Xlim, Ylim) ,
	X >= 0 ,
	X =< (Xlim + 1) ,
	Y >= 0 ,
	Y =< (Ylim + 1).

printLine(Row, Line) :-
	inRange(Row) ,
	format(Line).
