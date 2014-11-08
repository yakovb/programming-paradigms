mazeSize(2, 2).

move([X,Y], To, Path) :-
	NewX is X + 1 ,
	mazeSize(XBound, _) ,
	NewX =< XBound ,
	go([NewX,Y], To, [[NewX,Y]|Path]).

move([X,Y], To, Path) :-
	NewY is Y + 1 ,
	mazeSize(_, YBound) ,
	NewY =< YBound ,
	go([X,NewY], To, [[X,NewY]|Path]).


isFinished(To, To).


go(From, To, _) :-
	isFinished(From, To).

go(From, To, Path) :-
	move(From, To, Path).


solve(From, To, [From|Rest]) :- go(From, To, [From|[]]).
