mazeSize(2, 2).

move([X,Y], To, _) :-
	NewX is X + 1 ,
	mazeSize(XBound, _) ,
	NewX =< XBound ,
	go([NewX,Y], To, [_|[NewX,Y]]).

move([X,Y], To, _) :-
	NewY is Y + 1 ,
	mazeSize(_, YBound) ,
	NewY =< YBound ,
	go([X,NewY], To, [_|[X,NewY]]).


isFinished(To, To).


go(From, To, _) :-
	isFinished(From, To).

go(From, To, Path) :-
	move(From, To, Path).


solve(From, To, [From|Path]) :- go(From, To, Path).
