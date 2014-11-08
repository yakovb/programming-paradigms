mazeSize(2, 2).

move([OldX, Y] , [NewX, Y], [[NewX, Y]|_]) :-
	NewX is OldX + 1 ,
	mazeSize(XBound, _) ,
	NewX =< XBound.

move([X, OldY] , [X, NewY], [[X, NewY]|_]) :-
	NewY is OldY + 1 ,
	mazeSize(_, YBound) ,
	NewY =< YBound.


isFinished(To, To, _).


solve(From, To, Path) :-
	move(From, Step, Path) ,
	move(Step, To, Path) ,
	isFinished(Step, To, Path).
