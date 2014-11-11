use_module([maze, print-maze]).

valid(Height, Length, Acc) :-
	mazeSize(UpperHeight, UpperLength) ,
	Height > 0 ,
	Height =< UpperHeight ,
	Length > 0 ,
	Length =< UpperLength ,
	\+(member([Height,Length], Acc)) ,
	\+(barrier(Height,Length)).

finished(Step, Step).

step([Height,OldLength], [Height,NewLength], Acc) :-
	NewLength is OldLength + 1 ,
	valid(Height,NewLength, Acc).

step([OldHeight,Length], [NewHeight,Length], Acc) :-
	NewHeight is OldHeight + 1 ,
	valid(NewHeight,Length, Acc).

step([OldHeight,Length], [NewHeight,Length], Acc) :-
	NewHeight is OldHeight - 1 ,
	valid(NewHeight,Length, Acc).

step([Height,OldLength], [Height,NewLength], Acc) :-
	NewLength is OldLength - 1 ,
	valid(Height,NewLength, Acc).


move(StepA, _, To, Acc, Acc) :-
	finished(StepA, To).

move(StepA, StepB, To, Acc, Path) :-
	step(StepA, StepB, Acc) ,
	move(StepB, _, To, [StepB|Acc], Path).

solve(From, To, Path) :-
	move(From, _, To, [From], Result) ,
	reverse(Result, Path) ,
	printMaze(Path).

