mazeSize(2, 2).

barrier(1,2).

valid(X, Y) :-
	\+(barrier(X,Y)) ,
	mazeSize(Xlim, Ylim) ,
	X =< Xlim ,
	Y =< Ylim .

finished(Step, Step).

step([Xold,Y], [Xnew,Y]) :-
	Xnew is Xold + 1 ,
	valid(Xnew,Y).

step([X,Yold], [X,Ynew]) :-
	Ynew is Yold + 1 ,
	valid(X,Ynew).

move(StepA, _, To, Acc, Acc) :-
	finished(StepA, To).

move(StepA, StepB, To, Acc, Path) :-
	step(StepA, StepB) ,
	move(StepB, NextStep, To, [Acc,StepB], Path).

solve(From, To, Path) :-
	move(From, Next, To, From, Path).
