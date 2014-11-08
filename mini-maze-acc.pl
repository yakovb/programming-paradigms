mazeSize(7, 7).

barrier(2,1).

valid(X, Y, Path) :-
	\+(member([X,Y], Path)) ,
	\+(barrier(X,Y)) ,
	mazeSize(Xlim, Ylim) ,
	X =< Xlim ,
	Y =< Ylim .

finished(Step, Step).


step([Xold,Y], [Xnew,Y], Path) :-
	Xnew is Xold + 1 ,
	valid(Xnew,Y, Path).

step([X,Yold], [X,Ynew], Path) :-
	Ynew is Yold + 1 ,
	valid(X,Ynew, Path).

step([Xold,Y], [Xnew,Y], Path) :-
	Xnew is Xold - 1 ,
	valid(Xnew,Y, Path).

step([X,Yold], [X,Ynew], Path) :-
	Ynew is Yold - 1 ,
	valid(X,Ynew, Path).


move(StepA, _, To, Acc, Acc) :-
	finished(StepA, To).

move(StepA, StepB, To, Acc, Path) :-
	step(StepA, StepB, Path) ,
	move(StepB, NextStep, To, [Acc,StepB], Path).

solve(From, To, Path) :-
	move(From, Next, To, From, Path).
