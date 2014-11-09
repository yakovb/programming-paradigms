mazeSize(3,3).

barrier(2,1).

valid(X, Y, Acc) :-
	\+(member([X,Y], Acc)) ,
	\+(barrier(X,Y)) ,
	mazeSize(Xlim, Ylim) ,
	X =< Xlim ,
	Y =< Ylim .

finished(Step, Step).


step([Xold,Y], [Xnew,Y], Acc) :-
	Xnew is Xold + 1 ,
	valid(Xnew,Y, Acc).

step([X,Yold], [X,Ynew], Acc) :-
	Ynew is Yold + 1 ,
	valid(X,Ynew, Acc).

step([Xold,Y], [Xnew,Y], Acc) :-
	Xnew is Xold - 1 ,
	valid(Xnew,Y, Acc).

step([X,Yold], [X,Ynew], Acc) :-
	Ynew is Yold - 1 ,
	valid(X,Ynew, Acc).


move(StepA, _, To, Acc, Acc) :-
	finished(StepA, To).

move(StepA, StepB, To, Acc, Path) :-
	step(StepA, StepB, Acc) ,
	move(StepB, _, To, [StepB|Acc], Path).

solve(From, To, Path) :-
	move(From, _, To, From, Path).
