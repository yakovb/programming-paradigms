mazeSize(3,3).

barrier(2,2).

valid(X, Y, Acc) :-
	mazeSize(Xlim, Ylim) ,
	X > 0 ,
	X =< Xlim ,
	Y > 0 ,
	Y =< Ylim ,
	\+(member([X,Y], Acc)) ,
	\+(barrier(X,Y)).

finished(Step, Step).


step([Xold,Y], [Xnew,Y], Acc) :-
	Xnew is Xold + 1 ,
	valid(Xnew,Y, Acc).

step([X,Yold], [X,Ynew], Acc) :-
	Ynew is Yold + 1 ,
	valid(X,Ynew, Acc).

step([X,Yold], [X,Ynew], Acc) :-
	Ynew is Yold - 1 ,
	valid(X,Ynew, Acc).

step([Xold,Y], [Xnew,Y], Acc) :-
	Xnew is Xold - 1 ,
	valid(Xnew,Y, Acc).


move(StepA, _, To, Acc, Acc) :-
	finished(StepA, To).

move(StepA, StepB, To, Acc, Path) :-
	step(StepA, StepB, Acc) ,
	move(StepB, _, To, [StepB|Acc], Path).

solve(From, To, Path) :-
	move(From, _, To, [From], Result) ,
	reverse(Result, L2) ,
	Path = [From|L2].
