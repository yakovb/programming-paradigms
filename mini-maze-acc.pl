mazeSize(2, 2).

valid(X, Y) :-
	mazeSize(Xlim, Ylim) ,
	X =< Xlim ,
	Y =< Ylim .

move([Xold,Y], [Xnew,Y]) :-
	Xnew is Xold + 1 ,
	valid(Xnew,Y).

move([X,Yold], [X,Ynew]) :-
	Ynew is Yold + 1 ,
	valid(X,Ynew).
