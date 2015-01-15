use_module([maze]).

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


printTop :-
	write('    1 2 3 4 5 6 7 8 9  ') , nl ,
	write('  +-------------------+') , nl .
printBottom :-
	write('  +-------------------+').

printLeftMargin(Path, [Row,Col]) :-
	write(Row) ,
	write(' | ') ,
	printElem(Path, [Row,Col]).

printElem(Path, [Row, Col]) :-
	Col =< 9 , Row =< 5 ,
	member([Row, Col], Path) ,
	write('* ') ,
	NewCol is Col + 1 ,
	! , printElem(Path, [Row, NewCol]) .
printElem(Path, [Row, Col]) :-
	Col =< 9 , Row =< 5 ,
	barrier(Row, Col) ,
	write('x ') ,
	NewCol is Col + 1 ,
	! , printElem(Path, [Row, NewCol]).
printElem(Path, [Row, Col]) :-
	Col =< 9, Row =< 5 ,
	write('. ') ,
	NewCol is Col + 1 ,
	! , printElem(Path, [Row, NewCol]).
printElem(Path, [Row, Col]) :-
	Col > 9 , Row < 5 ,
	NewRow is Row + 1 ,
	write('|') , nl ,
	! , printLeftMargin(Path, [NewRow, 1]).
printElem(_, [Row,Col]) :-
	Col > 9, Row =:= 5 ,
	write('|') , nl ,
	printBottom.

printMaze(Path) :-
	printTop ,
	printLeftMargin(Path, [1, 1]).
