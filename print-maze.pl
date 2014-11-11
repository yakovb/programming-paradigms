module(print-maze, [printMaze/1]).
use_module(maze).

%stop if row outside boundary
buildLine(Row, _, _, _) :-
	mazeSize(_, Ylim) ,
	Row > Ylim + 1 .

%move to next row since column is outside boundary
%print the line built so far
buildLine(Row, Column, Line, Path) :-
	mazeSize(Xlim, _) ,
	Column > Xlim + 1 ,
	reverse(Line, RLine) ,
	format(RLine) ,
	format('~n') ,
	NewRow is Row + 1 ,
	buildLine(NewRow, 0, [], Path).

%coordinate is a corner
buildLine(Row, Column, Line, Path) :-
	mazeSize(Xlim, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['+'|Line], Path).

%coordinate is a top/bottom border
buildLine(Row, Column, Line, Path) :-
	mazeSize(_, Ylim) ,
	(Row =:= 0  ;  Row =:= (Ylim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['-'|Line], Path).

%coordinate is a side border
buildLine(Row, Column, Line, Path) :-
	mazeSize(Xlim, Ylim) ,
	(Row =\= 0  ;  Row =\= (Ylim + 1)) ,
	(Column =:= 0  ;  Column =:= (Xlim + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['|'|Line], Path).

%coordinate is a barrier
buildLine(Row, Column, Line, Path) :-
	barrier(Row, Column) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['x'|Line], Path).

%coordinate is in the path
buildLine(Row, Column, Line, Path) :-
	member([Row, Column], Path) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['*'|Line], Path).

%coordinate is not in the path
buildLine(Row, Column, Line, Path) :-
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['.'|Line], Path).


printMaze(Path) :-
	buildLine(0, 0, [], Path).
