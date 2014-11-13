module(print-maze, [printMaze/1]).
use_module(maze).

%stop if row outside boundary
buildLine(Row, _, _, _) :-
	mazeSize(Height, _) ,
	Row > Height + 1 .

%move to next row since column is outside boundary
%print the line built so far
buildLine(Row, Column, Line, Path) :-
	mazeSize(_, Length) ,
	Column > Length + 1 ,
	reverse(Line, RLine) ,
	format(RLine) ,
	format('~n') ,
	NewRow is Row + 1 ,
	buildLine(NewRow, 0, [], Path).

%coordinate is on an un-numbered position on an axis
buildLine(Row, Column, Line, Path) :-
	Row =< 0 ,
	Column =< 0 ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, [' '|Line], Path).

%coordinate is on the "Length" axis
buildLine(Row, Column, Line, Path) :-
	mazeSize(_, Length) ,
	Row =:= -1 ,
	(Column > 0 , Column =< Length) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['0'|Line], Path).

%coordinate is on the "Height" axis
buildLine(Row, Column, Line, Path) :-
	mazeSize(Height, _) ,
	Column =:= -1 ,
	(Row > 0 , Row =< Height) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['0'|Line], Path).

%coordinate is a corner
buildLine(Row, Column, Line, Path) :-
	mazeSize(Height, Length) ,
	(Row =:= 0  ;  Row =:= (Height + 1)) ,
	(Column =:= 0  ;  Column =:= (Length + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['+'|Line], Path).

%coordinate is a top/bottom border
buildLine(Row, Column, Line, Path) :-
	mazeSize(Height, _) ,
	(Row =:= 0  ;  Row =:= (Height + 1)) ,
	NewCol is Column + 1 ,
	buildLine(Row, NewCol, ['-'|Line], Path).

%coordinate is a side border
buildLine(Row, Column, Line, Path) :-
	mazeSize(Height, Length) ,
	(Row =\= 0  ;  Row =\= (Height + 1)) ,
	(Column =:= 0  ;  Column =:= (Length + 1)) ,
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
	buildLine(-1, -1, [], Path).
