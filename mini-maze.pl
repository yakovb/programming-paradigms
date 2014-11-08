mazeSize(2, 2).

move([OldX, _] , [NewX, _]) :-  NewX is OldX + 1.
move([_, OldY] , [_, NewY]) :-  NewY is OldY + 1.

