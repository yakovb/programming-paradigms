-module(convert).
-export([loop/0]).

loop() ->
	receive
		test -> 
			io:format("test message received by converter"),
			loop()
			
		end.