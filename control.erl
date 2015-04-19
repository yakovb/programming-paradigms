-module(control).
-export([loop/0]).

loop() ->
	receive
		test -> 
			io:format("test message received by controller"),
			loop()
			
		end.