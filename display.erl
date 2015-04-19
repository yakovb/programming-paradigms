-module(display).
-export([loop/0]).

loop() ->
	receive
		test -> 
			io:format("test message received by displayer"),
			loop()
			
		end.