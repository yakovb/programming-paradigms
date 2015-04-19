-module(control).
-export([loop/0]).

%TODO: spawn/kill/restart converters and displays
%TODO: create temp list for testing including temps in/out of range for F and C
%TODO: create multiple converters and displays if possible
%TODO: have a system shutdown option
%TODO note if converter dies and restart it

loop() ->
	receive
		test -> 
			io:format("test message received by controller"),
			loop()

		end.