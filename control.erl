-module(control).
-export([loop/0]).

%TODO: kill converters and displays
%TODO: create temp list for testing including temps in/out of range for F and C
%TODO: have a system shutdown option

loop() ->
	receive
		kick_off ->
			io:format("Starting a new converter actir and display actor.~n"),
			register(converter, spawn_link(fun convert:loop/0)),
			register(display, spawn_link(fun display:loop/0)),
			loop();


		end.