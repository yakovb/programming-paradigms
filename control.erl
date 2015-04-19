-module(control).
-export([loop/0]).

%TODO: kill converters and displays
%TODO: create temp list for testing including temps in/out of range for F and C
%TODO: have a system shutdown option

loop() ->
	receive
		kick_off ->
			io:format("Starting a new converter actor and display actor.~n"),
			register(converter, spawn_link(fun convert:loop/0)),
			register(display, spawn_link(fun display:loop/0)),
			loop();


		shutdown ->
			exit(display, kill),
			io:format("Killed the display actor.~n"),
			exit(converter, kill),
			io:format("Killed the converter actor.~n"),
			io:format("Killing myself...~n"),
			exit(kill)

	end.