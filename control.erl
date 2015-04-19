-module(control).
-export([loop/0, start/0]).

%TODO: create temp list for testing including temps in/out of range for F and C

loop() ->
	receive
		kick_off ->
			io:format("Starting a new converter actor and display actor.~n"),
			register(converter, spawn_link(fun convert:loop/0)),
			register(display, spawn_link(fun display:loop/0)),
			loop();


		{convert_C, CTemp} ->
			converter ! {display, "ConvertToFahrenheit", CTemp},
			loop();


		shutdown ->
			io:format("Shutting down..."),
			exit(shutdown)

	end.


start() ->
	self() ! kick_off,
	CListGood = [23, -100, 896, 46, 99751],
	[self() ! {convert_C, C} || C <- CListGood].