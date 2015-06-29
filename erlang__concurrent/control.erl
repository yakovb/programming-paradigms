-module(control).
-export([loop/0, tester/0]).

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


		{convert_F, FTemp} ->
			converter ! {display, "ConvertToCelsius", FTemp},
			loop();


		shutdown ->
			io:format("Shutting down..."),
			exit(shutdown)

	end.


tester() ->
	Control = spawn(fun control:loop/0),
	Control ! kick_off,
	timer:sleep(2000),
	io:format("~nTesting Celsius conversion and display...~n"),
	celsius_good(Control),
	celsius_too_cold(Control),
	celsius_bad_data(Control),
	io:format("~nTesting Fahrenheit conversion and display...~n"),
	fahrenheit_good(Control),
	fahrenheit_too_cold(Control),
	fahrenheit_bad_data(Control),
	Control ! shutdown.

celsius_good(PID) ->
	io:format("~nThe next 5 Celsius temps should all convert correctly: ~n"),
	rollingSend({PID, convert_C,  [0, -100, 896, 46, 99751]}).

celsius_too_cold(PID) ->
	io:format("~nThe next 2 Celsius temps should fail because they're below absolute zero: ~n"),
	rollingSend({PID, convert_C,  [-1000, -273.16]}).

celsius_bad_data(PID) ->
	io:format("~nThe next 3 Celsius conversions should fail as they're not numeric data: ~n"),
	rollingSend({PID, convert_C,  [testoid, "string", [atom, list]]}).

fahrenheit_good(PID) ->
	io:format("~nThe next 5 Fahrenheit temps should all convert correctly: ~n"),
	rollingSend({PID, convert_F,  [0, -100, 896, 46, 99751]}).

fahrenheit_too_cold(PID) ->
	io:format("~nThe next 2 Fahrenheit temps should fail because they're below absolute zero: ~n"),
	rollingSend({PID, convert_F,  [-1000, -523.68]}).

fahrenheit_bad_data(PID) ->
	io:format("~nThe next 3 Fahrenheit conversions should fail as they're not numeric data: ~n"),
	rollingSend({PID, convert_F,  [test, "string", [atom, list]]}).

rollingSend({PID, Conv, Nums}) -> 
	lists:foreach(fun(X) -> PID ! {Conv, X} end, Nums),
	timer:sleep(2000).