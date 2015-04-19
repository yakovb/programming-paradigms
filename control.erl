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
	Me = spawn(fun control:loop/0),
	Me ! kick_off,
	io:format("~nTesting Celsius conversion and display...~n"),
	celsius_good(Me),
	celsius_too_cold(Me),
	celsius_bad_data(Me),
	io:format("~nTesting Fahrenheit conversion and display...~n"),
	fahrenheit_good(Me),
	fahrenheit_too_cold(Me),
	fahrenheit_bad_data(Me),
	Me ! shutdown.

celsius_good(PID) ->
	io:format("The next 5 Celsius temps should all convert correctly: ~n"),
	rollingSend({PID, convert_C,  [0, -100, 896, 46, 99751]}).

celsius_too_cold(PID) ->
	io:format("The next 2 Celsius temps should all fail because they're below absolute zero: ~n"),
	rollingSend({PID, convert_C,  [-1000, -273.16]}).

celsius_bad_data(PID) ->
	io:format("The next 3 Celsius conversions should fail as they're not numeric data: ~n"),
	rollingSend({PID, convert_C,  [test, "string", [atom, list]]}).

fahrenheit_good(PID) ->
	io:format("The next 5 Fahrenheit temps should all convert correctly: ~n"),
	rollingSend({PID, convert_F,  [0, -100, 896, 46, 99751]}).

fahrenheit_too_cold(PID) ->
	io:format("The next 2 Fahrenheit temps should all fail because they're below absolute zero: ~n"),
	rollingSend({PID, convert_F,  [-1000, -523.68]}).

fahrenheit_bad_data(PID) ->
	io:format("The next 3 Fahrenheit conversions should fail as they're not numeric data: ~n"),
	rollingSend({PID, convert_F,  [test, "string", [atom, list]]}).

rollingSend({PID, Conv, Nums}) -> lists:foreach(fun(X) -> PID ! {Conv, X} end, Nums).