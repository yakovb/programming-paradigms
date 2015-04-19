-module(display).
-export([loop/0]).

%TODO: be killable by controller

loop() ->
	receive
		{celsiusOK, Ctemp, Ftemp} ->
			io:format("~.2f C is equivalent to ~.2f F~n", [float(Ctemp), float(Ftemp)]),
			loop();

		{fahrenheitOK, Ftemp, Ctemp} ->
			io:format("~.2f F is equivalent to ~.2f C~n", [float(Ftemp), float(Ctemp)]),
			loop();

		{zero_error_C, X} ->
			io:format("Oops! ~.2f C is below absolute zero of -273.15 C so conversion is impossible~n", [X]),
			loop();

		{zero_error_F, X} ->
			io:format("Oops! ~.2f F is below absolute zero of -523.67 F so conversion is impossible~n", [X]),
			loop()
			
		end.