-module(display).
-export([loop/0]).

%TODO: display original temp and converted temp
%TODO: be killable by controller

loop() ->
	receive
		{celsiusOK, Ctemp, Ftemp} ->
			io:format("~.2f C is equivalent to ~.2f F~n", [float(Ctemp), float(Ftemp)]),

		{fahrenheitOK, Ftemp, Ctemp} ->
			io:format("~.2f F is equivalent to ~.2f C~n", [float(Ftemp), float(Ctemp)]),

		test -> 
			io:format("test message received by displayer"),
			loop()
			
		end.