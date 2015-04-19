-module(convert).
-export([loop/0]).

%TODO: error check for temps below absolute zero (is there a max temp??)
%TODO: send results to display

loop() ->
	receive
		{"ConvertToCelsius", Ftemp} ->
			io:format("~.2f F is equivalent to ~.2f C~n", [float(Ftemp), f2c(Ftemp)]), %TODO debug text, remove
			loop();

		{"ConvertToFahrenheit", Ctemp} ->
			c2f(Ctemp),
			loop();

		test -> 
			io:format("test message received by converter"),
			loop()
			
		end.

c2f(Ctemp) -> if
				Ctemp =< 273.15 -> 
					{error, "Requested temp is below absolute zero"};
				true ->
					Ctemp * (9/5) + 32.

f2c(Ftemp) -> (Ftemp - 32) * (5/9).


% checks if the temperature is below absolute zero
zeroTest(T) -> case T of
	{c, C} -> 	if
					Ctemp =< 273.15 -> false;
					true -> true.
				end;

	{f, F} -> zeroTest(f2c(F))
end.