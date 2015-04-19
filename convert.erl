-module(convert).
-export([loop/0]).

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



c2f(Ctemp) -> 	if 
					Ctemp < -273.15 -> {zero_error, c, Ctemp}
					true -> {celsiusOK, Ctemp, Ctemp * (9/5) + 32}
				end.


f2c(Ftemp) -> 	if 
					Ftemp < -523.67 -> {zero_error, f, Ftemp};
					true -> {fahrenheitOK, Ftemp, (Ftemp - 32) * (5/9)}
				end.