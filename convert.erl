-module(convert).
-export([loop/0]).

loop() ->
	receive
		{PID, "ConvertToCelsius", Ftemp} ->
			PID ! f2c(Ftemp),
			loop();

		{PID, "ConvertToFahrenheit", Ctemp} ->
			PID ! c2f(Ctemp),
			loop();

		_ -> 
			io:format("Convert: got bad data to process: "),
			loop()
			
	end.


c2f(Ctemp) -> 	if 
					Ctemp < -273.15 -> {zero_error_C, Ctemp};
					true -> {celsiusOK, Ctemp, Ctemp * (9/5) + 32}
				end.


f2c(Ftemp) -> 	if 
					Ftemp < -523.67 -> {zero_error_F, Ftemp};
					true -> {fahrenheitOK, Ftemp, (Ftemp - 32) * (5/9)}
				end.