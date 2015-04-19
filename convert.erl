-module(convert).
-export([loop/0]).

%TODO: convert from F and C as per formula
%TODO: error check for temps below absolute zero (is there a max temp??)
%TODO: send results to display

loop() ->
	receive
		{"ConvertToCelsius", Ftemp} ->
			io:format("~.2f F is equivalent to ~.2f C~n", [float(Ftemp), f2c(Ftemp)]), %TODO debug text, remove
			loop();

		test -> 
			io:format("test message received by converter"),
			loop()
			
		end.

f2c(Ftemp) -> (Ftemp - 32) * (5/9).
c2f(Ctemp) -> Ctemp * (9/5) + 32.