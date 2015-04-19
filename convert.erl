-module(convert).
-export([loop/0]).

%TODO: convert from F and C as per formula
%TODO: error check for temps below absolute zero (is there a max temp??)
%TODO: send results to display

loop() ->
	receive
		{"ConvertToCelsius", Ftemp} ->
			Ctemp = (Ftemp - 32) * (5/9),
			io:format("~.2f F is equivalent to ~.2f C~n", [float(Ftemp), Ctemp]), %TODO debug text, remove
			loop();

		test -> 
			io:format("test message received by converter"),
			loop()
			
		end.