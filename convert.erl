-module(convert).
-export([loop/0]).

%TODO: convert from F and C as per formula
%TODO: error check for temps below absolute zero (is there a max temp??)
%TODO: send results to display

loop() ->
	receive
		test -> 
			io:format("test message received by converter"),
			loop()
			
		end.