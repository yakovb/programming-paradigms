-module(display).
-export([loop/0]).

%TODO: display original temp and converted temp
%TODO: be killable by controller

loop() ->
	receive
		test -> 
			io:format("test message received by displayer"),
			loop()
			
		end.