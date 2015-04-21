# erlang
MSc CS, PPL Erlang assignment 2015

This is a simple temperature conversion programme, from Celsius to Fahrenheit and vice versa. To run it you must:

1. Open an Erlang terminal from the folder holding the source files;
2. Compile each file with the command ```c(<source name without extension>).```;
3. Run with the test data by typing: ```control:tester().```.

Alternatively you can spawn your own control actor and give it temperatures to convert:

1. Spawn a control: ```Ctrl = spawn(fun control:loop/0).```;
2. Activate the control: ```Ctrl ! kick_off.```;
3. Send it a message: ```Ctrl ! {convert_C, <Celsius temp>}.``` or ```Ctrl ! {convert_F, <Fahrenheit temp>}.```;
4. Shut it down with ```Ctrl ! shutdown.```.

Hours of fun can be had this way. 
