# ruby
MSc CS, PPL Erlang assignment 2015

This is a simple library management programme. It is meant to help a librarian manage library members and the books lent to them. Most methods in the library.rb file are public so this source file functions as the API (the method names are self explanatory). The key is that before any action can be taken, the library must be open and the customer must be a member of the library. 

The programme isn't run from the command line - you'd have to use it from within your own Ruby session. However, if you want to give it a test drive with the test suite I've written for it you can do this by opening a Ruby terminal from the folder where the source files are located. Then type ```ruby library_test.rb```. This should run 75 tests, all passing (they do on my system!). 
