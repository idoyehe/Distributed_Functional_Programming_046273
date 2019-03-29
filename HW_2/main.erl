-module(main).
-author("Ido").
-export([print_helloWorld/0]).

print_helloWorld()->
  io:format('Hello World ~n').