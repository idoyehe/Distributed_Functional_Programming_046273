-module(mymath).
-author("Ido").
-export([power/2, nth/2]).

power(_,0) -> 1;
power(X,Y) -> X * power(X,Y-1).

nth(0, [Head|_])-> Head;
nth(Nth,[_|Tail])-> nth(Nth-1, Tail).