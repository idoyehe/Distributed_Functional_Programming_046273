-module(mymath).
-author("Ido").
-export([power/2, nth/2, foo/1]).

power(_, 0) -> 1;
power(X, Y) -> X * power(X, Y - 1).

nth(_, []) -> false;
nth(1, [Head | _]) -> Head;
nth(Nth, [_ | Tail]) -> nth(Nth - 1, Tail).

foo(X) ->
  Mult = fun(Y) -> X * Y end,
  Bump = fun(X) -> X + 1 end,
  Mult(2) + Bump(10).