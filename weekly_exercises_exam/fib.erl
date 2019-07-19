-module(fib).
-author("Ido").
-export([fib/1]).

fib(N) -> fib_iter(N, 0, 1).
fib_iter(0, Result, _) -> Result;
fib_iter(N, Result, Next) when N > 0 ->
  fib_iter(N - 1, Next, Result + Next).