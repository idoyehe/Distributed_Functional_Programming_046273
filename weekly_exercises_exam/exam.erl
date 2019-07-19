-module(exam).
-author("IdoYe").
-behaviour(gen_server).

%% API
-export([next/1, find/1, isPrime/1, solver/2, add/1, sortComplexList/1, calc/1, calcOnServer/1, startServer/0, init/1, handle_call/3, calcFun/2]).

add([]) -> 0;
add([H | T]) -> addAux(H, T, {complex, 0, 0}).

addAux({complex, X, Y}, [], {complex, ACC_X, ACC_Y}) -> {complex, ACC_X + X, ACC_Y + Y};
addAux(X, [], {complex, ACC_X, ACC_Y}) -> {complex, ACC_X + X, ACC_Y};

addAux({complex, X, Y}, [H | T], {complex, ACC_X, ACC_Y}) -> addAux(H, T, {complex, ACC_X + X, ACC_Y + Y});
addAux(X, [H | T], {complex, ACC_X, ACC_Y}) -> addAux(H, T, {complex, ACC_X + X, ACC_Y}).

dist({complex, X, Y}) -> math:sqrt(X * X + Y * Y);
dist(X) -> X.

sortComplexList(List) when is_list(List) ->
  lists:sort(fun(A, B) -> dist(A) =< dist(B) end, List).

wrapper([]) -> [];
wrapper([{add, List} | T]) -> [add(List) | wrapper(T)];
wrapper([H | T]) -> [H | wrapper(T)].

calc({add, List}) -> add(wrapper(List)).


startServer() ->
  gen_server:start_link({local, ido}, ?MODULE, [], []).

calcOnServer(F) ->
  gen_server:call(ido, {calc, F}).
init([]) -> {ok, []}.


handle_call({calc, F}, From, State) ->
  spawn(?MODULE, calcFun, [From, F]),
  {noreply, State}.

calcFun(From, F) -> gen_server:reply(From, F()).


solver(_F, []) -> none;
solver(F, [H | T]) ->
  case F(H) of
    0 -> H;
    _ -> solver(F, T)
  end.

isPrime(N) when N < 2 -> false;
isPrime(N) -> isPrime(N, 2).
isPrime(N, N) -> true;
isPrime(N, M) ->
  ChPrime = N rem M,
  if
    ChPrime == 0 -> false;
    true -> isPrime(N, M + 1)
  end.

next(Num) ->
  case isPrime(Num) of
    true -> [Num | fun() -> next(Num + 1) end];
    false -> next(Num+1)
  end.

find(Y) -> find(Y, next(1)).
find(Y, [H | T]) ->
  case H > Y of
    true -> H;
    false -> find(Y, T())
  end.