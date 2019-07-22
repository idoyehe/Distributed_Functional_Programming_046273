-module(exam).
-author("IdoYe").
-behaviour(gen_server).

%% API
-export([start2018A/0, loop2018A/1, calcFuns2018/1, derive/2, discretDerv/2, genSeries/3, genSeries/4, isSeries/2, isAlgebric/1, integral/3, next/1, find/1, isPrime/1, solver/2, add/1, sortComplexList/1, calc/1, calcOnServer/1, startServer/0, init/1, handle_call/3, calcFun/2]).

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
wrapper([{add, List} | T]) -> [add(wrapper(List)) | wrapper(T)];
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
  case N rem M == 0 of
    true -> false;
    false -> isPrime(N, M + 1)

  end.

next(Num) ->
  case isPrime(Num) of
    true -> [Num | fun() -> next(Num + 1) end];
    false -> next(Num + 1)
  end.

find(Y) -> find(Y, next(1)).
find(Y, [H | T]) ->
  case H > Y of
    true -> H;
    false -> find(Y, T())
  end.

integral(F, Start, End) ->
  L = lists:seq(Start, End),
  F1 = fun(PID, X) -> PID ! {X, F(X)} end,
  F2 = fun(_Key, [Val], AccIn) -> AccIn + Val end,
  mapreduce(F1, F2, 0, L).



mapreduce(F1, F2, Acc0, L) ->
  S = self(),
  Pid = spawn(fun() -> reduce(S, F1, F2, Acc0, L) end),
  receive
    {Pid, Result} -> Result
  end.
reduce(Parent, F1, F2, Acc0, L) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  lists:foreach(fun(X) ->
    spawn_link(fun() -> F1(ReducePid, X) end)
                end, L),
  N = length(L),
  Dict0 = dict:new(),
  Dict1 = collect_replies(N, Dict0),
  Acc = dict:fold(F2, Acc0, Dict1),
  Parent ! {self(), Acc}.

collect_replies(0, Dict) -> Dict;
collect_replies(N, Dict) ->
  receive
    {Key, Val} ->
      case dict:is_key(Key, Dict) of
        true -> Dict1 = dict:append(Key, Val, Dict),
          collect_replies(N, Dict1);
        false -> Dict1 = dict:store(Key, [Val], Dict),
          collect_replies(N, Dict1)
      end;
    {'EXIT', _, _} -> collect_replies(N - 1, Dict)
  end.

isAlgebric(L) when length(L) == 1 -> true;
isAlgebric(L) when length(L) == 2 -> true;
isAlgebric([H | [S_H | S_T]]) -> isAlgebric([S_H | S_T], S_H - H).

isAlgebric([_H | []], _D) -> true;
isAlgebric([H | [S_H | S_T]], D) ->
  case S_H - H == D of
    true -> isAlgebric([S_H | S_T], D);
    false -> false
  end.

isSeries(_F, []) -> true;
isSeries(_F, L) when length(L) == 1 -> true;
isSeries(F, [H | [S_H | S_T]]) ->
  case F(H) == S_H of
    true -> isSeries(F, [S_H | S_T]);
    false -> false
  end.

genSeries(algebraic, A0, D, N) ->
  F = fun(X) -> X + D end,
  genSeries(special, A0, F, N, []);

genSeries(geomtric, A0, Q, N) ->
  F = fun(X) -> X * Q end,
  genSeries(special, A0, F, N, []);

genSeries(special, A0, F, N) ->
  genSeries(special, A0, F, N, []).


genSeries(special, _A0, _F, 0, L) -> L;
genSeries(special, A0, F, N, L) ->
  R = F(A0),
  genSeries(special, R, F, N - 1, L ++ [A0]).


genSeries(algebraic, A0, D) ->
  F = fun(X) -> X + D end,
  genSeries(special, A0, F);

genSeries(geomtric, A0, Q) ->
  F = fun(X) -> X * Q end,
  genSeries(special, A0, F);

genSeries(special, A0, F) ->
  [A0 | fun() -> genSeries(special, F(A0), F) end].


discretDerv(F, [H | T]) ->
  discretDerv(F, [H | T], []).

discretDerv(_F, [], L) -> L;
discretDerv(F, [H | T], L) ->
  discretDerv(F, T, L ++ [(F(H + 1) - F(H - 1)) / 2]).



derive(F, List) ->
  IndexList = lists:zip(lists:seq(1, length(List)), List),
  F1 = fun(PID, {I, X}) -> PID ! {I, (F(X + 1) - F(X - 1)) / 2} end,
  F2 = fun(Key, [Val], ACCIN) -> ACCIN ++ [{Key, Val}] end,
  RES = mapreduce(F1, F2, [], IndexList),
  lists:map(fun(TUP) -> element(2, TUP) end, lists:sort(fun(A, B) -> element(1, A) =< element(1, B) end, RES)).

calcFuns2018([]) -> [];
calcFuns2018([H | T]) ->
  calcFuns2018([H | T], []).

calcFuns2018([], L) -> L;
calcFuns2018([H | T], L) ->
  calcFuns2018(T, L ++ [H()]).


start2018A() -> register(my_server, spawn(?MODULE, loop2018A, [42])).

loop2018A(X) ->
  receive
    {increaseCounter} -> loop2018A(X + 1);
    {addToCounter, Y} -> loop2018A(X + Y);
    {getCounter, From, MsgRef} ->
      From ! {MsgRef, X},
      loop2018A(X);
    {stop} -> io:format("Server stopped~n"), ok
  after 1000 ->
    loop2018A(X - 1)
  end.
  