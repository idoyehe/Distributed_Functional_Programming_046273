-module(test_LB).

-export([main/0]).

start() ->
  loadBalance:startServers().

stop() ->
  loadBalance:stopServers().

status(StarTime) ->
  CurrTime = getTime(StarTime),
  io:format("~p:Server 1 is handlling ~p tasks ~n", [CurrTime, loadBalance:numberOfRunningFunctions(1)]),
  io:format("~p:Server 2 is handlling ~p tasks ~n", [CurrTime, loadBalance:numberOfRunningFunctions(2)]),
  io:format("~p:Server 3 is handlling ~p tasks ~n", [CurrTime, loadBalance:numberOfRunningFunctions(3)]),
  getMessages(CurrTime).

whereare() ->
  io:format("sup pid is: ~p t~n", [whereis(servers_supervisor)]),
  io:format("Server 1 pid is: ~p t~n", [whereis(server1)]),
  io:format("Server 2 pid is: ~p t~n", [whereis(server2)]),
  io:format("Server 3 pid is: ~p t~n", [whereis(server3)]).

battle() ->
  compile:file(loadBalance), compile:file(servers),
  F3 = fun() -> timer:sleep(3000), 3 * 3 end,
  F5 = fun() -> timer:sleep(10000), 4 * 4 end,
  StarTime = calendar:local_time(),

  %%Divide 10 functions:
  whereare(),
  myLoop(F3, 9),
  status(StarTime),
  myLoop(F5, 900),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  myLoop(F3, 20),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  timer:sleep(1000),
  status(StarTime),
  ok.

myLoop(_F, 0) ->
  ok;

myLoop(F, Times) ->
  ok = loadBalance:calcFun(self(), F, make_ref()),
  myLoop(F, Times - 1).


getMessages(CurrTime) ->
  receive
    {MsgRef, Result} -> io:format("~p: got message: {~p,~p}~n", [CurrTime, MsgRef, Result]),
      getMessages(CurrTime)
  after 100 -> ok
  end.


getTime(StarTime) ->
  {_, {_, _, TimeDiff}} = calendar:time_difference(StarTime, calendar:local_time()),
  TimeDiff.

main() ->
  start(),
  battle(),
  stop(),
  ok_main.


