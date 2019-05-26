-module(loadBalance).
-author("Ido Yehezkel & Ohad Zohar").
%% API
-export([startServers/0, stopServers/0, numberOfRunningFunctions/1, calcFun/3]).


startServers() ->
  supervisor_instance:start_link(servers_supervisor).


stopServers() ->
  supervisor:terminate_child(servers_supervisor, server1),
  supervisor:terminate_child(servers_supervisor, server2),
  supervisor:terminate_child(servers_supervisor, server3),
  supervisor:delete_child(servers_supervisor, server1),
  supervisor:delete_child(servers_supervisor, server2),
  supervisor:delete_child(servers_supervisor, server3),
  SupervisorPid = whereis(servers_supervisor),
  exit(SupervisorPid, normal).

numberOfRunningFunctions(1) -> gen_server:call(server1, {get_active_jobs});
numberOfRunningFunctions(2) -> gen_server:call(server2, {get_active_jobs});
numberOfRunningFunctions(3) -> gen_server:call(server3, {get_active_jobs}).

calcFun(ClientPID, JobFunction, MsgRef) ->
  ServersLoadList = [{server1, numberOfRunningFunctions(1)}, {server2, numberOfRunningFunctions(2)}, {server3, numberOfRunningFunctions(3)}],
  SortedServersLoadList = lists:sort(fun(T1, T2) -> element(2, T1) =< element(2, T2) end, ServersLoadList),
  SelectedServerRef = element(1, lists:nth(1, SortedServersLoadList)),
  gen_server:cast(SelectedServerRef, {new_job, ClientPID, MsgRef, JobFunction}),
  ok.

