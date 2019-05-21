-module(server_supervisor).
-author("Ido Yehezkel & Ohad Zohar").

%% API
-export([restarter/1]).


restarter(Starter) ->
  process_flag(trap_exit, true),
  Server_Pid = spawn_link(matrix_server, matrix_server, []),
  register(matrix_server, Server_Pid),
  Starter ! server_up,
  receive
    {'EXIT', Server_Pid, shutdown} -> ok; % no crash, shutdown request
    {'EXIT', Server_Pid, _} -> restarter(Starter) % restart, recover from crash
  end.
