-module(server_supervisor).
-author("Ido Yehezkel & Ohad Zohar").

%% API
-export([restarter/0]).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(matrix_server, matrix_server, []),
  register(matrix_server, Pid),
  receive
    {'EXIT', Pid, shutdown} -> ok; % no crash, shutdown request
    {'EXIT', Pid, _} -> restarter() % restart, recover from crash
  end.
