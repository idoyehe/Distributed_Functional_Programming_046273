-module(matrix_server).
-author("Ido Yehezkel & Ohad Zohar").

%% API
-export([mult/2, get_version/0, shutdown/0, start_server/0, explanation/0, matrix_server/0]).

% starting the loop, generating the template and collect all results into matrix
matricesMultiplyWrapper(Matrix1, Matrix2, ClientPid, MsgRef) ->
  ClientPid ! {MsgRef, matrix_utils:matricesMultiply(Matrix1, Matrix2)}.

% the server infinite loop
matrix_server() ->
  receive
    {Pid, MsgRef, {multiple, Mat1, Mat2}} -> % multiple message processing
      spawn(fun() -> matricesMultiplyWrapper(Mat1, Mat2, Pid, MsgRef) end),
      matrix_server();
    {Pid, MsgRef, get_version} -> % get_version message processing
      Pid ! {MsgRef, version_1},
      matrix_server();
    sw_upgrade -> % sw_upgrade message processing
      ?MODULE:matrix_server();
    shutdown -> % do cleanup here
      cleanup(),
      exit(shutdown);
    _ -> % default message processing, prevent mailbox explosion
      matrix_server()
  end.

% cleanup function unregister
cleanup() -> % cleanup here procedure
  unregister(matrix_server).

% send shutdown request to server
shutdown() -> % sending shutdown to server
  matrix_server ! shutdown.

rpc(Request) ->
  MsgRef = make_ref(),
  matrix_server ! {self(), MsgRef, Request},
  receive
    {MsgRef, Response} -> Response
  end.

% send matrix multiply request and return the result.
mult(Matrix1, Matrix2) ->
  rpc({multiple, Matrix1, Matrix2}).

% get version request and return the result.
get_version() ->
  rpc(get_version).

start_server() ->
  spawn(server_supervisor, restarter, []).

explanation() ->
  io:format("The supervisor code and the server code should be in separated moduls because
  we would like to compile and link the upgraded server module as many times as we want without killing the server,
  becuase Erlang can hold only two versions of code for a single module, but the supervisor process is waiting
  in a receive it can prevent us to upgrade the server code more than one time if not seperated.
  When seperating, other alive modules (supervisor) don't distrub upgrading the server module many times.").