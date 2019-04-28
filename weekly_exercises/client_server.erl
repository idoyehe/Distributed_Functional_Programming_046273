-module(client_server).
-author("IdoYe").

%% API
-export([start/0, rpc/1, stop/0, loop/0]).

% start and register the area server
start() ->
  case whereis(server) of
    undefined ->
      Pid = spawn(fun client_server:loop/0),
      register(server, Pid),
      {ok, Pid};
    Pid when is_pid(Pid) ->
      {error, already_started}
  end.


rpc(Request) ->
  server ! {type, self(), Request},
  receive
    {server, Response} -> Response
  end.

stop() ->
  case whereis(server) of
    undefined ->
      {error, already_stoped};
    Pid when is_pid(Pid) ->
      Pid ! {stop, self()},
      receive
        {Pid, Response} -> Response
      end
  end.


loop() ->
  receive
    {type, From, Request} ->% message processing
      From ! {self(), process_request(Request, From)},
      loop();
    {stop, From} ->% do cleanup here
      cleanup(),
      From ! {self(), stoped},
      ok;
    Any -> % default message processing
      io:format("Got ~p~n", [Any]),
      loop()
  after 5000 -> % periodic events
    periodic_event(),
    loop()
  end.

process_request(_, _) -> ack.
periodic_event() -> periodic.
cleanup() ->
  case whereis(server) of
    undefined -> {error, already_cleaned};
    Pid when is_pid(Pid) ->
      unregister(server),
      {ok, cleaned}
  end.
