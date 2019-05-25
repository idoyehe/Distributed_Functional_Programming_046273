-module(server_instance).
-author("Ido Yehezkel & Ohad Zohar").

%% API
-behaviour(gen_server).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).


start_link(Server_Name) ->
  gen_server:start_link({local, Server_Name}, ?MODULE, [Server_Name], []).

init([Server_Name]) ->
  {ok, {Server_Name, 0}}. % state is the server name and job counter initialize to 0


handle_call({get_active_jobs}, _From, {Server_Name, Counter}) -> {reply, Counter, {Server_Name, Counter}}.


handle_cast({job_finished}, {Server_Name, Counter}) -> {noreply, {Server_Name, Counter - 1}};

handle_cast({new_job, From, MsgRef, Job_Function}, {Server_Name, Counter}) ->
  spawn(fun() -> execute_job(From, MsgRef, Job_Function, Server_Name) end),
  {noreply, {Server_Name, Counter + 1}}.

job_wrapper(JobFunction) ->
  F_result = JobFunction(),
  exit({result, F_result}).


execute_job(From, MsgRef, JobFunction, Server_Name) ->
  process_flag(trap_exit, true),
  JobPid = spawn_link(fun() -> job_wrapper(JobFunction) end),
  receive
    {'EXIT', JobPid, {result, F_result}} ->
      From ! {MsgRef, F_result},
      gen_server:cast(Server_Name, {job_finished});
    {'EXIT', JobPid, _} -> gen_server:cast(Server_Name, {job_finished})
  end.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.