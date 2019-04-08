-module(game).
-author("idoYe & ohadZ").

%% API
-export([canWin/1, nextMove/1, explanation/0]).

canWin(N) when N > 0 -> canWinAux(N);
canWin(N) when N =< 0 -> erlang:error("number of matches is not valid").

canWinAux(1) -> true;
canWinAux(2) -> true;
canWinAux(N) when N =< 0 -> false;
canWinAux(N) -> not canWin(N - 1) or not canWin(N - 2).


nextMove(N) ->
  case canWin(N) of
    false -> false;
    true ->
      %% winning is guaranteed  but need to figure if to take 1 or 2 matches
      case canWinAux(N - 1) of
        false -> {true, 1}; %%opponent losses when taking 1 match so win with taking 1 matches move
        true -> {true, 2} %%opponent wins when taking 1 match so win with taking 2 matches move
      end
  end.

explanation() ->
  {"The difficulty is we have to check both possiable moves taking 1 match or taking 2 matchs therefore 2 recursive calls are needed for N-1 and N-2 and not just 1"}.
