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
      case canWinAux(N - 1) of
        false -> {true, 1};
        true -> {true, 2}
      end
  end.

explanation() -> {"the difficulty is that we have to check both steps N-1 and N-2 so we need 2 recursive calls and not just 1"}.