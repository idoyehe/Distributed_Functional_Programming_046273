-module(myList).
-author("IdoYe").

%% API
-export([filter/2, bump/1]).

filter(_, []) -> [];
filter(F, [H | T]) ->
  case F(H) of
    true -> [H | filter(F, T)];
    false -> filter(F, T)
  end.

bump(List) -> bump (List, []).
bump ([], Acc) -> lists:reverse(Acc);
bump([H| T], Acc) -> bump(T, [H+1 | Acc]).