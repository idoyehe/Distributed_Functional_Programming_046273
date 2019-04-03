-module(guardedFunction).
-author("IdoYe").

%% API
-export([do_what/3]).
do_what(Time, Weather, Duties) when Time=:= day, Weather=:= sunny, Duties=:= none -> go_to_beach;
do_what(Time, _Weather, Duties) when Time =:= night, ((Duties=:= none) or(Duties=:= light)) -> sleep;
do_what(_Time, _Weather,Duties)when Duties=:= homework; Duties=:= study ->go_study;
do_what(_Time, _Weather, _Duties) -> have_fun.