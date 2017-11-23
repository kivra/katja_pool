%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc katja_pool application
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(katja_pool_app).
-behaviour(application).

%%%_* Exports ==========================================================
-export([start/2, stop/1]).

%%%_* API ==============================================================
start(_StartType, _StartArgs) ->
  katja_pool_sup:start_link().

stop(_State) ->
  ok.

%%%_* Editor ===========================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
