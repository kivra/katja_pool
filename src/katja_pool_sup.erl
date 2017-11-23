%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc katja_pool top level supervisor.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(katja_pool_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([start_link/0]).

%%%_ * Supervisor callbacks --------------------------------------------
-export([init/1]).

%%%_* Defines ==========================================================
-define(SERVER, ?MODULE).

%%%_* API ==============================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%_* Supervisor callbacks =============================================
init([]) ->
  {ok, Pools} = application:get_env(pools),
  PoolSpecs = lists:map(
                fun({Name, PoolArgs0, WorkerArgs}) ->
                    PoolArgs = [{name, {local, Name}}] ++ PoolArgs0,
                    poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                end, Pools),
  {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%%%_* Editor ===========================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
