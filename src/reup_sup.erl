-module(reup_sup).
-behaviour(supervisor).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [?CHILD(reup_watcher, worker, [])],
    {ok, {{one_for_one, 100, 1}, Procs}}.
