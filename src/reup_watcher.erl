-module(reup_watcher).
-behaviour(gen_server).

-define(POLL_INTERVAL, 2000).

-define(LOG(S,F), io:format("[reup] " ++ S ++ "\n", F)).

%% API.
-export([start_link/0, reup_module/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    port
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

env(K, Def) -> application:get_env(reup, K, Def).

%% interval for running find, to check for modified src files
poll_interval() -> env(poll_interval, 2000).

%% path to watch for src changes, relative to $CWD when erl started
src_dir()       ->
    case env(src_dir, undefined) of
        undefined -> default_src_dir();
        Dir -> Dir
    end.

default_src_dir() ->
    {ok, CWD} = file:get_cwd(),
    Guesses = [
        %% assuming rebar3 _build/<profile>/rel/<relname> structure
        CWD ++ "/../../../../apps",
        %% assuming script from project root, with apps
        "./apps",
        %% assuming script from project root, with single-otp project
        "./src",
        CWD
    ],
    first_valid_dir(Guesses).

first_valid_dir([]) -> undefined;
first_valid_dir([Dir|Rest]) ->
    case filelib:is_dir(Dir) of
        true -> Dir;
        false -> first_valid_dir(Rest)
    end.


%% gen_server.

init([]) ->
    SrcDir = src_dir(),
    ?LOG("watching for source changes in ~s",[SrcDir]),
    Exe = code:priv_dir(reup) ++ "/reup-watcher.sh",
    Port = open_port({spawn_executable, Exe}, [
        {args, [SrcDir]},
        {line, 2048},
        use_stdio
    ]),
    true = link(Port),
    self() ! pump,
    {ok, #state{port=Port}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(Info, State) ->
    ?LOG("unhandled cast: ~p",[Info]),
    {noreply, State}.

handle_info(pump, State = #state{port=Port}) ->
    erlang:port_command(Port, "pump\n"),
    {noreply, State};

handle_info({Port, {data, {eol, "ok"}}}, State = #state{port=Port}) ->
    erlang:send_after(poll_interval(), self(), pump),
    {noreply, State};

handle_info({Port, {data, {eol, Line}}}, State = #state{port=Port}) ->
    Mod = list_to_atom(filename:basename(Line, ".erl")),
    reup_module(Mod),
    {noreply, State};

handle_info({'EXIT', Port, Reason}, State = #state{port=Port}) ->
    ?LOG("port exit ~p", [Reason]),
    {stop, {port_exit, Reason}, State};

handle_info(Info, State) ->
    ?LOG("unhandled reup info: ~p",[Info]),
    {noreply, State}.

terminate(_Reason, #state{port=Port}) ->
    (catch port_command(Port, "exit\n")),
    (catch port_close(Port)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

mod_info(M) when is_atom(M) ->
    %% does this always work?
    %% can easily get this info from module_info() if debug_info used.
    case code:load_file(M) of
        {error, nofile} ->
            nofile;
        {error, not_purged} ->
            loaded_mod_info(M);
        {module, M} ->
            loaded_mod_info(M)
    end.

loaded_mod_info(M) when is_atom(M) ->
    case filename:find_src(M) of
        {error, {not_existing, _}} ->
            not_existing;
        {Src, Opts} ->
            {Src, Opts}
    end.

reup_module(M) when is_atom(M) ->
    case mod_info(M) of
        nofile ->
            ?LOG("~s - nofile",[M]),
            nofile;
        not_existing ->
            ?LOG("~s - not_existing",[M]),
            not_existing;
        {Src, Opts0} ->
            ?LOG("~s - compiling...",[M]),
            OutDir = filename:dirname(code:which(M)),
            Opts = [{outdir, OutDir} | Opts0],
            case compile:file(Src, Opts) of
                error ->
                    ?LOG("ERROR: ~s\n~p",[Src,Opts]),
                    error;
                {ok, M} ->
                    case application:get_env(reup, reload_on_compile, true) of
                        true ->
                            code:purge(M),
                            case code:load_file(M) of
                                {error, Err} ->
                                    ?LOG("reloaded ERROR -> ~p: ~s",[Err, M]),
                                    nofile;
                                {module, M} ->
                                    ?LOG("reloaded: ~s",[M]),
                                    maybe_run_tests(M)
                            end;
                        _ ->
                            ok
                    end
            end
    end.

maybe_run_tests(M) when is_atom(M) ->
    case erlang:function_exported(M, test, 0) of
        true ->
            ?LOG("~s:test() ...", [M]),
            try M:test() of
                ok ->
                    ?LOG("~s:test() ... PASSED", [M]),
                    reloaded_test_pass
            catch
                Reason ->
                    ?LOG("~s:test() ... FAILED ~p", [M, Reason]),
                    reloaded_test_fail
            end;
        false ->
            reloaded_no_test
    end.
