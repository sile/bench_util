%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor Module
%% @private
-module(bench_util_sar_log_server_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_child/2]).
-export([stop_child/1]).


%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Starts root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 子プロセスを起動する
-spec start_child(atom(), bench_util_sar_log_server:start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_child(ServerName, Arg) ->
    Server = bench_util_sar_log_server,
    ChildSpec = {ServerName, {Server, start_link, [Arg]}, permanent, 5000, worker, [Server]},
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc 子プロセスを停止する
-spec stop_child(atom()) -> ok.
stop_child(ServerName) ->
    _ = supervisor:terminate_child(?MODULE, ServerName),
    _ = supervisor:delete_child(?MODULE, ServerName),
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
