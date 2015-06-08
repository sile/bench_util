%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 統計ログ出力サーバ
%% @private
-module(bench_util_sar_log_server).

-behaviour(gen_server).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

-export_type([start_arg/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          logger    :: logi:logger(),
          interval  :: pos_integer(),  % seconds
          prev_stat :: [{atom(), term()}]
        }).

-type start_arg() :: {logi:logger(), IntervalSeconds::pos_integer()}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc サーバを起動する
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).


%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init({Logger, IntervalSeconds}) ->
    State =
        #state{
           logger    = Logger,
           interval  = IntervalSeconds,
           prev_stat = bench_util:stat()
          },
    ok = schedule_output(State),
    {ok, State}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info(log_output, State0) ->
    State1 = do_output(State0),
    ok = schedule_output(State1),
    {noreply, State1};
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec schedule_output(#state{}) -> ok.
schedule_output(#state{interval = Seconds}) ->
    _ = erlang:send_after(Seconds * 1000, self(), log_output),
    ok.

-spec do_output(#state{}) -> #state{}.
do_output(State = #state{prev_stat = Prev}) ->
    Curr = bench_util:stat(),

    {PrevActiveTime, PrevTotalTime} =
        lists:foldl(fun ({_, Active, Total}, {AccActive, AccTotal}) -> {AccActive + Active, AccTotal + Total} end,
                    {0, 0},
                    val(scheduler_wall_time, Prev)),
    {CurrActiveTime, CurrTotalTime} =
        lists:foldl(fun ({_, Active, Total}, {AccActive, AccTotal}) -> {AccActive + Active, AccTotal + Total} end,
                    {0, 0},
                    val(scheduler_wall_time, Curr)),
    SchedulerUtilization = (CurrActiveTime - PrevActiveTime) / (CurrTotalTime - PrevTotalTime) * 100,

    CpuUtilization = lists:sum([Usage || {_, Usage, _, _} <- val(cpu_utilization, Curr)]) / length(val(cpu_utilization, Curr)),

    Headers =
        [
         {reductions, delta(reduction_count, Curr, Prev)},
         {context_switches, delta(context_switch_count, Curr, Prev)},
         {gc_count, delta(gc_count, Curr, Prev)},
         {gc_bytes, delta(gc_bytes, Curr, Prev)},
         {output_bytes, delta(output_bytes, Curr, Prev)},
         {input_bytes, delta(input_bytes, Curr, Prev)},
         {run_queue_length, val(run_queue_length, Curr)},
         {cpu_util, CpuUtilization},
         {scheduler_util, SchedulerUtilization},
         {memory_total, val(memory_total, Curr)},
         {memory_processes, val(memory_processes, Curr)},
         {memory_system, val(memory_system, Curr)},
         {memory_atom, val(memory_atom, Curr)},
         {memory_binary, val(memory_binary, Curr)},
         {memory_ets, val(memory_ets, Curr)},
         {port_count, val(port_count, Curr)},
         {process_count, val(process_count, Curr)}
        ],

    _ = logi:info_opt(State#state.logger, "", [], [{headers, Headers}]),
    State#state{prev_stat = Curr}.

-spec delta(atom(), [{atom(), integer()}], [{atom(), integer()}]) -> integer().
delta(Key, New, Old) ->
    val(Key, New) - val(Key, Old).

-spec val(atom(), [{atom(), term()}]) -> term().
val(Key, Assoc) ->
    proplists:get_value(Key, Assoc).
