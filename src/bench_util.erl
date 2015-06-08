%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Benchmark Utility Functions
-module(bench_util).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([stat/0]).
-export([sar/1]).
-export([dotimes/2]).
-export([dotimes_tc/2]).
-export([start_sar_log_server/3]).
-export([stop_sar_log_server/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec stat() -> [Entry] when Entry :: {atom(), term()}.
stat() ->
    {{_, InputBytes}, {_, OutputBytes}} = statistics(io),
    {Number_of_GCs, Words_Reclaimed, _} = statistics(garbage_collection),
    SchedulerWallTime =
        case statistics(scheduler_wall_time) of
            undefined ->
                _ = erlang:system_flag(scheduler_wall_time, true),
                statistics(scheduler_wall_time);
            Time ->
                Time
        end,
    [{total, MemTotal}, {processes, MemProcesses}, {system, MemSystem},
     {atom, MemAtom}, {binary, MemBinary}, {code, MemCode}, {ets, MemEts}] =
        erlang:memory([total, processes, system, atom, binary, code, ets]),
    [
     {reduction_count, element(1, statistics(reductions))},
     {context_switch_count, element(1, statistics(context_switches))},
     {gc_count, Number_of_GCs},
     {gc_bytes, Words_Reclaimed * erlang:system_info(wordsize)},
     {input_bytes, InputBytes},
     {output_bytes, OutputBytes},
     {run_queue_length, statistics(run_queue)},
     {scheduler_wall_time, SchedulerWallTime},
     {cpu_utilization, cpu_sup:util([per_cpu])},
     {memory_total, MemTotal},
     {memory_processes, MemProcesses},
     {memory_system, MemSystem},
     {memory_atom, MemAtom},
     {memory_binary, MemBinary},
     {memory_code, MemCode},
     {memory_ets, MemEts},
     {port_count, erlang:system_info(port_count)},
     {process_count, erlang:system_info(process_count)}
    ].

-spec dotimes(non_neg_integer(), Fun) -> ok when
      Fun :: fun (() -> any()).
dotimes(0, _)   -> ok;
dotimes(N, Fun) ->
    _ = Fun(),
    dotimes(N - 1, Fun).

-spec dotimes_tc(non_neg_integer(), Fun) -> TimeInMicroSeconds::non_neg_integer() when
      Fun :: fun (() -> any()).
dotimes_tc(N, Fun) ->
    element(1, timer:tc(?MODULE, dotimes, [N, Fun])).

%% @doc System Activity Report
-spec sar(Options) -> ok when
      Options :: [Option],
      Option  :: {output, stdout | file:filename_all()} % default: stdout
               | {interval, pos_integer()} % seconds. default: 5
               | {count, infinity | non_neg_integer()} % default: infinity
               | {format, csv}. % default: csv
sar(Options) ->
    {ok, TRef} = timer:send_interval(val(interval, Options, 5) * 1000, {?MODULE, do_report}),
    Out = case val(output, Options, stdout) of
              stdout -> standard_io;
              OutputFile ->
                  {ok, FileDevice} = file:open(OutputFile, [write]),
                  FileDevice
          end,
    sar_loop(0,
             val(count, Options, infinity),
             TRef,
             Out,
             stat(),
             Options).

%% @doc 統計ログ出力サーバを起動する
-spec start_sar_log_server(atom(), logi:logger(), pos_integer()) -> {ok, pid()} | {error, Reason::term()}.
start_sar_log_server(ServerName, Logger, IntervalSeconds) ->
    bench_util_sar_log_server_sup:start_child(ServerName, {Logger, IntervalSeconds}).

%% @doc 統計ログ出力サーバを停止する
%%
%% 指定のサーバが存在しない場合は単に無視される
-spec stop_sar_log_server(atom()) -> ok.
stop_sar_log_server(ServerName) ->
    bench_util_sar_log_server_sup:stop_child(ServerName).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec sar_loop(non_neg_integer(),
               non_neg_integer() | infinity,
               timer:tref(),
               io:device(),
               PrevStat :: [{atom(), term()}], % XXX:
               Options  :: [{atom(), term()}]) % XXX:
              -> ok.
sar_loop(Count, Limit, TRef, Out, _, _) when Count >= Limit ->
    _ = timer:cancel(TRef),
    _ = (not is_atom(Out)) andalso file:close(Out),
    ok;
sar_loop(Count, Limit, TRef, Out, Prev, Options) ->
    ok = receive {?MODULE, do_report} -> ok end,
    Curr = stat(),
    ok = report(Count, Out, Prev, Curr, Options),
    sar_loop(Count + 1, Limit, TRef, Out, Curr, Options).

-spec report(non_neg_integer(),
             io:device(),
             PrevStat :: [{atom(), term()}], % XXX:
             CurrStat :: [{atom(), term()}], % XXX:
             Options  :: [{atom(), term()}]) % XXX:
            -> ok.
report(N, Out, Prev, Curr, _) ->
    _ = case N of
            0 -> io:format(Out, "~s\n",
                           [string:join(
                              lists:map(fun erlang:atom_to_list/1,
                                        [
                                         loop, time, reductions, context_switches,
                                         gc_count, gc_bytes, inputs, outputs, run_queue, cpu_util, scheduler_util,
                                         mem_total, mem_procs, mem_sys, mem_atom, mem_bin, mem_ets,
                                         ports, processes
                                        ]),
                              "\t")]);
            _ -> ok
        end,
    {PrevActiveTime, PrevTotalTime} =
        lists:foldl(fun ({_, Active, Total}, {AccActive, AccTotal}) -> {AccActive + Active, AccTotal + Total} end,
                    {0, 0},
                    val(scheduler_wall_time, Prev)),
    {CurrActiveTime, CurrTotalTime} =
        lists:foldl(fun ({_, Active, Total}, {AccActive, AccTotal}) -> {AccActive + Active, AccTotal + Total} end,
                    {0, 0},
                    val(scheduler_wall_time, Curr)),
    SchedulerUtilization = (CurrActiveTime - PrevActiveTime) / (CurrTotalTime - PrevTotalTime),

    CpuUtilization = lists:sum([Usage || {_, Usage, _, _} <- val(cpu_utilization, Curr)]) / length(val(cpu_utilization, Curr)) / 100,

    _ = io:format(Out,
                  "~p\t~s\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~f\t~f\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\n",
                  [
                   N,
                   format_datetime(calendar:local_time()),
                   delta(reduction_count, Curr, Prev),
                   delta(context_switch_count, Curr, Prev),
                   delta(gc_count, Curr, Prev),
                   delta(gc_bytes, Curr, Prev),
                   delta(input_bytes, Curr, Prev),
                   delta(output_bytes, Curr, Prev),
                   val(run_queue_length, Curr),
		   CpuUtilization,
                   SchedulerUtilization,
                   val(memory_total, Curr),
                   val(memory_processes, Curr),
                   val(memory_system, Curr),
                   val(memory_atom, Curr),
                   val(memory_binary, Curr),
                   val(memory_ets, Curr),
                   val(port_count, Curr),
                   val(process_count, Curr)
                  ]),
    ok.

-spec format_datetime(calendar:datetime()) -> iodata().
format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                  [Year, Month, Day, Hour, Minute, Second]).

-spec delta(atom(), [{atom(), integer()}], [{atom(), integer()}]) -> integer().
delta(Key, New, Old) ->
    val(Key, New) - val(Key, Old).

-spec val(atom(), [{atom(), term()}]) -> term().
val(Key, Assoc) ->
    proplists:get_value(Key, Assoc).

-spec val(atom(), [{atom(), term()}], term()) -> term().
val(Key, Assoc, Default) ->
    proplists:get_value(Key, Assoc, Default).
