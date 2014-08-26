%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Benchmark Utility Functions
-module(bench_util).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([stat/0]).
-export([dotimes/2]).
-export([dotimes_tc/2]).

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
     {context_switch_count, element(1, statistics(context_switches))},
     {gc_count, Number_of_GCs},
     {gc_bytes, Words_Reclaimed * erlang:system_info(wordsize)},
     {input_bytes, InputBytes},
     {output_bytes, OutputBytes},
     {reduction_count, element(1, statistics(reductions))},
     {run_queue_length, statistics(run_queue)},
     {scheduler_wall_time, SchedulerWallTime},
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
