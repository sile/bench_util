bench_util
==========

A collection of benchmark utility functions for Erlang.

Examples
--------

```erlang
%% collect system statistics
> bench_util:stat().
[{context_switch_count,438816},
 {gc_count,169988},
 {gc_bytes,6050455640},
 {input_bytes,16659997},
 {output_bytes,7036235},
 {reduction_count,147065226},
 {run_queue_length,0},
 {scheduler_wall_time,[{4,2464280012,1088013030380},
                       {1,19098477409,1088012870925},
                       {3,2583927538,1088013038389},
                       {2,9365974094,1088012796198}]},
 {memory_total,14356944},
 {memory_processes,6972176},
 {memory_system,7384768},
 {memory_atom,202481},
 {memory_binary,201176},
 {memory_code,2904490},
 {memory_ets,253872},
 {port_count,4},
 {process_count,27}]

%% timer:tc/1 + loop
> bench_util:dotimes_tc(10, fun () -> io:format("hello\n") end).
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
80    % elapsed time (in microseconds)
```
