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

```erlang
> bench_util:sar([]).
loop    time    reductions      context_switches        gc_count        gc_bytes        inputs  outputs run_queue       cpu_util        scheduler_util  mem_total       mem_procs       mem_sys mem_atom        mem_bin mem_ets ports   processes
0       2015-04-23T17:49:30     1386    12      5       23072   296     1       0       0.012235        0.000017        14728664        5405280 9323384 215345  718704  257056  7       42
1       2015-04-23T17:49:35     3288    18      5       81400   296     292     0       0.037843        0.000026        14763712        5439912 9323800 215345  719120  257056  7       42
2       2015-04-23T17:49:40     2974    15      2       277192  296     119     0       0.078227        0.000026        14533504        5210384 9323120 215345  718440  257056  7       42
3       2015-04-23T17:49:45     2938    15      6       96464   296     120     0       0.022739        0.000028        14541240        5217960 9323280 215345  718600  257056  7       42
4       2015-04-23T17:49:50     2828    15      3       80288   296     119     0       0.010980        0.000021        14541400        5217960 9323440 215345  718760  257056  7       42
5       2015-04-23T17:49:55     2836    15      2       27384   296     119     0       0.002510        0.000021        14528824        5205168 9323656 215345  718976  257056  7       42
6       2015-04-23T17:50:00     13700   26      6       93608   313     120     0       0.001003        0.000037        14549632        5225776 9323856 215345  719240  257056  7       42
7       2015-04-23T17:50:05     2848    15      3       101240  296     120     0       0.002503        0.000021        14563464        5239384 9324080 215345  719400  257056  7       42
8       2015-04-23T17:50:10     2905    15      4       94944   296     120     0       0.011266        0.000025        14563624        5239384 9324240 215345  719560  257056  7       42
9       2015-04-23T17:50:15     2798    15      1       17808   296     119     0       0.004982        0.000021        14563784        5239384 9324400 215345  719720  257056  7       42
```

References
---------
- http://www.slideshare.net/lpgauth/performance-optimization-101-erlang-factory-sf-2014
- http://www.erlang-factory.com/upload/presentations/795/lkubica_erlangfactory_2013.pdf
- https://www.erlang-solutions.com/resources/webinars/understanding-erlang-scheduler
