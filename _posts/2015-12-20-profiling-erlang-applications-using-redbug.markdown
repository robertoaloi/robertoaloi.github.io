---
layout      : post
title       : "Profiling Erlang Apps using Redbug"
description :
headline    :
modified    : 2015-12-20
category    : erlang
tags        : [erlang, profiling]
image       :
  feature: bg_09.jpg
comments    : true
---

When working with Erlang, I often find myself using a simple profiling technique. The technique allows me to quickly identify where time is spent within a given code path of an Erlang application. When pair programming, people are sometimes astonished how simple and effective this technique is and often fall in love with it. This is why I decided to share it with a wider audience.

I normally adopt this technique as my first attempt when looking for bottlenecks in Erlang code, before attempting more sophisticated routes which may involve using tools such as [fprof](http://www.erlang.org/doc/man/fprof.html), [eprof](http://www.erlang.org/doc/man/eprof.html) or [eflame](https://github.com/proger/eflame) or implementing my own utilities on top of [dbg](http://www.erlang.org/doc/man/dbg.html) or [ttb](http://www.erlang.org/doc/man/ttb.html). You would be surprised to hear how often this approach revealed to be enough, allowing me to stop my profiling analysis straight away.

The technique is based on _redbug_, an open source debugging tool part of the [eper](https://github.com/massemanet/eper) Erlang application. Although redbug is primarily designed as a debugging tool, it can be used quite effectively for profiling purposes, too. In fact, in Erlang the boundary between debugging and profiling is subtle, given that most of the existing debuggers and profilers utilize the powerful Erlang [tracing](http://www.erlang.org/doc/man/erlang.html#trace-3) facilities as their own foundation.

In this post, after defining the concepts of _tracing_ and _profiling_, we will introduce the _redbug_ tool, explaining how to use it for profiling purposes.

### Tracing and Profiling

[Wikipedia](https://en.wikipedia.org/wiki/Tracing_(software)) defines _tracing_ as a "_specialized use of logging to record information about a program's execution_", whilst in [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) the authors talk of _profiling_ as the ability of "_gathering **statistics** about the code as it’s **running**, associating the **data** with corresponding points in the **code**"_. In Erlang the two concepts are tightly coupled and most Erlang profilers are implemented using the Erlang tracing functionalities, which allow you to be _notified_ whenever a specific event happens.

### Redbug

Redbug is a _debugging_ utility, which allows you to easily interact with the Erlang tracing facilities. In Erlang, you can generate _trace messages_ whenever certain events occur. An event could be the sending or the receive of a message, or a function being called. Redbug is a wonderful and simple tool which will save your life at least once, if you give him an opportunity. One of my favourite Redbug features is its ability to shut himself down in case of overload. A full description of Redbug esulates from the purpose of this blog post, but we will still go through its main features and use cases.

### Installation

You can clone redbug via:

    $ git clone https://github.com/massemanet/eper.git

Then, compile it with:

    $ cd eper
    $ make

Ensure _eper_ is included in your path when you start an Erlang shell. You can achieve this by adding the following flag when invoking `erl`:

    $ erl -pa /path/to/eper/ebin

Alternatively, you can add the following line to your `~/.erlang` file to get it executed at startup:

    code:add_patha("/path/to/eper/ebin").

### Usage

Redbug is safe to be used in production, thanks to a self-protecting mechanism against overload, which kills the tool in case too many tracing messages are around, preventing the Erlang node to become overloaded due to an heavy profiling session.

Assuming Redbug is in your path, you can use redbug as follows:

    $ erl
    Erlang/OTP 17 [erts-6.4] [...]
    
    Eshell V6.4  (abort with ^G)
    1> l(redbug).
    {module,redbug}
    
    2> redbug:start("lists:sort/1").
    {30,1}
    
    3> lists:sort([3,2,1]).
    [1,2,3]
       
    % 15:20:20 <0.31.0>({erlang,apply,2})
    % lists:sort([3,2,1])
    redbug done, timeout - 1

After ensuring that the redbug module is available and loaded, we start redbug, explaining we are interested in the function named `sort` with _arity_ 1, exported by the module `lists`. Remember that, in Erlang lingo, the arity represents the number of input arguments that a given function takes. As the third instruction, we invoke the `lists:sort/1` function and we verify that a message is produced by redbug (more about the message below). After the default timeout (15 seconds) redbug stops and displays the message _"redbug done"_. Redbug is also kind enough to tell us the reason why it stopped (_timeout_ reached) and the number of messages that it collected so far: _1_.

Let's look at the single message produced by redbug. By default messages are printed to the standard output, but it's also possible to dump them to file.

    % 15:20:20 <0.31.0>({erlang,apply,2})
    % lists:sort([3,2,1])

Depending on the version of redbug you are using, you might get a slightly different message. In this case, the message is split across two lines. The first line contains a timestamp, the Process Identifier (aka _PID_) of the Erlang process which invoked the function and the _caller_ function.
The second line contains the function _called_, including the input arguments. Both lines are prepended with a `%`, which reminds us of the syntax for _Erlang comments_.

We can also ask Redbug to produce an extra message for the return value. This is achieved using the following syntax:

    4> redbug:start("lists:sort/1->return").
    {30,1}
    
    5> lists:sort([3,2,1]).                 
    [1,2,3]
    
    % 15:35:52 <0.31.0>({erlang,apply,2})
    % lists:sort([3,2,1])
       
    % 15:35:52 <0.31.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,3]
    redbug done, timeout - 1
    
In this case two messages are produced, one when entering the function and one when leaving the same function. This feature will constitute the basis of our profiling technique.

In the above examples, timestamp have the granularity of _seconds_. When profiling code, you normally work in the range of milliseconds, so those timestamps are not enough. The good news is that you can enable printing of _milliseconds_ in Redbug:

    6> redbug:start("lists:sort/1->return", [{print_msec, true}]).
    {30,1}
    
    7> lists:sort([3,2,1]).                          
    [1,2,3]
    
    % 15:44:08.030 <0.31.0>({erlang,apply,2})
    % lists:sort([3,2,1])
        
    % 15:44:08.030 <0.31.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,3]
    redbug done, timeout - 1

As you can see, milliseconds are now included in the timestamps. The list to sort in the example was really small, so the entire operation took less than one millisecond.

When dealing with real code, trace messages can be complex and therefore hardly readable. Let's see what happens if we try to trace the sorting of a list containing 10.000 elements.

    8> lists:sort(lists:seq(10000, 1, -1)).                       
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     23,24,25,26,27,28,29|...]
        
    % 15:48:42.208 <0.77.0>({erlang,apply,2})
    % lists:sort([10000,9999,9998,9997,9996,9995,9994,9993,9992,9991,9990,9989,9988,9987,9986, 
     9985,9984,9983,9982,9981,9980,9979,9978,9977,9976,9975,9974,9973,9972,9971,
     9970,9969,9968,9967,9966,9965,9964,9963,9962,9961,9960,9959,9958,9957,9956,
     9955,9954,9953,9952,9951,9950,9949,9948,9947,9946,9945,9944,9943,9942,9941,
     9940,9939,9938,9937,9936,9935,9934,9933,9932,9931,9930,9929,9928,9927,9926,
     9925,9924,9923,9922,9921,9920,9919,9918,9917,9916,9915,9914,9913,9912,9911,
     [...]
      84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,
     59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,
     34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,
     8,7,6,5,4,3,2,1])
        
    % 15:48:42.210 <0.77.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
                       23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                       42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                       61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
                       80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,
                       99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,
     [...]
                        9951,9952,9953,9954,9955,9956,9957,9958,9959,9960,9961,
                       9962,9963,9964,9965,9966,9967,9968,9969,9970,9971,9972,
                       9973,9974,9975,9976,9977,9978,9979,9980,9981,9982,9983,
                       9984,9985,9986,9987,9988,9989,9990,9991,9992,9993,9994,
                       9995,9996,9997,9998,9999,10000]
    redbug done, timeout - 1

I truncated most of the output here, but you should get the idea. Once again, we can use another redbug option to our advantage. Let's omit the input arguments and the return value from the produced output. The `arity` option tells Redbug to only display the number of input arguments for the given function, instead of their actual value. The `print_return` option is [my own recent little contribution to Redbug](https://github.com/massemanet/eper/pull/34). Setting the option to `false` tells Redbug not to display the return value and instead to display a `'...'` symbol. Let's try again.

    9> redbug:start("lists:sort/1->return", [{arity, true}, {print_msec, true}, {print_return, false}]).
    {30,1}
    
    10> lists:sort(lists:seq(10000, 1, -1)).                                                             
    % 15:55:32.215 <0.77.0>({erlang,apply,2})
    % lists:sort/1
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     23,24,25,26,27,28,29|...]
    
    % 15:55:32.216 <0.77.0>({erlang,apply,2})
    % lists:sort/1 -> '...'
    redbug done, timeout - 1

That's a bit more readable. I always tend to start Redbug in _quiet mode_ and to include input arguments and return values only when necessary.

By default, redbug stops after 15 seconds or after 10 messages are received. Those values are a safe default, but they're usually not enough. You can bump those limits by using the `time` and `msgs` options. `time` is expressed in milliseconds.

    11> redbug:start("lists:sort/1->return", [{arity, true}, {print_msec, true}, {print_return, false}, {time, 60 * 1000}, {msgs, 100}]).
    {30,1}

You can also activate redbug for several function calls simultaneously.

    12> redbug:start(["lists:sort/1->return", "lists:sort_1/3->return"]).
    {30,2}

    13> lists:sort([4,4,2,1]).                                                           
    [1,2,4,4]
    
    % 18:39:26 <0.32.0>({erlang,apply,2})
    % lists:sort([4,4,2,1])
    
    % 18:39:26 <0.32.0>({erlang,apply,2})
    % lists:sort_1(4, [2,1], [4])
    
    % 18:39:26 <0.32.0>({erlang,apply,2})
    % lists:sort_1/3 -> [1,2,4,4]
    
    % 18:39:26 <0.32.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,4,4]
    redbug done, timeout - 2

Last but not least, redbug offers the ability to only display results for matching input arguments. This is when the syntax looks a bit like magic.

    14> redbug:start(["lists:sort([1,2,5])->return"]).                   
    {30,1}
    
    15> lists:sort([4,4,2,1]).                                        
    [1,2,4,4]
    
    16> lists:sort([1,2,5]).                                          
    [1,2,5]

    % 18:45:27 <0.32.0>({erlang,apply,2})
    % lists:sort([1,2,5])

    % 18:45:27 <0.32.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,5]
    redbug done, timeout - 1

In the above example, we are telling redbug we're only interested in function calls to the `lists:sort/1` function when the input arguments is the list `[1,2,5]`. This allows us to remove a huge amount of noise in the case our target function is used by many actors at the same time and we are only interested in a specific use case.

Oh, and don't forget that you can use the _underscore_ as a _wildcard_...

    17> redbug:start(["lists:sort([1,_,5])->return"]).
    {30,1}
    
    18> lists:sort([1,2,5]).
    [1,2,5]

    % 18:49:07 <0.32.0>({erlang,apply,2})
    % lists:sort([1,2,5])
    
    % 18:49:07 <0.32.0>({erlang,apply,2})
    % lists:sort/1 -> [1,2,5]
    
    19> lists:sort([1,4,5]).
    [1,4,5]
    
    % 18:49:09 <0.32.0>({erlang,apply,2})
    % lists:sort([1,4,5])
    
    % 18:49:09 <0.32.0>({erlang,apply,2})
    % lists:sort/1 -> [1,4,5]
    redbug done, timeout - 2

This blog post doesn't pretend to be a comprehensive guide to redbug, so you're envisaged to have a look to [eper](https://github.com/massemanet/eper) yourself. You won't regret it. A convenient help function exists, too:

    13> redbug:help().

### Profiling using Redbug

Now, back to our original topic. How should you use _redbug_ for profiling purposes? Well, use the following recipe.

1. Use the `->return` syntax to collect messages when both entering and leaving a function
2. Use the `print_msec` option to get milliseconds granularity in timestamps
3. Use the `arity` and `print_return` options to obtain more readable output
4. Enable redbug for multiple _potentially slow_ functions in one go
5. Execute the code path which is perceived as _slow_
6. Look at the times reported by redbug and focus on the _slower_ function
7. Look in the implementation for that function for find potential slowness candidates
8. Repeat points 1-6 for the functions found in 7, until you identify the real bottleneck
9. Profit

Oh, and if you reached this point, it probably means that this topic of interest to you.

In that case, [we are hiring](http://app.jobvite.com/m?32zEJhwK).
