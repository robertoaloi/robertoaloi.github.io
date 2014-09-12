---
layout      : post
title       : "Notes on Erlang QuickCheck"
description :
headline    :
modified    : 2014-09-12
category    : erlang
tags        : [erlang, testing, quickcheck]
image       :
  feature: bg_07.jpg
comments    : true
---

A few days ago I had the chance to spend some time with John Hughes,
one of the creators of the [Erlang
Quickcheck](http://www.quviq.com/products/erlang-quickcheck/).
We looked at some of our APIs and went through some of our QuickCheck models.
It has been an incredibly helpful learning experience, so I thought to
share some of the highlights from our discussions to a broader public.

The following notes are listed in a more or less random order
and they assume that you are already familiar with QuickCheck (_EQC_) and
QuickCheck abstract state machines.

### Generating sublists

Given a list of items - say, atoms - you want to extract random sublists
from the original list.

One way of doing this is to pair
each element of the list with a boolean generator and to filter out
_false_ values:

{% highlight erlang %}
sublist(L0) ->
  ?LET(L, [{E, eqc_gen:bool()} || E <- L0], [X || {X, true} <- L]).
{% endhighlight %}

Since the boolean generator shrinks towards the _false_ value, your
sublist will shrink towards the empty list, which is
what you want in most cases. If you want to invert this behaviour and
you want to shrink - or I should say to grow - towards the entire list, you can
simply toggle the _true_ boolean and set it to _false_:

{% highlight erlang %}
sublist(L0) ->
  ?LET(L, [{E, eqc_gen:bool()} || E <- L0], [X || {X, false} <- L]).
{% endhighlight %}

### Use `elements/1` or `oneof/1`?

In QuickCheck two similar functions are available: `elements/1` and
`oneof/1`. They look similar at a first glance, so you may wonder
which one to use.

The main difference between the two functions stands in the input arguments.
The `elements/1` takes a list of elements as input and it generates an
element from it, whilst `oneof/1` takes a list of generators and it
picks one of them.

The above is important for how the two functions shrink. `elements/1`
shrinks towards the first element of the list, whilst `oneof/1`
shrinks towards the _last used_ one.

### Verify your generators

Whenever you implement a new generator or even if you simply use one
of the built-in ones, you may want to verify that the generator works
as you expect. You can use `eqc_gen:sample/1` for the purpose.

{% highlight erlang %}
> eqc_gen:sample(eqc_gen:list(eqc_gen:nat())).
[7,7,1]
[6,7,3]
[]
"\t\b\f"
[1,1,10,4]
[9,6]
[]
[5,3,10,7,14]
[15,6,1,10,11,6]
[8,0]
[4,4,10,20,0]
ok
{% endhighlight %}

This is also useful to verify the sequence of commands that your abstract state
machine can generate:

{% highlight erlang %}
> eqc_gen:sample(eqc_statem:commands(my_eqc)).
> eqc_gen:sample(eqc_statem:parallel_commands(my_eqc)).
{% endhighlight %}

Where `my_eqc` is the module where your _eqc_statem_ is defined.

### Think shrinking

The `elements/1` generator shrinks towards the beginning of the list.
Keep that in mind when implementing your QuickCheck abstract state machine. If
you have a list of elements - say, pids - in your model state and you
plan to use `elements/1` on them, ensure that whenever a new element is added
to the state, it is always appended at the end of a list, [even if you
are a seasoned Erlang developer and it feels a bit unnatural to
you](http://www.erlang.org/doc/efficiency_guide/myths.html#id56691). This
will give you a better shrinking in most cases.

{% highlight erlang %}
### DO
add_next(S, Res, [Pid]) ->
  S#state{pids = S#state.pids ++ [Pid]}

### DON'T
add_next(S, Res, [Pid]) ->
  S#state{pids = [Pid | S#state.pids]}
{% endhighlight %}

Don't bother about quadratic complexity here, since you will only run
this code during tests and you will probably have only a few elements
in your state at any given time.

### Live in the Erlang shell

Sometimes the feedback loop for executing QuickCheck tests can be
long and/or the way properties are verified can be a bit
inflexible. As an example, you may be wrapping your QuickCheck
properties in EUnit suites to get code coverage information, or you
may run QuickCheck via [rebar](https://github.com/basho/rebar).

When implementing new properties or debugging an abstract state
machine, try to run QuickCheck from the Erlang shell as much as
possible. Being able to run `eqc:check/1` on your latest
counterexample and being able to re-run a property in a matter of
seconds after a quick fix will reveal a great win in the long term.

### How long will my commands take?

In your QuickCheck abstract state machine, you have operations which can take
long time and you may want to set a timeout for the entire test suite. You don't
want to set the timeout too small, since you may incur into _occasionaly
failing test cases_ (due to the timeout being hit, in case long
sequences of commands are generated). On the other hand, you don't
want to set the timeout too long, since you don't want your tests to
run forever. So, what's a good value for a timeout?

Well, since the sequence of command is available in your property, you
could calculate a timeout value from the sequence of commands itself:

{% highlight erlang %}
prop_bar() ->
   ?FORALL(Cmds, commands(),
     ?TIMEOUT(my_timeout(Cmds),
       begin
        ...
       end)).

my_timeout(Cmds) ->
  lists:sum([expected_time(Cmd) || Cmd <- Cmds]).

expected_time({call, ?MODULE, op1, _Args}) ->
  2000;
expected_time({call, ?MODULE, op2, _Args}) ->
  3000.
{% endhighlight %}

At this point, you should get the idea.

### Non derministic test outcomes

In certain situations, test outcomes are non-deterministic.
In such cases, the `?SOMETIMES/2` macro can help.
An extract from the QuickCheck documentation follows.

{% highlight erlang %}
?SOMETIMES(N,Prop)

A property which tests Prop repeatedly N times, failing only if all of the
tests fail. In other words, the property passes if Prop sometimes passes.
This is used in situations where test outcomes are non-deterministic,
to search for test cases that consistently fail.
A property such as ?FORALL(X,...,?SOMETIMES(10,...)) will find test cases X for
which the property inside ?SOMETIMES is very likely to fail.
{% endhighlight %}

### More compacted FSMs

The most recent versions of QuickCheck have a new format for defining
an abstract state machine, which is more readable and concise and which requires
much less boilerplate than before. You can
find extensive documentation about the new _statem_ format by looking at the
documentation for the `eqc_group_commands` module. Not easy to find, which is
why I'm mentioning it here.

### Failing is good

Too often your QuickCheck tests pass and your properties are
successful. In many occasions it's useful to ensure that your tests
are meaningful by testing properties that should *not* succeed. You
can perform negative testing by wrapping a property using the
`eqc:fails/1` function.

{% highlight erlang %}
fails(?FORALL(...))
{% endhighlight %}

### Testing race conditions

In QuickCheck, going from sequential testing to parallel testing is a
snap in most cases. As an example, the following property:

{% highlight erlang %}
prop_registry() ->
  ?FORALL(Cmds, commands(?MODULE),
    begin
      cleanup(),
      {H, S, Res} = run_commands(?MODULE, Cmds),
      pretty_commands(?MODULE, Cmds, {H, S, Res},
                      aggregate(command_names(Cmds),
                      Res == ok))
    end).
{% endhighlight %}

Would become:

{% highlight erlang %}
prop_registry() ->
  ?FORALL(Cmds, parallel_commands(?MODULE),
    begin
      cleanup(),
      {H, S, Res} = run_parallel_commands(?MODULE, Cmds),
      pretty_commands(?MODULE, Cmds, {H, S, Res},
                      aggregate(command_names(Cmds),
                      Res == ok))
    end).
{% endhighlight %}

You can use _preconditions_ to ensure logical precedence between operations.

### Running on multicore

If you are testing race conditions and you are using `parallel_cmds` as above,
ensure you are running on a multicore system. Do not underestimate
this requisite since, even if your machine has at least 4 or 8 cores,
your application could be running in a VM or in a Jenkins slave with
limited cores. In such a situation, in fact, the Erlang scheduler will
try as hard as possible to prevent context switches between processes
and your parallel tests may be less useful than you think.

### About `check/1` and `check/2`

It's possible to test a property for a given case (often the current
counterexample) by using the `check/1` and `check/2` functions
exported by the `eqc` module. An interesting application of the
`check` functions is in a system where plenty of log information are
produced by the system under test while running QuickCheck. By using
the following pattern, it will be possible to reply a failing test
case and to display only relevant logs.

{% highlight erlang %}
case eqc:quickcheck(Property) of
        true -> true;
        _    -> io:format(user, "Repeating failing test~n", []),
                true = eqc:check(Prop)
end.
{% endhighlight %}

As a plus, the `check/2` function can be used without a QuickCheck
licence, allowing anyone to run tests that a licenced user has
generated.
