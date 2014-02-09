---
layout      : post
title       : "Create and deploy your Erlang / Cowboy Application on Heroku in 30 minutes"
description :
headline    :
modified    : 2013-07-13
category    : erlang
tags        : [erlang, heroku]
image       :
comments    : true
---

<div style="text-align:center">
  <img src="/images/herloku.png" alt="Herloku" /></a>
</div>
<br />

[Erlang](http://www.erlang.org) is a programming language used to build massively scalable soft real-time systems with requirements on high availability.

[Cowboy](https://github.com/extend/cowboy) is a small, fast and modular HTTP server written in Erlang.

[Heroku](https://www.heroku.com/) is a cloud application platform which allows you to deploy and scale your own application at pleasure.

In this post, we will write an Erlang/Cowboy Web Application from scratch, deploying it on Heroku. Since Heroku [offers a free tier for deploying web apps](https://devcenter.heroku.com/articles/usage-and-billing#750-free-dyno-hours-per-app), this will cost you nothing. Also, the entire process should take no more than half an hour.

## Requirements

* You have [git](http://git-scm.com/) installed on your machine.
* You have [Erlang](https://www.erlang-solutions.com/downloads/download-erlang-otp) installed on your machine. In this example I will be using R15B01.
* You are familiar with the basic concepts of [Erlang and OTP](http://erlang.org/doc/)
* You have a free account on [Heroku](https://www.heroku.com/).
* You have [the Heroku start-kit for your OS](https://toolbelt.heroku.com/) installed.

## Summary

* [Create the skeleton of an Erlang app using Rebar](#rebar)
* [Create a Heroku application](#heroku)
* [Use Cowboy to create a simple web app](#cowboy)
* [Compile and run your application locally](#compile)
* [Configure your Heroku app](#config)
* [Deploy your Erlang application on Heroku](#deploy)
* [Profile your deployed Erlang application](#profile)

<a id="rebar"></a>

### Create the skeleton of an Erlang app using Rebar

[Rebar](https://github.com/rebar/rebar) is the de-facto standard build-tool for Erlang projects.

Fetch rebar from Github and bootstrap it:

{% highlight bash %}
$ git clone https://github.com/rebar/rebar.git
$ cd rebar
$ ./bootstrap
$ cd ..
{% endhighlight %}

Initialize a new git repository and use rebar to create the skeleton for a new Erlang app. I decided to call my application *erlblog*. Call your application differently, replacing every occurrence of *erlblog* with your favourite application name in the instructions below. Please note that this is **not** optional, since two applications cannot have the same name on Heroku and you don't dare to clash with my own application.

{% highlight bash %}
$ git init erlblog
$ cd erlblog
$ cp ../rebar/rebar .
$ ./rebar create-app appid=erlblog
{% endhighlight %}

Commit what you have done in git:

{% highlight bash %}
$ git add rebar src
$ git commit -m "Add rebar skeleton"
{% endhighlight %}

<a id="heroku"></a>

### Create a Heroku application

Login into Heroku using the *heroku* command from the terminal.

{% highlight bash %}
$ heroku login
{% endhighlight %}

Use your Heroku email and password to login.

Now create a new Heroku app, using the Erlang buildpack from [@archaelus](https://github.com/archaelus):

{% highlight bash %}
$ heroku create erlblog --stack cedar --buildpack https://github.com/archaelus/heroku-buildpack-erlang
{% endhighlight %}

<a id="cowboy"></a>

### Use Cowboy to create a simple web app

Add the *Cowboy* web server as a rebar dependency:

{% highlight erlang %}
$ cat rebar.config

{deps, [
        {cowboy, "0.8.4", {git, "https://github.com/extend/cowboy.git", {tag, "0.8.4"}}}
       ]}.
{% endhighlight %}

Add *cowboy* to the list of applications in your **.app.src** file. Also, set the *http_port* environment variable to *8080* (see next paragraphs).

{% highlight erlang %}
$ cat src/erlblog.app.src

{application, erlblog,
 [
  {description, ""},
  {vsn, "1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  cowboy
                 ]},
  {mod, { erlblog_app, []}},
  {env, [{http_port, 8080}]}
 ]}.
{% endhighlight %}

Modify the **start/2** function from the **erlblog_app** module so that Cowboy starts a pool of acceptors when the erlblog application is started. Configure the Cowboy dispatcher with a single dispatching rule, routing all requests to **'/'** to the **erlblog_handler** (see below).

Heroku assigns random ports to your application and uses the OS environment variable *$PORT* to inform you about the port on which your web server should listen to. Therefore, in the following code we read that environment variable, defaulting to port 8080 in case the environment variable is not specified. This is useful, for example, if you want to try your web server locally before deploying it on Heroku.

{% highlight erlang %}
$ cat src/erlblog_app.erl

-module(erlblog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    erlblog_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
    [
     {'_', [
            {"/", erlblog_handler, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
{% endhighlight %}

Let's now implement a basic HTTP Cowboy handler, which simply replies with a 200 status code and a notorious welcoming message to any incoming request:

{% highlight erlang %}
$ cat src/erlblog_handler.erl

-module(erlblog_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello world!">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
{% endhighlight %}

Finally, let's create an interface module which will be responsible for starting your *erlblog* application together with all its dependencies.

{% highlight erlang %}
$ cat src/erlblog.erl

-module(erlblog).

-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(erlblog).
{% endhighlight %}

<a id="compile"></a>

### Compile and run your application locally

Compile the *erlblog* application using rebar:

{% highlight bash %}
$ ./rebar get-deps compile
{% endhighlight %}

Start the application and verify that everything works as expected:

{% highlight bash %}
$ erl -pa ebin deps/*/ebin -s erlblog
{% endhighlight %}

From the Erlang shell, type:

{% highlight erlang %}
1> application:which_applications().
{% endhighlight %}

The erlblog application should be included in the output.

Finally, point your browser to:

{% highlight bash %}
http://localhost:8080
{% endhighlight %}

And verify that the string *"Hello World!"* is there.

You can use **Ctrl-G q** to exit the Erlang shell.

If everything works as expected, commit everything to git:

{% highlight bash %}
$ git add rebar.config src
git ci -m "Include Cowboy skeleton"
{% endhighlight %}

<a id="config"></a>

### Configure your Heroku app

You need to tell Heroku that you're going to deploy an Erlang Application. To do so, you need to create a *Procfile* file, containing your start-up script:

{% highlight bash %}
$ cat Procfile

web: erl -pa ebin deps/*/ebin -noshell -noinput -s erlblog
{% endhighlight %}

Commit your changes to git:

{% highlight bash %}
$ git add Procfile
$ git ci -m "Add Procfile"
{% endhighlight %}

You also want to specify that your application requires Erlang R15B01:

{% highlight bash %}
$ cat .preferred_otp_version

OTP_R15B01
{% endhighlight %}

Commit your changes to git:

{% highlight bash %}
$ git add .preferred_otp_version
$ git ci -m "Specify R15B01 as Erlang version"
{% endhighlight %}

<a id="deploy"></a>

### Deploy your Erlang application on Heroku

That's the beautiful part:

{% highlight bash %}
$ git push heroku master
{% endhighlight %}

You should now be able to access the erlblog application at:

{% highlight bash %}
http://erlblog.herokuapp.com
{% endhighlight %}

If something does not work as expected, you might want to verify the logs for your Heroku app:

{% highlight bash %}
$ heroku logs
{% endhighlight %}

<a id="profile"></a>

### Profile your deployed Erlang application

Let's now verify how many requests our erlblog application can handle. Please note that to run the steps below, you need [ab](http://httpd.apache.org/docs/2.2/programs/ab.html) and [gnuplot](http://www.gnuplot.info/) installed on your machine.

Using ApacheBench, perform 5000 HTTP requests against your new web server, using 20 concurrent requests. Store the output in the *gnuplot.dat* file.

{% highlight bash %}
$ ab -n 5000 -c 20 -g gnuplot.dat http://erlblog.herokuapp.com/

This is ApacheBench, Version 2.3 <$Revision: 655654 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking erlblog.herokuapp.com (be patient)
Completed 500 requests
Completed 1000 requests
Completed 1500 requests
Completed 2000 requests
Completed 2500 requests
Completed 3000 requests
Completed 3500 requests
Completed 4000 requests
Completed 4500 requests
Completed 5000 requests
Finished 5000 requests


Server Software:        Cowboy
Server Hostname:        erlblog.herokuapp.com
Server Port:            80

Document Path:          /
Document Length:        12 bytes

Concurrency Level:      20
Time taken for tests:   71.473 seconds
Complete requests:      5000
Failed requests:        0
Write errors:           0
Total transferred:      615000 bytes
HTML transferred:       60000 bytes
Requests per second:    69.96 [#/sec] (mean)
Time per request:       285.891 [ms] (mean)
Time per request:       14.295 [ms] (mean, across all concurrent requests)
Transfer rate:          8.40 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:      121  130  46.4    127    1317
Processing:   126  154  60.0    140    1176
Waiting:      125  153  60.0    139    1162
Total:        248  284  75.7    268    1495

Percentage of the requests served within a certain time (ms)
  50%    268
  66%    275
  75%    282
  80%    286
  90%    307
  95%    343
  98%    441
  99%    617
 100%   1495 (longest request)
{% endhighlight %}

Using our Heroku free tier (1 single dyno worker), our Cowboy Web Server managed to complete all **5000 requests**, allowing **~70 requests per second**. 90% of the requests have been served in about **300 ms**. Of course, such a good result has been possible only because we surely have been hitting some kind of cache in the Heroku servers. Still, not bad for a free hosting solution for a simple Erlang applications.

We can visualize the above results using Gnuplot:

{% highlight bash %}
$ gnuplot

gnuplot> plot "gnuplot.dat" using 9 smooth sbezier with lines title "Cowboy Heroku Benchmarking"
{% endhighlight %}

<div style="text-align:center">
  <img src="/images/gnuplot.png" alt="Gnuplot" /></a>
</div>
<br />

The complete source code for the *erlblog* application is available [here](https://github.com/robertoaloi/erlblog).
