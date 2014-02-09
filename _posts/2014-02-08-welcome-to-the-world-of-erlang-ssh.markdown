---
layout      : post
title       : "Welcome to the World of Erlang SSH"
description :
headline    :
modified    : 2014-02-08
category    : erlang
tags        : [erlang, ssh]
image       :
  feature: bg_05.jpg
comments    : true
---

I've been playing around with the Erlang SSH application lately and I've
noticed some weird behaviours which I decided to report here for future
reference.

The general impression that I got from the application - and from its
coursins such as _crypto_, _public_key_ and _ssh_ - is that it does
not reflect the usual OTP quality standards and that it should be used
with care.

### Misleading error message on _ssh\_sftp:start\_channel/1_ in case of missing shell


When starting a SFTP channel towards a system where user does not have
a shell (i.e. it has `/bin/false` or equilvalent assigned in the
`/etc/passwd` a misleading error message is returned to the user:

{% highlight erlang %}
{% raw %}
1> test:go().
** exception exit: {normal,{gen_server,call,
                                       [<0.53.0>,
                                        {{timeout,infinity},
                                         wait_for_version_negotiation},
                                        infinity]}}
     in function  gen_server:call/3 (gen_server.erl, line 188)
     in call from ssh_channel:call/3 (ssh_channel.erl, line 88)
     in call from ssh_sftp:start_channel/2 (ssh_sftp.erl, line 93)
     in call from test:connect/1 (test.erl, line 14)
{% endraw %}
{% endhighlight %}

Where:

{% highlight erlang %}
{% raw %}
-module(test).
-compile(export_all).

go() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssh),
    connect("rabbitmq").

connect(Username) ->
    {ok, SSHRef} = ssh:connect("localhost", 22, [{user, Username}]),
    {ok, _SFTPChannelRef} = ssh_sftp:start_channel(SSHRef),
    ok.
{% endraw %}
{% endhighlight %}

This has been experienced in both R16B03 and R15B03.

### Crash on SSH connect

On R16B03, when connecting via SSH on OS X Mavericks I get:

{% highlight erlang %}
{% raw %}
1> test:go().
** exception error: no match of right hand side value {error,"Internal error"}
     in function  test:connect/1 (test.erl, line 13)
3>
=ERROR REPORT==== 7-Feb-2014::13:38:06 ===
Erlang ssh connection handler failed with reason: function_clause
, Stacktace: [{ssh_connection_handler,userauth,
                  [{ssh_msg_userauth_pk_ok,<<>>, ...
{% endraw %}
{% endhighlight %}

### Internal Crash is not real when using the _key_cb_ option

When using the _key_cb_ option on `ssh:connect/3` the user should
implement a series of callback functions. Not implementing them and
trying to connect results in an "internal error" which invites the
user to send a bug report. The undef callback function should be
catched and reported as a normal error, not an internal one.


{% highlight bash %}
{% raw %}
please report this to erlang-bugs@erlang.org
{% endraw %}
{% endhighlight %}
