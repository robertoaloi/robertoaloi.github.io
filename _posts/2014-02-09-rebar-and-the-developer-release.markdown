---
layout      : post
title       : "Rebar and the Developer Shell"
description :
headline    :
modified    : 2014-02-09
category    : erlang
tags        : [erlang, rebar, devrel, productivity]
image       :
comments    : true
---

In some cases (e.g. during development) it is helpful to symlink
applications into a release, rather than copying them. I believe
_relx_ has a dev-mode option for that. In rebar-based projects, what I
end up doing in most of the cases is to add something like this into a
Makefile:

{% highlight Makefile %}
{% raw %}
devrel: rel
  $(foreach dep, $(wildcard deps/* apps/*), \
    $(eval $@_target := $(wildcard rel/svt/lib/$(shell basename $(dep))-*)) \
    rm -rf $($@_target) && ln -sfn $(abspath $(dep)) $($@_target); \
  )
{% endraw %}
{% endhighlight %}

[_Riak_ does something on these
lines](https://github.com/basho/riak/blob/develop/Makefile#L102), too.

Even if I don't think this behaviour is achievable directly via
_reltool_, it could be implement as an optional step in _rebar_reltool_
or via a dedicated plugin or separate command.

I will probably give this a try at some point. Does anyone have an
opinion on the above?
