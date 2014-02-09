---
layout      : post
title       : "Killin' them softly (by name)"
description :
headline    :
modified    : 2014-02-08
category    : erlang
tags        : []
image       :
comments    : true
---

Sometimes is useful to kill an Erlang node by name.
The task involves several manual steps, Here is a little helper which
you might find helpful.

{% gist 8884096 %}

Remember that if you want to kill all Erlang nodes running on a
system rather than a specific one, you can simply do:


````
killall beam.smp
````
