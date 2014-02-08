---
layout     : post
title      : "Skerleton: bootstrap Erlang Projects in Seconds"
categories : erlang
---

Every time you create a brand-new Erlang project, some manual steps
are required. Most of the times, this means fetching `rebar` via
`wget`, copying and pasting a `rebar.config` from a previous project,
making a bunch of new directories, creating an empty release, and so
on and so forth.

To ease the above, I created a simple skeleton project that you can
use to bootstrap your new Erlang projects. Here is a quickstart on how
to use skerleton. You can find the skerleton source code on
[GitHub](https://github.com/robertoaloi/skerleton).


### Create a new project based on skerleton and bootstrap it

````
git clone https://github.com/robertoaloi/skerleton.git my_app
cd my_app
./bootstrap.sh
````

### Remove the skerleton left-overs and you are ready to go

````
rm -rf bootstrap.sh README.md .git rel/reltool.config.template
git init
git add . && git commit -a -m "Initial commit."
````

### start your Erlang node

````
make rel
rel/my_app/bin/my_app console
````
