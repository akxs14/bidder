PROJECT = master_banker

DEPS = cowboy, jiffy
dep_cowboy = git@github.com:extend/cowboy.git master
dep_jiffy = git@github.com:davisp/jiffy.git master

.PHONY: release clean-release

release: 
	rebar compile
	relx -o rel

clean-release: 
	rm -rf rel/master_banker

chmod:
	chmod a+x rel/master_banker/bin/master_banker

start:
	sh rel/master_banker/bin/master_banker

full:
	rebar compile
	relx -o rel
	chmod a+x rel/master_banker/bin/master_banker
	sh rel/master_banker/bin/master_banker

include erlang.mk
