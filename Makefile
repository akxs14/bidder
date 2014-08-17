PROJECT = bidder

DEPS = cowboy jiffy
dep_cowboy = https://github.com/extend/cowboy.git master
dep_jiffy = https://github.com/davisp/jiffy.git master
 
.PHONY: release clean-release

release: 
	rebar compile
	relx -o rel

clean-release: 
	rm -rf rel/bidder

chmod:
	chmod a+x rel/bidder/bin/bidder

start:
	sh rel/bidder/bin/bidder

full:
	rebar compile
	relx -o rel
	chmod a+x rel/bidder/bin/bidder
	sh rel/bidder/bin/bidder

include erlang.mk
