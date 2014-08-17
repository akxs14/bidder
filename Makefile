PROJECT = bidder

DEPS = cowboy jiffy
dep_cowboy = https://github.com/extend/cowboy.git master
dep_jiffy = https://github.com/davisp/jiffy.git master
 
.PHONY: release clean-release

release: 
	relx -o rel/bidder
	chmod a+x rel/bidder/bidder/bin/bidder

clean-release: 
	rm -rf rel/bidder

start:
	sh rel/bidder/bidder/bin/bidder

include erlang.mk
