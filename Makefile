PROJECT = bidder

DEPS = cowboy jiffy
dep_cowboy = https://github.com/extend/cowboy.git 0.10.0
dep_jiffy = https://github.com/davisp/jiffy.git 0.11.3

.PHONY: release clean-release

release: clean-release all projects
	relx -o rel/$(PROJECT)

clean-release: clean-projects
	rm -rf rel/$(PROJECT)

include erlang.mk


