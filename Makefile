compile:
	rebar compile
 
deps:
	rebar get-deps
 
generate:
	cd rel
	rm -rf bidder
	rebar generate

console:
	. rel/bidder/bin/bidder console

run:
	erl -pa ebin/ -pa apps/bidder/ebin/

