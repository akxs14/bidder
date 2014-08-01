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
 
rel: deps compile generate reto_server-rel

run:
	erl -pa ebin deps/*/ -s reto_launch