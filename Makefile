all: test

test: compile
	./rebar eunit

eunit: test

compile:
	./rebar compile

clean:
	./rebar clean