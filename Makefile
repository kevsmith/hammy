all: test

test: compile
	./rebar eunit

eunit: test

compile:
	./rebar compile

clean:
	./rebar clean

benchmark: compile
	@erl -noshell -pa ebin -eval 'hammy_bench:start().' -s init stop