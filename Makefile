all: test

test: compile
	./rebar eunit

eunit: test

compile:
	./rebar compile

clean:
	./rebar clean

doc:
	./rebar doc

docs:
	./rebar doc

benchmark: compile
	@erl -noshell -pa ebin -eval 'hammy_bench:start().' -s init stop