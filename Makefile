.PHONY: compile clean doc eunit

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

dialyzer:
	dialyzer src/*.erl

doc:
	$(REBAR) doc

eunit:
	$(REBAR) eunit

speed:
	escript test-scripts/testspeed.escript
