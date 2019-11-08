.PHONY: test compile clean dialyzer lint doc

test: lint dialyzer
	@rm -rf test_app/formatted
	@cd test_app && rebar3 format --output formatted
	@git --no-pager diff --no-index -- test_app/after test_app/formatted

compile:
	@rebar3 compile && cd test_app && rebar3 compile

clean:
	@rm -rf doc _build test_app/_build test_app/formatted

dialyzer:
	@rebar3 dialyzer || rebar3 dialyzer

lint:
	@rebar3 lint

doc:
	@rebar3 edoc
