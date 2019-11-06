.PHONY: test compile clean dialyzer lint

test: lint dialyzer
	@cd test_app && rebar3 format --output formatted/default
	@cd test_app && rebar3 as format_config format --output formatted/format_config
	@git --no-pager diff --no-index -- test_app/after test_app/formatted

compile:
	@rebar3 compile && cd test_app && rebar3 compile

clean:
	@rm -rf _build test_app/_build test_app/formatted

dialyzer:
	@rebar3 dialyzer

lint:
	@rebar3 lint
