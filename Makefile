.PHONY: test run diff compile clean

test: run diff

run:
	@cd test_app && rebar3 format --output formatted

diff:
	@git --no-pager diff --no-index -- test_app/after test_app/formatted

compile:
	@rebar3 compile && cd test_app && rebar3 compile

clean:
	@rm -rf _build test_app/_build test_app/formatted
