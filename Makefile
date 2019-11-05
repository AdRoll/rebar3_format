.PHONY: test
REBAR?=./rebar3

test:
	@rsync -avq --exclude='after' --exclude='_build' --exclude='_checkouts' test_app/ test_app/after
	@cd test_app/after && $(REBAR) format

diff: test
	@git --no-pager diff --no-index -- test_app/src test_app/after/src

compile:
	@rebar3 compile && cd test_app && rebar3 compile

clean:
	@rm -rf _build test_app/_build test_app/after
