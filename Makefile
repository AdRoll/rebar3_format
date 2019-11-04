.PHONY: test

test:
	@rsync -avq --exclude='after' --exclude='_build' --exclude='_checkouts' test_app/ test_app/after
	@cd test_app/after && rebar3 format

diff: test
	@git --no-pager diff --no-index -- test_app/src test_app/after/src

clean:
	@rm -rf _build test_app/_build test_app/after