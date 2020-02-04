# Changelog

## [0.2.0](https://github.com/AdRoll/rebar3_format/tree/0.2.0) (2020-02-04)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.1.0...0.2.0)

**Fixed bugs:**

- Remove space before :: [\#61](https://github.com/AdRoll/rebar3_format/issues/61)

**Closed issues:**

- Consider using themes/styles instead of multiple config options [\#64](https://github.com/AdRoll/rebar3_format/issues/64)
- Move ribbon's default up to 90 or 95 [\#63](https://github.com/AdRoll/rebar3_format/issues/63)
- Break on receive's after [\#62](https://github.com/AdRoll/rebar3_format/issues/62)
- Consider splitting inline\_items into two options \(one for large binaries / lists\) [\#60](https://github.com/AdRoll/rebar3_format/issues/60)
- add a break after "try" for multiline try/catch blocks [\#59](https://github.com/AdRoll/rebar3_format/issues/59)
- Option to indent/inline all clauses consistently [\#51](https://github.com/AdRoll/rebar3_format/issues/51)

**Merged pull requests:**

- Format tag style [\#72](https://github.com/AdRoll/rebar3_format/pull/72) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#51: Option to avoid one-liners [\#71](https://github.com/AdRoll/rebar3_format/pull/71) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#60: Perfect inline\_items [\#70](https://github.com/AdRoll/rebar3_format/pull/70) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#59: Indent try body [\#69](https://github.com/AdRoll/rebar3_format/pull/69) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix default\_formatter to use \_our\_ defaults [\#68](https://github.com/AdRoll/rebar3_format/pull/68) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#60\] Move ribbon's default to 90 [\#67](https://github.com/AdRoll/rebar3_format/pull/67) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#64\] Implement rebar3\_formatter callback [\#66](https://github.com/AdRoll/rebar3_format/pull/66) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fixing github url [\#65](https://github.com/AdRoll/rebar3_format/pull/65) ([pbrudnick](https://github.com/pbrudnick))

## [0.1.0](https://github.com/AdRoll/rebar3_format/tree/0.1.0) (2020-01-21)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.0.3...0.1.0)

**Fixed bugs:**

- Remove trailing whitespaces [\#12](https://github.com/AdRoll/rebar3_format/issues/12)
- Don't format numbers written with sintactic sugar  [\#24](https://github.com/AdRoll/rebar3_format/issues/24)

**Closed issues:**

- Consider using erlang\_ls parser instead of epp\_dodger [\#34](https://github.com/AdRoll/rebar3_format/issues/34)
- Add a configuration option to control export lists indentation [\#33](https://github.com/AdRoll/rebar3_format/issues/33)
- Don't put more than one item per line in multi-line data structures [\#32](https://github.com/AdRoll/rebar3_format/issues/32)
- Module attribute grouping [\#30](https://github.com/AdRoll/rebar3_format/issues/30)
- The formatter adds parentheses arbitrarily [\#50](https://github.com/AdRoll/rebar3_format/issues/50)

**Merged pull requests:**

- Bump Version to 0.0.4 [\#49](https://github.com/AdRoll/rebar3_format/pull/49) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#30\] Add an option to avoid adding empty lines after attributes [\#47](https://github.com/AdRoll/rebar3_format/pull/47) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#32: don't put more one item per row [\#45](https://github.com/AdRoll/rebar3_format/pull/45) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#12\] Remove trailing whitespaces [\#44](https://github.com/AdRoll/rebar3_format/pull/44) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Verify the result of the formatting [\#43](https://github.com/AdRoll/rebar3_format/pull/43) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Temporarily remove lint and apply other fixes [\#42](https://github.com/AdRoll/rebar3_format/pull/42) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Recover the linting as a plugin, once rebar3\_lint uses katana\_code 0.2.1 [\#41](https://github.com/AdRoll/rebar3_format/pull/41) ([juanbono](https://github.com/juanbono))
- \[Fix \#50\] Remove superfluous parentheses from macros [\#57](https://github.com/AdRoll/rebar3_format/pull/57) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#24\] Don't reformat numbers written with syntactic sugar [\#52](https://github.com/AdRoll/rebar3_format/pull/52) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.3](https://github.com/AdRoll/rebar3_format/tree/0.0.3) (2019-12-12)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.0.2...0.0.3)

**Closed issues:**

- Consider using ktn\_dodger instead of epp\_dodger for parsing [\#25](https://github.com/AdRoll/rebar3_format/issues/25)

**Merged pull requests:**

- Bump version to 0.0.3 [\#40](https://github.com/AdRoll/rebar3_format/pull/40) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update README.md [\#38](https://github.com/AdRoll/rebar3_format/pull/38) ([msantos](https://github.com/msantos))
- \[\#25\] Replace epp\_dodger with ktn\_dodger [\#35](https://github.com/AdRoll/rebar3_format/pull/35) ([juanbono](https://github.com/juanbono))
- Add support for preserving empty lines [\#31](https://github.com/AdRoll/rebar3_format/pull/31) ([juanbono](https://github.com/juanbono))
- Some changes to the new script to fit in hex.pm [\#29](https://github.com/AdRoll/rebar3_format/pull/29) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.2](https://github.com/AdRoll/rebar3_format/tree/0.0.2) (2019-11-21)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.0.1...0.0.2)

**Fixed bugs:**

- Unicode support is not 100% bulletproof [\#11](https://github.com/AdRoll/rebar3_format/issues/11)

**Closed issues:**

- Publish the lib to hex.pm [\#9](https://github.com/AdRoll/rebar3_format/issues/9)
- Add CHANGELOG.md [\#8](https://github.com/AdRoll/rebar3_format/issues/8)
- Indent using whitespaces and tabs [\#7](https://github.com/AdRoll/rebar3_format/issues/7)
- Apply formatter to itself [\#2](https://github.com/AdRoll/rebar3_format/issues/2)

**Merged pull requests:**

- Fixes 2: Apply the formatter to its own code [\#18](https://github.com/AdRoll/rebar3_format/pull/18) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Ready to be published on hex.pm [\#17](https://github.com/AdRoll/rebar3_format/pull/17) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#8: Add CHANGELOG and bump version to 0.0.2 [\#28](https://github.com/AdRoll/rebar3_format/pull/28) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#7: Allow indentation with spaces only [\#27](https://github.com/AdRoll/rebar3_format/pull/27) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Allow configuration of indent sizes [\#26](https://github.com/AdRoll/rebar3_format/pull/26) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix 21: Complete coverage on rebar3\_prettypr [\#23](https://github.com/AdRoll/rebar3_format/pull/23) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Use CT for tests [\#20](https://github.com/AdRoll/rebar3_format/pull/20) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Format the code for rebar3\_format on make test [\#19](https://github.com/AdRoll/rebar3_format/pull/19) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add the project to TravisCI [\#16](https://github.com/AdRoll/rebar3_format/pull/16) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Remove hook support from rebar3\_prettypr [\#15](https://github.com/AdRoll/rebar3_format/pull/15) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.1](https://github.com/AdRoll/rebar3_format/tree/0.0.1) (2019-11-09)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/d9d137456067a952cbb83c4fbbe09948726c81be...0.0.1)

**Fixed bugs:**

- Fix make compile target [\#13](https://github.com/AdRoll/rebar3_format/issues/13)

**Merged pull requests:**

- \#13 Fix make compile target [\#14](https://github.com/AdRoll/rebar3_format/pull/14) ([diegomanuel](https://github.com/diegomanuel))
- use AdRoll dependency [\#4](https://github.com/AdRoll/rebar3_format/pull/4) ([juanbono](https://github.com/juanbono))
- Fix 52 ensure unicode works [\#3](https://github.com/AdRoll/rebar3_format/pull/3) ([juanbono](https://github.com/juanbono))
- Adjustments for rebar3\_prettypr [\#1](https://github.com/AdRoll/rebar3_format/pull/1) ([diegomanuel](https://github.com/diegomanuel))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
