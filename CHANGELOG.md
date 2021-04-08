# Changelog

## [1.0.1](https://github.com/AdRoll/rebar3_format/tree/1.0.1) (2021-04-08)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/1.0.0...1.0.1)

**Fixed bugs:**

- Strings in attributes crash the formatter [\#239](https://github.com/AdRoll/rebar3_format/issues/239)

**Merged pull requests:**

- \[\#239\] Fix \#239: Attributes with strings [\#241](https://github.com/AdRoll/rebar3_format/pull/241) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.0.0](https://github.com/AdRoll/rebar3_format/tree/1.0.0) (2021-04-07)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.10.1...1.0.0)

**Fixed bugs:**

- Tabs in strings break the formatter [\#236](https://github.com/AdRoll/rebar3_format/issues/236)

**Closed issues:**

- Support for @format in comments [\#233](https://github.com/AdRoll/rebar3_format/issues/233)
- We need to reorganize tests [\#197](https://github.com/AdRoll/rebar3_format/issues/197)
- Add CLI [\#37](https://github.com/AdRoll/rebar3_format/issues/37)
- Complete Coverage [\#21](https://github.com/AdRoll/rebar3_format/issues/21)

**Merged pull requests:**

- Improve test coverage [\#235](https://github.com/AdRoll/rebar3_format/pull/235) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#197\] Fix \#197: Organize Tests [\#238](https://github.com/AdRoll/rebar3_format/pull/238) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Rename master branch as main [\#237](https://github.com/AdRoll/rebar3_format/pull/237) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#233\] Fix \#233: Allow format options in comment form [\#234](https://github.com/AdRoll/rebar3_format/pull/234) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.10.1](https://github.com/AdRoll/rebar3_format/tree/0.10.1) (2021-02-19)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.10.0...0.10.1)

**Fixed bugs:**

- Macros in module qualifiers within types [\#231](https://github.com/AdRoll/rebar3_format/issues/231)

**Merged pull requests:**

- \[Fix \#231\] Handle macros in specs and types [\#232](https://github.com/AdRoll/rebar3_format/pull/232) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.10.0](https://github.com/AdRoll/rebar3_format/tree/0.10.0) (2021-02-18)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.9.0...0.10.0)

**Closed issues:**

- Support formatting rebar.config [\#86](https://github.com/AdRoll/rebar3_format/issues/86)

**Merged pull requests:**

- \[\#86\] Add support for non-module files [\#230](https://github.com/AdRoll/rebar3_format/pull/230) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.9.0](https://github.com/AdRoll/rebar3_format/tree/0.9.0) (2021-02-10)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.8.2...0.9.0)

**Fixed bugs:**

- inline\_simple\_funs overrides inline\_clause\_bodies [\#223](https://github.com/AdRoll/rebar3_format/issues/223)

**Closed issues:**

- Remove parentheses on simple attributes [\#227](https://github.com/AdRoll/rebar3_format/issues/227)
- Don't put every infix operator in columns [\#219](https://github.com/AdRoll/rebar3_format/issues/219)
- Don't rewrite strings [\#55](https://github.com/AdRoll/rebar3_format/issues/55)

**Merged pull requests:**

- Fix \#227: Don't put parentheses around 'ignore' [\#228](https://github.com/AdRoll/rebar3_format/pull/228) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#223\] Don't let inline\_simple\_funs affect inline\_clause\_bodies [\#226](https://github.com/AdRoll/rebar3_format/pull/226) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#55\] Properly handle contiguous strings… [\#225](https://github.com/AdRoll/rebar3_format/pull/225) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Kill dead code with fire 🔥 [\#224](https://github.com/AdRoll/rebar3_format/pull/224) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#219\] Don't indent right after an operator [\#221](https://github.com/AdRoll/rebar3_format/pull/221) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Upgrade katana-code [\#220](https://github.com/AdRoll/rebar3_format/pull/220) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.8.2](https://github.com/AdRoll/rebar3_format/tree/0.8.2) (2020-11-30)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.8.1...0.8.2)

## [0.8.1](https://github.com/AdRoll/rebar3_format/tree/0.8.1) (2020-11-19)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.8.0...0.8.1)

**Fixed bugs:**

- We removed too many parentheses [\#74](https://github.com/AdRoll/rebar3_format/issues/74)

**Closed issues:**

- Add an option to parse macro definitions [\#217](https://github.com/AdRoll/rebar3_format/issues/217)

**Merged pull requests:**

- \[\#217\] Fix \#217: Put no-macro-parsing behind an option [\#218](https://github.com/AdRoll/rebar3_format/pull/218) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.8.0](https://github.com/AdRoll/rebar3_format/tree/0.8.0) (2020-11-19)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.6.0...0.8.0)

**Fixed bugs:**

- Name/Arity changed to tuple in compile inline directive [\#210](https://github.com/AdRoll/rebar3_format/issues/210)
- Strange indentation of andalso guards [\#209](https://github.com/AdRoll/rebar3_format/issues/209)
- Jumpy Parentheses vs. Formatting Idempotence [\#200](https://github.com/AdRoll/rebar3_format/issues/200)
- Parsing error with parametrized macro [\#84](https://github.com/AdRoll/rebar3_format/issues/84)
- macro with ??  gives syntax error [\#53](https://github.com/AdRoll/rebar3_format/issues/53)
- ktn\_dodger can't parse macros in specs [\#48](https://github.com/AdRoll/rebar3_format/issues/48)
- Can't have concatenated string macros [\#22](https://github.com/AdRoll/rebar3_format/issues/22)

**Closed issues:**

- -inline should also accept arity qualifiers [\#201](https://github.com/AdRoll/rebar3_format/issues/201)
- Accept concatenated record access [\#198](https://github.com/AdRoll/rebar3_format/issues/198)
- Reduce indentation of multiline `andalso` [\#185](https://github.com/AdRoll/rebar3_format/issues/185)
- Formatting multiline maps [\#162](https://github.com/AdRoll/rebar3_format/issues/162)
- Style in record declaration [\#159](https://github.com/AdRoll/rebar3_format/issues/159)
- Spacing around parentheses / curly-braces [\#148](https://github.com/AdRoll/rebar3_format/issues/148)
- Formatting escript produces badarg [\#118](https://github.com/AdRoll/rebar3_format/issues/118)

**Merged pull requests:**

- Fix \#118: Parse escripts [\#215](https://github.com/AdRoll/rebar3_format/pull/215) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#74 by upgrading katana-code [\#214](https://github.com/AdRoll/rebar3_format/pull/214) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#185\] Place infix operators in columns [\#213](https://github.com/AdRoll/rebar3_format/pull/213) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#210\] Actually use arity qualifiers for inlining directives. [\#212](https://github.com/AdRoll/rebar3_format/pull/212) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Don't crash on weird macros [\#207](https://github.com/AdRoll/rebar3_format/pull/207) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#148\] Add options to space out parentheses and curly braces [\#206](https://github.com/AdRoll/rebar3_format/pull/206) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#200\] Fix arrangment of comments at the first paramter of an application [\#205](https://github.com/AdRoll/rebar3_format/pull/205) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#162\] Place \(by default\) each map/record field in its own line [\#204](https://github.com/AdRoll/rebar3_format/pull/204) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#198\] Allow A\#record.with\_another\#record.in\_it [\#203](https://github.com/AdRoll/rebar3_format/pull/203) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#201\] \[Fix \#201\] Arity qualifiers for -inline [\#202](https://github.com/AdRoll/rebar3_format/pull/202) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.6.0](https://github.com/AdRoll/rebar3_format/tree/0.6.0) (2020-10-26)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.5.0...0.6.0)

**Fixed bugs:**

- `when`, under `spec` or `callback` getting indented by 5 spaces [\#189](https://github.com/AdRoll/rebar3_format/issues/189)
- Unexpected blank line after %% @private [\#165](https://github.com/AdRoll/rebar3_format/issues/165)

**Closed issues:**

- parenthesize\_infix\_operations' aggressiveness [\#191](https://github.com/AdRoll/rebar3_format/issues/191)
- Keep extra space after `fun`, for anonymous functions [\#187](https://github.com/AdRoll/rebar3_format/issues/187)
- `of` in `case` doesn't get formatted to next line [\#186](https://github.com/AdRoll/rebar3_format/issues/186)
- Allow types to be indented after :: [\#184](https://github.com/AdRoll/rebar3_format/issues/184)
- 'after' is indented too much on 'receive' [\#183](https://github.com/AdRoll/rebar3_format/issues/183)
- Single-character variable assigment [\#168](https://github.com/AdRoll/rebar3_format/issues/168)
- Add a new config option to remove empty lines after flow control attributes. [\#157](https://github.com/AdRoll/rebar3_format/issues/157)

**Merged pull requests:**

- \[Fix \#166\] Recover sub\_indent [\#196](https://github.com/AdRoll/rebar3_format/pull/196) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#190\] Force indentation on spec's when [\#195](https://github.com/AdRoll/rebar3_format/pull/195) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#184, \#189\] Improve long spec indentation [\#194](https://github.com/AdRoll/rebar3_format/pull/194) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#183\] Properly indent after on receive [\#193](https://github.com/AdRoll/rebar3_format/pull/193) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#168\] Don't indent on underscores either [\#192](https://github.com/AdRoll/rebar3_format/pull/192) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.5.0](https://github.com/AdRoll/rebar3_format/tree/0.5.0) (2020-10-06)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.4.0...0.5.0)

**Implemented enhancements:**

- New release of erlfmt \(v0.8.0\) [\#180](https://github.com/AdRoll/rebar3_format/issues/180)
- New release of erlfmt [\#149](https://github.com/AdRoll/rebar3_format/issues/149)

**Closed issues:**

- Formatting multiline lists [\#161](https://github.com/AdRoll/rebar3_format/issues/161)
- Blank line before `export\_type` [\#160](https://github.com/AdRoll/rebar3_format/issues/160)
- rebar3\_format skips meaningful parentheses [\#137](https://github.com/AdRoll/rebar3_format/issues/137)
- Make compatible with erlfmt 0.6.0 [\#135](https://github.com/AdRoll/rebar3_format/issues/135)
- rebar3 project\_plugins \> plugins [\#173](https://github.com/AdRoll/rebar3_format/issues/173)
- ignore\_xref's content style [\#158](https://github.com/AdRoll/rebar3_format/issues/158)
- Long specs with when [\#150](https://github.com/AdRoll/rebar3_format/issues/150)
- Reduce indentation on after [\#147](https://github.com/AdRoll/rebar3_format/issues/147)
- Report the results of --verify in a non-erlang-ish way [\#145](https://github.com/AdRoll/rebar3_format/issues/145)
- Allow --files to receive a list of wildcards instead of just one [\#144](https://github.com/AdRoll/rebar3_format/issues/144)
- Prevent right part of a multiline assignation to go in a second line in certain conditions [\#143](https://github.com/AdRoll/rebar3_format/issues/143)
- Indent one-claused anonymous functions less severely [\#142](https://github.com/AdRoll/rebar3_format/issues/142)
- Format -dialyzer with name/arity [\#141](https://github.com/AdRoll/rebar3_format/issues/141)
- Indent on function composition [\#140](https://github.com/AdRoll/rebar3_format/issues/140)
- Parentheses around infix operations [\#139](https://github.com/AdRoll/rebar3_format/issues/139)
- Opening and closing bracket position [\#5](https://github.com/AdRoll/rebar3_format/issues/5)

**Merged pull requests:**

- Bump version to 0.5.0 [\#182](https://github.com/AdRoll/rebar3_format/pull/182) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Create CODE\_OF\_CONDUCT.md [\#138](https://github.com/AdRoll/rebar3_format/pull/138) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#135\] Compatibilize with erlfmt 0.6.0 and 0.5.1 [\#136](https://github.com/AdRoll/rebar3_format/pull/136) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#180: erlfmt 0.8.0 [\#181](https://github.com/AdRoll/rebar3_format/pull/181) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[fix \#144\] handle more than 1 wildcard [\#179](https://github.com/AdRoll/rebar3_format/pull/179) ([juanbono](https://github.com/juanbono))
- \[Fix \#145\] Report the results of --verify in a non-erlang-ish way [\#178](https://github.com/AdRoll/rebar3_format/pull/178) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#149\] Add support for erlfmt 0.7.0 [\#177](https://github.com/AdRoll/rebar3_format/pull/177) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#139\] parentheses around infix operators [\#176](https://github.com/AdRoll/rebar3_format/pull/176) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#168\] Avoid indeting match expressions if it's just a short var [\#175](https://github.com/AdRoll/rebar3_format/pull/175) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#173\] Use/recommend project\_plugins, instead of plugins [\#174](https://github.com/AdRoll/rebar3_format/pull/174) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Put \#140 / \#153 behind a flag [\#172](https://github.com/AdRoll/rebar3_format/pull/172) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#163\] Indent case…of like try…of [\#171](https://github.com/AdRoll/rebar3_format/pull/171) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Indent long specs on when [\#170](https://github.com/AdRoll/rebar3_format/pull/170) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#147\] Reduce indentation for after clauses [\#169](https://github.com/AdRoll/rebar3_format/pull/169) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Inline simple funs even more aggressively [\#156](https://github.com/AdRoll/rebar3_format/pull/156) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#141\] Format -dialyzer properly [\#155](https://github.com/AdRoll/rebar3_format/pull/155) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#142\] Remove space in single-clause anonymous function defintions [\#154](https://github.com/AdRoll/rebar3_format/pull/154) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#140\] Indent on function composition [\#153](https://github.com/AdRoll/rebar3_format/pull/153) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add helper scripts for git hooks [\#146](https://github.com/AdRoll/rebar3_format/pull/146) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.4.0](https://github.com/AdRoll/rebar3_format/tree/0.4.0) (2020-08-19)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.3.2...0.4.0)

**Fixed bugs:**

- Fix compatibility issues with erlfmt [\#133](https://github.com/AdRoll/rebar3_format/pull/133) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Consider removing sub\_indent altogether from the default formatter [\#131](https://github.com/AdRoll/rebar3_format/issues/131)
- Ensure compatibility with latest erlfmt [\#128](https://github.com/AdRoll/rebar3_format/issues/128)
- Remove extra spaces in statements [\#127](https://github.com/AdRoll/rebar3_format/issues/127)
- Adjust for OTP23 [\#105](https://github.com/AdRoll/rebar3_format/issues/105)

**Merged pull requests:**

- Remove sub\_indent altogether from the default\_formatter [\#134](https://github.com/AdRoll/rebar3_format/pull/134) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#127\] Extra spaces in binary comprehensions [\#132](https://github.com/AdRoll/rebar3_format/pull/132) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#105\] Adjust to OTP23 [\#130](https://github.com/AdRoll/rebar3_format/pull/130) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add support for new features in erlfmt v0.5.1 [\#129](https://github.com/AdRoll/rebar3_format/pull/129) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.3.2](https://github.com/AdRoll/rebar3_format/tree/0.3.2) (2020-07-13)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.3.1...0.3.2)

**Closed issues:**

- Improve steamroller integration [\#117](https://github.com/AdRoll/rebar3_format/issues/117)
- Formatting specs with 'when' keyword [\#90](https://github.com/AdRoll/rebar3_format/issues/90)
- Indent long lines on = [\#75](https://github.com/AdRoll/rebar3_format/issues/75)
- Don't unquote atoms [\#56](https://github.com/AdRoll/rebar3_format/issues/56)

**Merged pull requests:**

- Add link to VS Code extension [\#119](https://github.com/AdRoll/rebar3_format/pull/119) ([szTheory](https://github.com/szTheory))
- \[Fix \#75\] Indent long lines on = [\#126](https://github.com/AdRoll/rebar3_format/pull/126) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#90\] Indent specs on -\> [\#125](https://github.com/AdRoll/rebar3_format/pull/125) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Support erlfmt 0.3.0 [\#124](https://github.com/AdRoll/rebar3_format/pull/124) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add support for erlfmt 0.2.0 [\#123](https://github.com/AdRoll/rebar3_format/pull/123) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#55\] Some initial steps [\#122](https://github.com/AdRoll/rebar3_format/pull/122) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#56\] Handle quoted atoms [\#121](https://github.com/AdRoll/rebar3_format/pull/121) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#117\] Full steamroller integration [\#120](https://github.com/AdRoll/rebar3_format/pull/120) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.3.1](https://github.com/AdRoll/rebar3_format/tree/0.3.1) (2020-06-16)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.3.0...0.3.1)

**Fixed bugs:**

- Defining a type called `error\(\)` leads to `modified\_ast` error in some cases [\#108](https://github.com/AdRoll/rebar3_format/issues/108)

**Closed issues:**

- Integrate with other formatters [\#110](https://github.com/AdRoll/rebar3_format/issues/110)

**Merged pull requests:**

- Add badges to README [\#113](https://github.com/AdRoll/rebar3_format/pull/113) ([juanbono](https://github.com/juanbono))
- \[\#110\] A little refactor to simplify addition of new external formatters [\#111](https://github.com/AdRoll/rebar3_format/pull/111) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#110\] Refactor rebar3\_formatter behavior [\#116](https://github.com/AdRoll/rebar3_format/pull/116) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#110\] Allow users to use erlfmt from within rebar3 format [\#115](https://github.com/AdRoll/rebar3_format/pull/115) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#110\] Prepare for external formatters a bit more [\#114](https://github.com/AdRoll/rebar3_format/pull/114) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#110\] Add Steamroller integration [\#112](https://github.com/AdRoll/rebar3_format/pull/112) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix modified\_ast issue [\#109](https://github.com/AdRoll/rebar3_format/pull/109) ([juanbono](https://github.com/juanbono))

## [0.3.0](https://github.com/AdRoll/rebar3_format/tree/0.3.0) (2020-05-27)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.2.3...0.3.0)

**Fixed bugs:**

- Different representations of characters should be preserved [\#93](https://github.com/AdRoll/rebar3_format/issues/93)
- Output -on\_load\(Fun/Arity\) instead of -on\_load\({Fun, Arity}\) [\#89](https://github.com/AdRoll/rebar3_format/issues/89)
- List of exported functions and types are inlined despite the default {when\_over, 25} setting [\#88](https://github.com/AdRoll/rebar3_format/issues/88)
- Preservation of empty lines is broken [\#83](https://github.com/AdRoll/rebar3_format/issues/83)
- Can't use macros for integer ranges in type specs [\#58](https://github.com/AdRoll/rebar3_format/issues/58)
- Can't have macros with arguments and ; [\#10](https://github.com/AdRoll/rebar3_format/issues/10)

**Closed issues:**

- Exclude certain files from formatting [\#87](https://github.com/AdRoll/rebar3_format/issues/87)

**Merged pull requests:**

- Show line number of unparsable file [\#102](https://github.com/AdRoll/rebar3_format/pull/102) ([sebastiw](https://github.com/sebastiw))
- \[\#88\] Clarify description of  in README.md [\#101](https://github.com/AdRoll/rebar3_format/pull/101) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#89: on\_load [\#100](https://github.com/AdRoll/rebar3_format/pull/100) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#88\] Add inline\_attributes as a separate flag to inline\_items [\#106](https://github.com/AdRoll/rebar3_format/pull/106) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#93: Preserve char representations [\#104](https://github.com/AdRoll/rebar3_format/pull/104) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#58: Properly dodge and undodge macros on specs and types [\#103](https://github.com/AdRoll/rebar3_format/pull/103) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add support for file ignoring [\#91](https://github.com/AdRoll/rebar3_format/pull/91) ([juanbono](https://github.com/juanbono))

## [0.2.3](https://github.com/AdRoll/rebar3_format/tree/0.2.3) (2020-04-14)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.2.2...0.2.3)

**Merged pull requests:**

- Fix \#92: Verify should work even when adding new lines [\#99](https://github.com/AdRoll/rebar3_format/pull/99) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.2](https://github.com/AdRoll/rebar3_format/tree/0.2.2) (2020-04-09)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.2.1...0.2.2)

**Fixed bugs:**

- Redundant trailing newline added [\#82](https://github.com/AdRoll/rebar3_format/issues/82)

**Closed issues:**

- Verify or ci mode [\#92](https://github.com/AdRoll/rebar3_format/issues/92)
- Error parsing files: undef [\#80](https://github.com/AdRoll/rebar3_format/issues/80)
- Parser tries to format .xrl file [\#78](https://github.com/AdRoll/rebar3_format/issues/78)
- Error parsing files: undef \(ktn\_dodger:parse\_file\) [\#76](https://github.com/AdRoll/rebar3_format/issues/76)
- Do not require redbug for production usage [\#73](https://github.com/AdRoll/rebar3_format/issues/73)

**Merged pull requests:**

- Bump Version to 0.2.2 [\#98](https://github.com/AdRoll/rebar3_format/pull/98) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#83: Fix preservation of empty lines [\#95](https://github.com/AdRoll/rebar3_format/pull/95) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Handle line endings properly [\#85](https://github.com/AdRoll/rebar3_format/pull/85) ([maciej-szlosarczyk](https://github.com/maciej-szlosarczyk))
- Update default wildcard to only .erl/.hrl files [\#79](https://github.com/AdRoll/rebar3_format/pull/79) ([hengestone](https://github.com/hengestone))
- Fix \#73: Remove redbug from deps [\#97](https://github.com/AdRoll/rebar3_format/pull/97) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#92: Add --verify [\#96](https://github.com/AdRoll/rebar3_format/pull/96) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.1](https://github.com/AdRoll/rebar3_format/tree/0.2.1) (2020-02-04)

[Full Changelog](https://github.com/AdRoll/rebar3_format/compare/0.1.0...0.2.1)

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
