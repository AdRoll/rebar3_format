# Contributing

Contributions are welcome, and they are greatly appreciated! Every little bit
helps, and credit will always be given.

You can contribute in many ways:

Types of Contributions
----------------------

### Report Bugs

Report bugs at https://github.com/AdRoll/rebar3_format/issues.

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting (OTP version, rebar3 version, sample code, etc).
* Detailed steps to reproduce the bug.

### Fix Bugs

Look through the GitHub issues for bugs. Anything tagged with "bug" and "help
wanted" is open to whoever wants to implement it.

### Implement Features

Look through the GitHub issues for features. Anything tagged with "enhancement"
and "help wanted" is open to whoever wants to implement it.

### Write Documentation

rebar3_format could always use more documentation, whether as part of the
official rebar3_format docs, in docstrings, or even on the web in blog posts,
articles, and such.

### Submit Feedback

The best way to send feedback is to file an issue at https://github.com/AdRoll/rebar3_format/issues.

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions are welcome :)

Get Started!
------------

Ready to contribute? Here's how to set up `rebar3_format` for local development.

1. Fork the `rebar3_format` repo on GitHub.

2. Clone your fork locally:

    $ git clone git@github.com:your_name_here/rebar3_format.git

3. Compile the project, assuming you rebar3 and OTP 21 installed, you can run:

    $ make compile

4. Create a branch for local development:

    $ git checkout -b name-of-your-bugfix-or-feature

   Now you can make your changes locally.

5. When you're done making changes, check that your changes pass the tests and also
   run `make test` to ensure that the test_app is formatted correctly and there is no
   dialyzer warnings to fix.
   In that case you should see no diffs from the `after/` directory.

   **Note:** If you add support for a new config or any modification that changes
   the way `rebar3_format` does the formatting you should also include the new formatted
   files inside `after/`.
 
6. Commit your changes and push your branch to GitHub:

    $ git add .
    $ git commit -m "Your detailed description of your changes."
    $ git push origin name-of-your-bugfix-or-feature

7. Submit a pull request through the GitHub website.

Pull Request Guidelines
-----------------------

Before you submit a pull request, check that it meets these guidelines:

1. The pull request should include tests.
2. If the pull request adds functionality, the docs and test_app files should be updated.
3. In case of changing the way we do the formatting update the test_app to include
   the modified file examples to test the changes. `make test` shouldn't detect any diff.
