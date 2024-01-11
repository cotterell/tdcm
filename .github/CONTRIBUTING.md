# Contributing to tdcm

This document outlines how to propose a change to **tdcm**.

## Bug Reports and Feature Requests

If you have encountered a problem with the **tdcm** package or have an idea for 
a new feature, then please submit it using 
[the project's issue tracker](`r tdcm_issue_tracker_url`).

## Pull Requests

* We recommend that you create a Git branch for each pull request (PR).
* Look at the Github Actions build status before and after making changes. The 
  `README` should contain badges for any continuous integration services used by
  the package.
* New code should follow a consistent style. 
  You can use the [styler](https://CRAN.R-project.org/package=styler) package to
  apply a default style, but please don't restyle code that has nothing to do
  with your PR.  
* We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html), 
  for documentation.  
* We use [testthat](https://cran.r-project.org/package=testthat). Contributions
  with test cases included are easier to accept.  
* For user-facing changes, add a bullet to the top of `NEWS.md` below the
  current development version header describing the changes made followed by 
  your GitHub username, and links to relevant issue(s)/PR(s).

## Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

## Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

## Code of Conduct

Please note that the pkgdown project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
