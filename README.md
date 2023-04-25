<!-- badges: start -->

[![R-CMD-check](https://github.com/jhudsl/ottrpal/workflows/R-CMD-check/badge.svg)](https://github.com/jhudsl/ottrpal/actions) [![CRAN status](https://www.r-pkg.org/badges/version/ottrpal)](https://CRAN.R-project.org/package=ottrpal) [![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ottrpal)](https://cran.r-project.org/package=ottrpal) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) <!-- [![GitHub release (latest by --> <!-- date)](https://img.shields.io/github/v/release/jhudsl/ottrpal?style=social)](https://github.com/jhudsl/ottrpal/releases/tag/v1.0.0) --> <!-- [![Codecov test --> <!-- coverage](https://codecov.io/gh/jhudsl/ottrpal/branch/main/graph/badge.svg)](https://codecov.io/gh/jhudsl/ottrpal?branch=main) -->

<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgoogleclassroom 

`rgoogleclassroom` is a Google API wrapper that allows you to use Google Classroom and Google Forms from the comfort of R. 

You can [read the rgoogleclassroom package documentation here](https://datatrail-jhu.github.io/rgoogleclassroom/docs/index.html).

To use this package you need to have a GOogle Classroom account. Go here to get that: https://edu.google.com/workspace-for-education/classroom/

## How to install

If you want the development version (not advised) you can install using the `remotes` package to install from GitHub. 
``` r
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("datatrail-jhu/rgoogleclassroom")
```

## Usage 

To start, you need to `authorize()` the package to access your files. Select all the scopes you feel comfortable sharing. Note that you need to select certain scopes for certain functions to work. 

```r
authorize()
```

### Basics 

There are different objects on the Google API: 

- courses 
- forms 
- courseWorks 
- courseWork Materials 
- topics

Most of these objects has functions that do the following: 

- `get_<object>_list()` 
- `create_<object>()`
- `get_<object>_properties()`

For example: 

`get_course_list(owner_id)` retrieves a list of courses for a particular owner id. 
`create_course()` creates a course
`get_course_properties(course_id)` retrieves the properties of a course given its id. 

These can be built together to be pretty nifty.

## Example workflow

```r
authorize() 
```

```r
owner_id <- get_owner_id()
```

```r
course_df <- get_course_list(owner_id)
```
