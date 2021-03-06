
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkg.deps

<!-- badges: start -->

[![R build
status](https://github.com/robertdj/pkg.deps/workflows/R-CMD-check/badge.svg)](https://github.com/robertdj/pkg.deps/actions)
<!-- badges: end -->

R packages depend on other R packages. On Linux, R packages can also
depend on system requirements. As an example, the [{curl}
package](https://cran.r-project.org/package=curl) has this field in its
`DESCRIPTION` file and on its CRAN page:

> SystemRequirements: libcurl: libcurl-devel (rpm) or
> libcurl4-openssl-dev (deb).

When installing a lot of packages (or packages with lots of
dependencies) it can be time consuming to check these requirements
manually. Running `install.packages`, note which packages fail and look
up their dependencies can be very time consuming – in particular when
making Docker images.

The goal of {pkg.deps} is to help find system requirements for packages
and their dependencies. I consider two scenarios:

-   I want to install a (new) package and get the free-form
    `SystemRequirements` of all dependencies.
-   I want to translate the free-form entries in `SystemRequirements` to
    distribution-specific libraries.

The second scenario is relevant because some packages use generic
descriptions of their requirements. As an example, the [{data.table}
package](https://cran.r-project.org/package=data.table) notes the system
requirement `zlib` which on Ubuntu is `zlib1g-dev` and on Red Hat is
`zlib-devel`. To alleviate this RStudio has a repo with [System
Requirements for R
Packages](https://github.com/rstudio/r-system-requirements). I utilize
this in a low-tech manner where {pkg.deps} consult the rules in local
clone.

## Installation

The package is only available on GitHub and can be installed with the
[remotes package](https://remotes.r-lib.org) using this command:

``` r
remotes::install_github("robertdj/pkg.deps")
```

## Example 1

There are three functions in {pkg.deps}. The first function simply
returns all non-base dependencies:

``` r
pkg.deps::get_package_deps("httr")
#> [1] "curl"     "jsonlite" "mime"     "openssl"  "R6"       "askpass"  "sys"
```

The second returns all dependencies with system requirements in
free-form, that is, as they appear in their `DESCRIPTION` files:

``` r
pkg.deps::get_package_reqs("httr")
#>       Package                                           SystemRequirements
#> 2903     curl libcurl: libcurl-devel (rpm) or\nlibcurl4-openssl-dev (deb).
#> 10435 openssl                                             OpenSSL >= 1.0.1
```

The output is a subset of a dataframe, so it also includes row numbers.
The final function converts the output from `get_package_reqs` to
distribution specific libraries. It needs the path to a local copy/clone
of System Requirements for R Packages, which in this example is
`../r-system-requirements`.

For Ubuntu we get this output:

``` r
reqs <- pkg.deps::get_package_reqs("httr")
pkg.deps::distro_req(reqs, "ubuntu", "../r-system-requirements")
#>        Rule              Package
#> 283 libcurl libcurl4-openssl-dev
#> 489 openssl           libssl-dev
```

Compare this with the output for Redhat:

``` r
pkg.deps::distro_req(reqs, "redhat", "../r-system-requirements")
#>        Rule       Package
#> 286 libcurl libcurl-devel
#> 492 openssl openssl-devel
```

I am not trying anything fancy to help with `r-system-requirements` –
`distro_req` simply fails if the JSON files with rules are not in the
expected location.

# Example 2

In an R(Studio) project that is not a package there is no base R way of
keeping track of dependencies. The [{renv}
package](https://cran.r-project.org/package=renv) can help out with a
formal tracking and it has a function for *discovering* dependencies:

``` r
deps <- renv::dependencies()
#> Finding R package dependencies ... Done!
```

Here I am running it in the {pkg.deps} package project itself and the
dependencies are

``` r
unique(deps$Package)
#> [1] "jsonlite"  "tools"     "utils"     "testthat"  "rmarkdown" "knitr"    
#> [7] "pkg.deps"  "renv"
```

This vector can be used in `get_package_deps` as in the example above. A
final function from {pkg.deps} offers to remove the base R packages so
that the result is ready for `install.packages`:

``` r
pkg.deps::remove_base_packages(deps$Package)
#> [1] "jsonlite"  "testthat"  "rmarkdown" "knitr"     "pkg.deps"  "renv"
```
