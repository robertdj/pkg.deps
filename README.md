pkg.deps
========

<!-- badges: start -->
[![R build status](https://github.com/robertdj/pkg.deps/workflows/R-CMD-check/badge.svg)](https://github.com/robertdj/pkg.deps/actions)
<!-- badges: end -->

R packages depend on other R packages.
On Linux, R packages can also depend on system requirements.
As an example, the [{curl} package](https://cran.r-project.org/package=curl) has this field in its `DESCRIPTION` file and on its CRAN page:

> SystemRequirements: libcurl: libcurl-devel (rpm) or libcurl4-openssl-dev (deb).

When installing a lot of packages (or packages with lots of dependencies) it can be time consuming to check these requirements manually.
Running `install.packages`, note which packages fail and look up their dependencies can be very time consuming -- in particular when making Docker images.

The goal of {pkg.deps} is to help find system requirements for packages and their dependencies.
I consider two scenarios:

- I want to install a (new) package and get the free-form `SystemRequirements` of all dependencies.
- I want to translate the free-form entries in `SystemRequirements` to distribution-specific libraries.

The second scenario is relevant because some packages use generic descriptions of their requirements. 
As an example, the [{data.table} package](https://cran.r-project.org/package=data.table) notes the system requirement `zlib` which on Ubuntu is `zlib1g-dev` and on Red Hat is `zlib-devel`.
To alleviate this RStudio has a repo with [System Requirements for R Packages](https://github.com/rstudio/r-system-requirements).
I utilize this in a low-tech manner where {pkg.deps} consult the rules in local clone.


## Installation

The package is only available on GitHub and can be installed with the [remotes package](https://remotes.r-lib.org) using this command:

``` r
remotes::install_github("robertdj/pkg.deps")
```

## Example

There are three functions in {pkg.deps}.
The first function simply returns all non-base dependencies:

``` r
> pkg.deps::get_package_deps("httr")
[1] "curl"     "jsonlite" "mime"     "openssl"  "R6"       "askpass"  "sys"
```

The second returns all dependencies with system requirements in free-form, that is, as they appear in their `DESCRIPTION` files:

``` r
> pkg.deps::get_package_reqs("httr")
Package                                           SystemRequirements
curl    libcurl: libcurl-devel (rpm) or\nlibcurl4-openssl-dev (deb).
openssl                                             OpenSSL >= 1.0.1
```

The output is a subset of a dataframe, so it also includes row numbers.
The final function converts the output from `get_package_reqs` to distribution specific libraries.
For Ubuntu we get this output:

``` r
> deps <- pkg.deps::get_package_reqs("httr")
> pkg.deps::distro_req(deps, "ubuntu", "/path/to/r-system-requirements")
Rule                 Package
libcurl libcurl4-openssl-dev
openssl           libssl-dev
```

The last argument of `distro_req` is the path to a local copy/clone of System Requirements for R Packages.
I am not trying anything fancy to help with `r-system-requirements` -- `distro_req` simply fails if the JSON files with rules are not in the expected location.
