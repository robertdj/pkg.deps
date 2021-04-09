#' Find package dependencies
#'
#' Find all non-base packages that a package depends on.
#'
#' @param package_name `[character]` Name of the package(s).
#' @param verbose `[bool]` Print information along the way.
#'
#' @return A `character` vector with names of dependencies -- minus base packages.
#'
#' @export
get_package_deps <- function(package_name, verbose = FALSE) {
    db <- get_package_db(verbose = verbose)

    if (isTRUE(verbose))
        message("Finding package depedencies")

    package_deps <- tools::package_dependencies(
        packages = package_name, db = db, which = c("Depends", "Imports"), recursive = TRUE
    )

    all_deps <- unlist(package_deps)
    remove_base_packages(all_deps)
}


#' Remove base packages
#'
#' It is common to have base R packages dependencies, but when trying to install a base package in
#' RStudio you'll get complaints about the package already being loaded.
#' This function removes the base R packages from the input (if any).
#'
#' @inheritParams get_package_deps
#'
#' @return The unique entries in `package_name` without base packages (if any).
#'
#' @export
remove_base_packages <- function(package_name)
{
    base_packages_info <- utils::installed.packages(priority = "base")

    setdiff(unique(package_name), base_packages_info[, "Package"])
}
