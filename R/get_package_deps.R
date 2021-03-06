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
    unique_deps <- unique(all_deps)

    base_packages_info <- utils::installed.packages(priority = "base")
    base_package_names <- rownames(base_packages_info)

    setdiff(unique_deps, base_package_names)
}
