#' Find system requirements
#'
#' Find all system requirements of a package and all of its dependencies.
#'
#' @param package_name `[character]` Name of the package(s)
#' @param verbose `[bool]` Print information along the way
#'
#' @return A `data.frame` with columns `Package` and `SystemRequirements` showing all dependencies
#' with system requirements.
#'
#' @export
get_package_reqs <- function(package_name, verbose = FALSE) {
    package_deps <- get_package_deps(package_name = package_name, verbose = verbose)
    all_packages_to_check <- c(package_name, package_deps)

    if (isTRUE(verbose))
        message("Finding system requirements")

    db <- get_package_db()
    packages_with_sysreqs <- db[!is.na(db$SystemRequirements), c("Package", "SystemRequirements")]

    packages_with_sysreqs[packages_with_sysreqs$Package %in% all_packages_to_check, ]
}
