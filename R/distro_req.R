#' Get system requirements from package name
#'
#' This function use RStudio's repository of system requirements: <https://github.com/rstudio/r-system-requirements>.
#' A local copy/clone of this repository must be available.
#'
#' @param reqs A `data.frame` from [get_package_reqs()] with free-form system requirements.
#' @param distro `[character]` Name of the Linux distribution.
#' @param system_req_folder The location of a local clone of "System Requirements for R Packages".
#'
#' @return A `data.frame` with columns "Rule" and "Package", where "Package" is the name of systems
#' package on `distro`.
#'
#' @export
distro_req <- function(reqs, distro, system_req_folder) {
    rules <- parse_rules(system_req_folder)
    distro_rules <- subset(rules, Distribution == distro)

    if (nrow(distro_rules) == 0) {
        possible_distros <- unique(rules$Distribution)
        possible_distros <- possible_distros[!is.na(possible_distros)]

        stop(
            "No rules for distribution '", distro, "'.\nPossible values are: ",
            paste(possible_distros, collapse = "; ")
        )
    }

    sysreq_match <- lapply(distro_rules$CollapsedPatterns, grepl, unique(reqs$SystemRequirements))
    sysreq_index <- which(vapply(sysreq_match, any, logical(1)))

    distro_rules[sysreq_index, c("Rule", "Package")]
}
