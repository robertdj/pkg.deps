#' Parse rules in JSON files
#'
#' Parse system requirements in JSON format.
#'
#' @inheritParams distro_req
#'
#' @seealso distro_req
#'
#' @return A `data.frame` with rules info in these columns:
#'
#' - "Rule": Name of the rule/JSON file.
#' - "Packages": Name of the system package.
#' - "OS": The operating system (Linux or Windows).
#' - "Distribution": The Linux distribution. `NA` if `OS` is "Windows".
#' - "Patterns": Regular expression pattern to match `Package`.
#' - "CollapsedPatterns": If there are multiple entries in `Patterns`, they are here collapsed into
#' a single pattern to work with [grepl()].
parse_rules <- function(system_req_folder) {
    # if (!dir.exists(system_req_folder))

    sysreq_rules_folder <- file.path(system_req_folder, "rules")
    files_with_rules <- list.files(path = sysreq_rules_folder, pattern = "*.json$", full.names = TRUE)

    if (length(files_with_rules) == 0)
        stop("There are no JSON files with rules in", system_req_folder)

    rules <- lapply(files_with_rules, jsonlite::fromJSON, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    names(rules) <- tools::file_path_sans_ext(basename(files_with_rules))

    rectangular_rules <- lapply(rules, rectangularize_single_rule)
    rules_df <- do.call(rbind, c(rectangular_rules, make.row.names = FALSE))

    rules_length <- vapply(rectangular_rules, nrow, integer(1))
    rules_df$Rule <- rep(names(rectangular_rules), times = rules_length)

    return(rules_df[, c("Rule", "Package", "OS", "Distribution", "Patterns", "CollapsedPatterns")])
}


rectangularize_single_rule <- function(rule) {
    patterns <- data.frame(
        Patterns = I(list(rule$patterns)),
        CollapsedPatterns = paste(rule$patterns, collapse = "|")
    )

    rectangular_deps <- rectangularize_dependencies(rule$dependencies)

    cbind(patterns[rep(1, nrow(rectangular_deps)), ], rectangular_deps)
}


rectangularize_dependencies <- function(deps) {
    all_deps <- lapply(deps, rectangularize_single_dependency)
    do.call(rbind, all_deps)
}


rectangularize_single_dependency <- function(dep) {
    # Handle Windows which does not have a "distribution"
    raw_distribution <- lapply(dep$constraints, `[[`, "distribution")
    distribution <- vapply(
        raw_distribution,
        function(x) { ifelse(is.null(x), NA_character_, x) },
        character(1)
    )

    data.frame(
        Package = dep$packages,
        OS = vapply(dep$constraints, `[[`, character(1), "os"),
        Distribution = distribution
    )
}
