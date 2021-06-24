make_sysreq_folder <- function()
{
    system_req_folder <- file.path(tempdir(), "pkg.deps_test_rules")

    # rules.zip contains the files r-system-requirements/rules/z*.json
    rules_archive <- testthat::test_path("test-files", "rules.zip")
    unzip(rules_archive, exdir = system_req_folder)

    return(system_req_folder)
}


clear_rules_cache <- function()
{
    if (file.exists(rules_cache_location()))
        unlink(rules_cache_location())

    return(invisible(NULL))
}
