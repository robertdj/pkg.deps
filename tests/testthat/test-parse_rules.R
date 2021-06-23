test_that("Error when there is no rules folder", {
    expect_error(get_rules("foo"), regexp = "foo does not have a 'rules' subfolder")
})


test_that("Error when there are no JSON files with rules", {
    fake_sysreq_folder <- file.path(tempdir(), "sysreq")

    dir.create(file.path(fake_sysreq_folder, "rules"), recursive = TRUE)
    on.exit(unlink(fake_sysreq_folder, recursive = TRUE))

    expect_error(get_rules(fake_sysreq_folder), regexp = "There are no JSON files with rules in")
})


test_that("Parse (subset) of rules", {
    system_req_folder <- file.path(tempdir(), "pkg.deps_test_rules")
    on.exit(unlink(system_req_folder, recursive = TRUE))

    # rules.zip contains the files r-system-requirements/rules/z*.json
    rules_archive <- testthat::test_path("test-files", "rules.zip")
    unzip(rules_archive, exdir = system_req_folder)

    rules <- parse_rules(system_req_folder)

    expect_named(rules, c("Rule", "Package", "OS", "Distribution", "Patterns", "CollapsedPatterns"))
    expect_gt(nrow(rules), 0)

    expect_true(
        all(c("ubuntu", "debian", "centos", "redhat", "opensuse", "sle") %in% rules$Distribution)
    )

    expect_true(all(c("linux", "windows") %in% rules$OS))

    windows_rules <- subset(rules, OS == "windows")
    expect_true(all(is.na(windows_rules$Distribution)))
})
