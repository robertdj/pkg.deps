test_that("Error when there is no rules folder", {
    clear_rules_cache()
    expect_error(parse_rules("foo"), regexp = "foo does not have a 'rules' subfolder")
})


test_that("Error when there are no JSON files with rules", {
    fake_sysreq_folder <- file.path(tempdir(), "sysreq")

    dir.create(file.path(fake_sysreq_folder, "rules"), recursive = TRUE)
    on.exit(unlink(fake_sysreq_folder, recursive = TRUE))

    clear_rules_cache()
    expect_error(get_rules(fake_sysreq_folder), regexp = "There are no JSON files with rules in")
})


test_that("Use rules cache", {
    clear_rules_cache()
    on.exit(clear_rules_cache())

    system_req_folder <- make_sysreq_folder()
    on.exit(unlink(system_req_folder, recursive = TRUE))

    expect_false(file.exists(rules_cache_location()))

    rules <- get_rules(system_req_folder)
    expect_true(file.exists(rules_cache_location()))

    # Trigger load from cache
    rules <- get_rules(system_req_folder)
})


test_that("Get (subset) of rules", {
    system_req_folder <- make_sysreq_folder()
    on.exit(unlink(system_req_folder, recursive = TRUE))

    rules <- get_rules(system_req_folder)

    expect_named(rules, c("Rule", "Package", "OS", "Distribution", "Patterns", "CollapsedPatterns"))
    expect_gt(nrow(rules), 0)

    expect_true(
        all(c("ubuntu", "debian", "centos", "redhat", "opensuse", "sle") %in% rules$Distribution)
    )

    expect_true(all(c("linux", "windows") %in% rules$OS))

    windows_rules <- subset(rules, OS == "windows")
    expect_true(all(is.na(windows_rules$Distribution)))
})
