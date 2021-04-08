test_that("Error when there is no rules folder", {
    expect_error(get_rules("foo"), regexp = "foo does not have a 'rules' subfolder")
})


test_that("Error when there are no JSON files with rules", {
    fake_sysreq_folder <- file.path(tempdir(), "sysreq")
    fake_rules_folder <- file.path(fake_sysreq_folder, "rules")

    dir.create(fake_rules_folder, recursive = TRUE)
    on.exit(unlink(fake_rules_folder))

    expect_error(get_rules(fake_sysreq_folder), regexp = "There are no JSON files with rules in")
})
