test_that("Get distro specific packages for Ubuntu", {
    system_req_folder <- make_sysreq_folder()
    on.exit(unlink(system_req_folder, recursive = TRUE))

    distro_result <- distro_req(data.frame(SystemRequirements = "zlib"), "ubuntu", system_req_folder)

    expect_named(distro_result, c("Rule", "Package"))
    expect_gt(nrow(distro_result), 0)
})


test_that("Error getting distro specific packages for non-existing distro", {
    system_req_folder <- make_sysreq_folder()
    on.exit(unlink(system_req_folder, recursive = TRUE))

    expect_error(
        distro_req(data.frame(SystemRequirements = "zlib"), "foo", system_req_folder),
        regexp = "No rules for distribution 'foo'"
    )
})
