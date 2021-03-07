test_that("Empty result for package with no system requirements", {
    deps <- get_package_reqs("Rcpp")

    expect_named(deps, c("Package", "SystemRequirements"))
    expect_equal(nrow(deps), 0L)
})


test_that("Result for package with system requirements", {
    deps <- get_package_reqs("curl")

    expect_named(deps, c("Package", "SystemRequirements"))
    expect_gt(length(deps), 0L)
})
