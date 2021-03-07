test_that("Empty result for package with no non-base dependencies", {
    deps <- get_package_deps("remotes")

    expect_type(deps, "character")
    expect_length(deps, 0L)
})


test_that("Result for package with dependencies", {
    deps <- get_package_deps("purrr")

    expect_type(deps, "character")
    expect_gt(length(deps), 0L)
})
