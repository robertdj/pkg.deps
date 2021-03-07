test_that("Error getting rules if location is not specified", {
    expect_error(get_rules("foo"), regexp = "There are no JSON files with rules in")
})
