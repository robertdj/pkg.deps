test_that("Get package database with and without cache", {
    if (file.exists(package_db_cache_location()))
        file.remove(package_db_cache_location())

    expect_message(get_package_db(verbose = TRUE), regexp = "Downloading package database from CRAN")
    expect_message(get_package_db(verbose = TRUE), regexp = "Reading package database from cache")
})


test_that("Package database format", {
    db <- get_package_db()

    expect_s3_class(db, "data.frame")
})
