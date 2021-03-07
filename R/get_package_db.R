get_package_db <- function(verbose = FALSE) {
    package_db_cache <- package_db_cache_location()

    if (file.exists(package_db_cache)) {
        if (isTRUE(verbose))
            message("Reading package database from cache")

        db <- readRDS(package_db_cache)
    } else {
        if (isTRUE(verbose))
            message("Downloading package database from CRAN")

        db <- tools::CRAN_package_db()

        if (!dir.exists(dirname(package_db_cache)))
            dir.create(dirname(package_db_cache), recursive = TRUE)

        saveRDS(db, file = package_db_cache, compress = FALSE)
    }

    return(db)
}


package_db_cache_location <- function() {
    file.path(tempdir(), "pkg.deps_cache", "package_db.RDS")
}
