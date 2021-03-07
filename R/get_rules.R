get_rules <- function(system_req_folder) {
    rules_cache <- rules_cache_location()

    if (file.exists(rules_cache)) {
        rules <- readRDS(rules_cache)
    } else {
        rules <- parse_rules(system_req_folder)

        if (!dir.exists(dirname(rules_cache)))
            dir.create(dirname(rules_cache), recursive = TRUE)

        saveRDS(rules, rules_cache, compress = FALSE)
    }

    return(rules)
}


rules_cache_location <- function() {
    file.path(tempdir(), "pkg.sysreq_cache", "rules.RDS")
}
