#' New way to estimate Aggregate data
#' @rdname pip_gg
pip_grp_new <- \(country         = "ALL",
                 year            = "ALL",
                 povline         = 1.9,
                 group_by        = c("wb", "none"),
                 welfare_type    = c("all", "consumption", "income"),
                 reporting_level = c("all", "national"),
                 lkup,
                 censor          = TRUE,
                 lkup_hash       = lkup$cache_data_id$hash_pip_grp,
                 additional_ind  = FALSE) {

}
