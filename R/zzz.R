.onLoad <-
    function(...)
{
    opts <- list(
        orcid_client_id = "APP-OSPJCJPZ6P4I3DF0",
        orcid_client_secret = getOption("orcid_client_secret")
    )
    opts <- opts[!names(opts) %in% names(options())]
    options(opts)
}
