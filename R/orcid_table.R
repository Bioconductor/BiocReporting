.get_orcid_token <- function() {
    request("https://orcid.org/oauth/token") |>
        req_headers(
            "Accept" = "application/json",
            "Content-Type" = "application/x-www-form-urlencoded"
        ) |>
        req_body_form(
            client_id = getOption("orcid_client_id"),
            client_secret = getOption("orcid_client_secret"),
            grant_type = "client_credentials",
            scope = "/read-public"
        ) |>
        req_method("POST") |>
        req_perform() |>
        resp_body_json()
}

.get_orcid_endpoint <- function(orcid_id, endpoint, token) {
    request("https://pub.orcid.org/") |>
        req_template(
            "v3.0/{orcid}/{endpoint}",
            orcid = orcid_id,
            endpoint = endpoint
        ) |>
        req_headers(
            Accept = "application/vnd.orcid+json"
        ) |>
        req_auth_bearer_token(token$access_token) |>
        req_perform() |>
        resp_body_json()
}

#' @name orcid_table
#'
#' @importFrom httr2 request req_headers req_body_form req_method req_perform
#'   resp_body_json req_template req_auth_bearer_token
#'
#' @examples
#' get_orcid_employments(orcid_id = "0000-0003-4046-0063")
#' @export
get_orcid_employments <- function(orcid_id) {
    token <- .get_orcid_token()
    .get_orcid_endpoint(orcid_id, "employments", token)
}

.get_orcid_rec <- function(orcid, rename) {
    kp <- c(
        "employment-summary.source.source-name.value",
        "employment-summary.organization.name",
        "employment-summary.organization.address.city",
        "employment-summary.organization.address.region",
        "employment-summary.organization.address.country"
    )
    # return a slug of NAs -- in case affiliation-group data are not populated
    nfields <- length(kp)
    sna <- rep(NA, nfields)
    # needs to reflect order in kp above
    sslug <- c("name", "org", "city", "region", "country")
    if (rename)
        names(sna) <- sslug
    else
        names(sna) <- kp
    if (!is.na(orcid)) {
        ee <- orcid_employments(orcid)
        # should be more precise than try()
        dat <- try(
            t(ee[[orcid]][["affiliation-group"]][["summaries"]][[1]]),
            silent=TRUE
        )
    }
    if (is.na(orcid) || inherits(dat, "try-error")) {
        ans <- matrix(sna, nrow=1)
        colnames(ans) <- names(sna)
        ans <- data.frame(ans)
        ans$orcid <- orcid
        return(ans)
    }
    ans <- dat[kp,]
    if (rename)
        names(ans) <- sslug
    ans <- data.frame(t(ans))
    ans$orcid <- orcid
    rownames(ans) <- NULL
    ans
}

#' @title Mine employment data from ORCID
#'
#' @description Get a `data.frame` of employment info from ORCID
#'
#' @details The function uses fields returned by API using `rorcid` package
#'   (last update on Oct. 2022)
#'
#' @importFrom rorcid orcid_employments
#'
#' @param rename `logical(1)` if `TRUE` use short names
#'
#' @param orcids `character()` A vector of ORCID identifiers
#'
#' @returns a `data.frame` of employment info using the ORCID API
#'
#' @examples
#' if (interactive()) {
#'     ## need a token?
#'     oids <- c("0000-0003-4046-0063", "0000-0003-4046-0063")
#'     orcid_table(oids)
#'     oids <- c(oids, NA)
#'     orcid_table(oids)
#'     orcid_table(oids[1])
#' }
#' @export
orcid_table <- function(orcids, rename = TRUE) {
    ans <- lapply(orcids, .get_orcid_rec, rename = rename)
    do.call(rbind, ans)
}
