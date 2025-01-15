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
