#' summarize download statistics for bioconductor software
#' @import ggplot2
#' @import lubridate
#' @importFrom utils read.table
#' @importFrom dplyr filter
#' @import tibble
#' @param years integer(3)
#' @note Modified from BiocReports script to give raw data on download activities per month.
#' There may be effects in 2022 of modified distribution channels.
#' @return a list with components ipplot, dlplot with ggplot elements, and a tibble with all stats.
#' @examples
#' pls = summarize_software_downloads()
#' opar = par(no.readonly=TRUE)
#' par(ask=TRUE)
#' pls$ipplot
#' pls$dlplot
#' par(opar)
#' @export
summarize_software_downloads <-
    function(years = 2018:2022, final = "2022-08-16")
{
    urls <- sprintf(
        "http://bioconductor.org/packages/stats/bioc/bioc_%d_stats.tab",
        years
    )

    tbl <- Map(read.table, urls, MoreArgs=list(header=TRUE))
    tbl0 <- as_tibble(do.call(rbind, unname(tbl)))
    tbl0 = tbl0 |> filter(.data$Month != "all")
    dd = paste(tbl0$Year, tbl0$Month, "15", sep="-")
    dld = as_date(dd)
    tbl0$Date = dld
    tbl0 = tbl0 |> dplyr::filter(.data$Date < as_date(final))

    ipplot = ggplot(tbl0, aes(x=.data$Date, y=.data$Nb_of_distinct_IPs)) +
        geom_point(size=5) +
        ylab("# Unique IP addresses") +
        ggtitle("Unique IPs requesting downloads")
    dlplot = ggplot(tbl0, aes(x=.data$Date, y=.data$Nb_of_downloads)) +
        geom_point(size=5) +
        ylab("# downloads") +
        ggtitle("Number of downloads requested")
    list(ipplot=ipplot, dlplot=dlplot, table=tbl0)
}

