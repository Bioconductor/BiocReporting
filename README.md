# BiocReporting

This R package provides tools for reporting on the Bioconductor project,
with a focus on gathering metrics for funding agencies and internal review.

## Key Features

*   **GitHub Statistics**: Obtain commit statistics for multiple
    repositories within a specified time frame.
*   **ORCID Information**: Retrieve employment details for multiple ORCID iDs
    using the public API.
*   **Support Site Usage**: Track user and question metrics on the
    Bioconductor Support Site.
*   **Download Statistics**: Summarize and visualize download data for
    Bioconductor software packages over several years.

## Example Usage

```r
# Get support site statistics
get_support_site_stats()

# Summarize software downloads
pls <- summarize_software_downloads()
pls$ipplot
```

