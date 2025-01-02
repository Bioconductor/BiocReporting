#' @import lubridate
#' @importFrom tibble tibble
#' @importFrom gh gh
#' @importFrom purrr map_df map_chr map_dbl map
#'
#' @examples
#' results <- analyze_r_account_activity(
#'     "LiNk-NY", "waldronlab",
#'     start_date = "2023-08-31",
#'     end_date = "2024-09-01"
#' )
#'
#' @export
analyze_r_account_activity <- function(
        username,
        org,
        start_date,
        end_date,
        github_token = gh::gh_token()
) {
    # Input validation
    if (is.null(github_token)) {
        stop("No GitHub token found. Please set GITHUB_PAT environment variable or authenticate with GitHub CLI")
    }

    # Step 1: Find all repositories for the account
    message("Finding repositories for ", username, "...")
    repos <- list()
    page <- 1
    has_more <- TRUE

    while (has_more) {
        if (!missing(org)) {
            endpoint <- "GET /orgs/{org}/repos"
            new_repos <- gh::gh(
                endpoint,
                org = org,
                page = page,
                .token = github_token
            )
        } else {
            endpoint <- "GET /users/{username}/repos"
            new_repos <- gh::gh(
                endpoint,
                username = username,
                org = username,
                page = page,
                .token = github_token
            )
        }


        if (length(new_repos)) {
            repos <- c(repos, new_repos)
            page <- page + 1
        } else {
            has_more <- FALSE
        }
    }

    # Step 2: Filter for R repositories
    message("Identifying R repositories...")
    r_repos <- purrr::map_df(repos, function(repo, org) {
        # Check for R language
        if (!missing(org))
            languages <- gh::gh(
                "GET /repos/{owner}/{repo}/languages",
                owner = org,
                repo = repo$name,
                .token = github_token
            )
        else
            languages <- gh::gh(
                "GET /repos/{owner}/{repo}/languages",
                owner = username,
                repo = repo$name,
                .token = github_token
            )

        if ("R" %in% names(languages)) {
            tibble::tibble(
                full_name = repo$full_name,
                name = repo$name,
                description = ifelse(is.null(repo$description), NA, repo$description),
                stars = repo$stargazers_count,
                forks = repo$forks_count,
                last_updated = repo$updated_at,
                is_fork = repo$fork,
                default_branch = repo$default_branch,
                r_percentage = round(languages$R / sum(unlist(languages)) * 100, 1)
            )
        }
    }, org = org)

    if (!nrow(r_repos)) {
        stop("No R repositories found for this account")
    }

    # Step 3: Fetch commits for each R repository
    message("Fetching commits for ", nrow(r_repos), " R repositories...")
    all_commits <- list()

    for (i in seq_len(nrow(r_repos))) {
        repo <- r_repos$full_name[i]
        message("Processing ", repo, " (", i, "/", nrow(r_repos), ")")

        commits <- tryCatch({
            if (!missing(org))
                gh::gh(
                    "GET /repos/{owner}/{repo}/commits",
                    author = username,
                    owner = org,
                    repo = r_repos$name[i],
                    since = as.POSIXct(start_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                    until = as.POSIXct(end_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                    .token = github_token
                )
            else
                gh::gh(
                    "GET /repos/{owner}/{repo}/commits",
                    author = username,
                    owner = username,
                    repo = r_repos$name[i],
                    since = as.POSIXct(start_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                    until = as.POSIXct(end_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                    .token = github_token
                )
        }, error = function(e) {
            warning(paste("Error fetching commits for", repo, ":", e$message))
            return(list())
        })

        # Process commits
        repo_commits <- purrr::map(commits, function(commit, org) {
            list(
                repository = repo,
                sha = commit$sha,
                author = commit$commit$author$name,
                date = commit$commit$author$date,
                message = commit$commit$message,
                changes = tryCatch({
                    if (!missing(org))
                        commit_detail <- gh::gh(
                            "GET /repos/{owner}/{repo}/commits/{sha}",
                            owner = org,
                            repo = r_repos$name[i],
                            sha = commit$sha,
                            since = as.POSIXct(start_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                            until = as.POSIXct(end_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                            .token = github_token
                        )
                    else
                        commit_detail <- gh::gh(
                            "GET /repos/{owner}/{repo}/commits/{sha}",
                            owner = username,
                            repo = r_repos$name[i],
                            sha = commit$sha,
                            since = as.POSIXct(start_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                            until = as.POSIXct(end_date) |> format("%Y-%m-%dT%H:%M:%SZ"),
                            .token = github_token
                        )
                    list(
                        additions = commit_detail$stats$additions,
                        deletions = commit_detail$stats$deletions,
                        files_changed = length(commit_detail$files)
                    )
                }, error = function(e) {
                    list(additions = NA, deletions = NA, files_changed = NA)
                })
            )
        }, org = org)

        all_commits <- c(all_commits, repo_commits)
    }

    # Step 4: Generate statistics
    commit_stats <- tibble::tibble(
        repository = map_chr(all_commits, "repository"),
        author = map_chr(all_commits, "author"),
        date = map_chr(all_commits, "date"),
        additions = map_dbl(all_commits, function(x) x$changes$additions),
        deletions = map_dbl(all_commits, function(x) x$changes$deletions),
        files_changed = map_dbl(all_commits, function(x) x$changes$files_changed)
    )

    repo_summary <- commit_stats |>
        group_by(repository) |>
        summarise(
            total_commits = n(),
            unique_authors = n_distinct(author),
            total_additions = sum(additions, na.rm = TRUE),
            total_deletions = sum(deletions, na.rm = TRUE),
            total_files_changed = sum(files_changed, na.rm = TRUE)
        )

    # Step 5: Generate analysis prompt
    analysis_prompt <- sprintf(
        "You are a senior R developer and data scientist. Please analyze the
        following GitHub development activity for %s between %s and %s.\n\n",
        username, start_date, end_date
    )

    # Add repository overview
    analysis_prompt <- paste0(
        analysis_prompt,
        "Repository Overview:\n",
        paste(map_chr(seq_len(nrow(r_repos)), function(i) {
            sprintf("- %s: %s (Stars: %d, Forks: %d, R code: %.1f%%)",
                    r_repos$full_name[i],
                    if(is.na(r_repos$description[i])) "No description" else r_repos$description[i],
                    r_repos$stars[i],
                    r_repos$forks[i],
                    r_repos$r_percentage[i])
        }), collapse = "\n"),
        "\n\n"
    )

    # Add commit details
    analysis_prompt <- paste0(
        analysis_prompt,
        "Commit Activity:\n\n",
        paste(map_chr(all_commits, function(x) {
            sprintf("Repository: %s\nDate: %s\nAuthor: %s\nChanges: +%s/-%s (%s files)\nMessage: %s\n",
                    x$repository,
                    x$date,
                    x$author,
                    ifelse(is.na(x$changes$additions), "?", x$changes$additions),
                    ifelse(is.na(x$changes$deletions), "?", x$changes$deletions),
                    ifelse(is.na(x$changes$files_changed), "?", x$changes$files_changed),
                    x$message)
        }), collapse = "\n"),
        "\n"
    )

    # Add overall statistics
    analysis_prompt <- paste0(
        analysis_prompt,
        "Overall Statistics:\n",
        sprintf("- Total R Repositories: %d\n", nrow(r_repos)),
        sprintf("- Total Commits: %d\n", nrow(commit_stats)),
        sprintf("- Unique Contributors: %d\n", n_distinct(commit_stats$author)),
        sprintf("- Total Lines Added: %d\n", sum(commit_stats$additions, na.rm = TRUE)),
        sprintf("- Total Lines Deleted: %d\n", sum(commit_stats$deletions, na.rm = TRUE)),
        sprintf("- Total Files Changed: %d\n\n", sum(commit_stats$files_changed, na.rm = TRUE))
    )

    # Add analysis request
    analysis_prompt <- paste0(
        analysis_prompt,
        "Please provide a comprehensive analysis of this R development activity, including:\n",
        "1. Overall development patterns and trends\n",
        "2. Key areas of focus across repositories\n",
        "3. Notable features or changes\n",
        "4. Development activity distribution\n",
        "5. Collaboration patterns\n",
        "6. Recommendations for future development\n\n",
        "Please structure your response in clear sections and provide specific examples from the commit data to support your analysis."
    )

    # Return results
    list(
        account_info = list(
            name = username,
            date_range = list(start = start_date, end = end_date)
        ),
        repositories = r_repos,
        repository_stats = repo_summary,
        commit_details = commit_stats,
        analysis_prompt = analysis_prompt,
        overall_stats = list(
            total_repositories = nrow(r_repos),
            total_commits = nrow(commit_stats),
            unique_authors = n_distinct(commit_stats$author),
            total_additions = sum(commit_stats$additions, na.rm = TRUE),
            total_deletions = sum(commit_stats$deletions, na.rm = TRUE),
            total_files_changed = sum(commit_stats$files_changed, na.rm = TRUE)
        )
    )
}

# Print method for nice output
#' @export
print.r_account_analysis <- function(x) {
    cat("\nR Development Activity Analysis\n")
    cat("============================\n")
    cat(sprintf("Username/Org: %s (%s)\n", x$account_info$name))
    cat(sprintf("Period: %s to %s\n\n", x$account_info$date_range$start, x$account_info$date_range$end))

    cat("Overall Statistics:\n")
    cat(sprintf("- R Repositories: %d\n", x$overall_stats$total_repositories))
    cat(sprintf("- Total Commits: %d\n", x$overall_stats$total_commits))
    cat(sprintf("- Unique Contributors: %d\n", x$overall_stats$unique_authors))
    cat(sprintf("- Lines Added: %d\n", x$overall_stats$total_additions))
    cat(sprintf("- Lines Deleted: %d\n", x$overall_stats$total_deletions))
    cat(sprintf("- Files Changed: %d\n\n", x$overall_stats$total_files_changed))

    cat("Repository Summary:\n")
    print(x$repository_stats)

    cat("\nAnalysis Prompt:\n")
    cat("Copy and paste the following prompt into your preferred AI tool:\n")
    cat("----------------------------------------------------------------\n")
    cat(x$analysis_prompt)
    cat("\n----------------------------------------------------------------\n")
}
