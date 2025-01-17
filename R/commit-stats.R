#' @name commit_stats
#'
#' @title Get and summarize repository statistics from GitHub
#'
#' @description These functions allow the user to query the GitHub API and
#'   produce a comprehensive summary of commit activity for each R package
#'   repository. The main workhorse function is `summarize_commit_activity`
#'   which calls all other functions internally to produce a summary of GitHub R
#'   repository activity.
#'
#' @details The main function `summarize_commit_activity` calls most of all the
#'   other helper functions to obtain a list of repository and commit summaries
#'   for a specified date range. Note that `account_repositories` works at the
#'   organization or user level by querying all of the repositories in the user
#'   or organization account. `select_repositories` on the other hand, allows
#'   the user to specify a character vector of owner and repository
#'   combinations, e.g., "owner/repo".
#'
#'   Functions such as `filter_r_repos` and `filter_topic_repos` are utilities
#'   to allow filtering of repositories by certain criteria.
#'
#' @param username `character(1)` The GitHub username
#'
#' @param org `character(1)` optional. The organization account for which to
#'   search repositories for.
#'
#' @param github_token `gh_pat` The personal access token obtained from GitHub.
#'   By default, `gh::gh_token()` is used.
#'
#' @returns `account_repositories`: A list of repositories for the corresponding
#'   account or organization using the `gh` package.
#'
#' @examples
#' if (interactive()) {
#'     ## summarize overall activity for an account
#'     gitcreds::gitcreds_set()
#'     summarize_commit_activity(
#'         username = "LiNk-NY",
#'         org = "waldronlab",
#'         topics = "u24ca289073",
#'         start_date = "2023-08-31",
#'         end_date = "2024-09-01"
#'     )
#'
#'     ## select only certain repositories for analysis
#'     slugs <- c(
#'         "vjcitn/TxRegInfra2",
#'         "vjcitn/BiocOncoTK",
#'         "vjcitn/YESCDS",
#'         "vjcitn/xenLite"
#'     )
#'     select_repositories(repo_slugs = slugs)
#'
#'     ## common workflow using helper functions
#'     ## (alternative to summarize_commit_activity)
#'     username <- "LiNk-NY"
#'     org <- "waldronlab"
#'     all_repos <- account_repositories(username, org)
#'     r_repos <- filter_r_repos(all_repos, username, org)
#'     grant_repos <-
#'         filter_topic_repos(r_repos, username, org, "u24ca289073")
#'     repo_df <- repo_list_df(grant_repos)
#'     repo_commits <- repository_commits(
#'         repo_df,
#'         start_date = "2023-08-31", end_date = "2024-09-01"
#'     )
#'     commits_log <- commits_summary(repo_commits, repo_df)
#'     summary <- repository_summary(
#'         repo_commits, commits_log,
#'         start_date = "2023-08-31", end_date = "2024-09-01"
#'     )
#'     commits_log <-
#'         commits_summary(summary$commits_list, summary$repositories)
#' }
#' @export
account_repositories <- function(username, org, github_token = gh::gh_token()) {
    message("Finding repositories for ", username, "...")
    repos <- list()
    page <- 1
    has_more <- TRUE

    while (has_more) {
        if (!missing(org)) {
            endpoint <- "GET /orgs/{org}/repos"
            username <- org
        } else {
            endpoint <- "GET /users/{username}/repos"
        }
        new_repos <- gh::gh(
            endpoint,
            username = username,
            org = username,
            page = page,
            .token = github_token
        )
        if (length(new_repos)) {
            repos <- c(repos, new_repos)
            page <- page + 1
        } else {
            has_more <- FALSE
        }
    }
    repos
}

#' @rdname commit_stats
#'
#' @param repo_slugs `character()` A vector of owner and repository strings
#'   collapsed by the forward slash, e.g., "owner/repo"
#'
#' @returns `select_repositories`: A list of repository metadata as obtained by
#'   `"GET /repos/{owner}/{repo}"` GitHub API endpoint using the `gh` package.
#'
#' @importFrom BiocBaseUtils isCharacter
#'
#' @export
select_repositories <- function(repo_slugs, github_token = gh::gh_token()) {
    stopifnot(isCharacter(repo_slugs))
    owners_repos <- strsplit(repo_slugs, "/")
    lapply(
        owners_repos, function(ownerrepo) {
            owner <- ownerrepo[1L]
            repo <- ownerrepo[2L]
            message("Fetching ", owner, "/", repo, "...")
            gh::gh(
                "GET /repos/{owner}/{repo}",
                owner = owner,
                repo = repo,
                .token = github_token
            )
        }
    )
}

#' @rdname commit_stats
#'
#' @param repo_list `list` A list as obtained from `account_repositories` or
#'   `select_repositories`
#'
#' @returns `filter_r_repos`: A list of filtered repositories containing R code
#'
#' @export
filter_r_repos <-
    function(repo_list, username, org, github_token = gh::gh_token())
{
    message("Identifying R repositories...")
    if (!missing(org))
        username <- org
    purrr::map(repo_list, function(repo) {
        languages <- gh::gh(
            "GET /repos/{owner}/{repo}/languages",
            owner = username,
            repo = repo$name,
            .token = github_token
        )
        if ("R" %in% names(languages)) {
            repo$r_percentage <-
                round(languages$R / sum(unlist(languages)) * 100, 1)
            repo
        }
    }) |> purrr::compact()
}

#' @rdname commit_stats
#'
#' @param topics `character()` A vector of topics e.g., grant award numbers
#'   (`u24ca######`) that are listed under "topics" on the GitHub repository
#'   page
#'
#' @returns `filter_topic_repos`: A list of filtered repositories matching any
#'   of the topics in the GitHub repository
#'
#' @export
filter_topic_repos <-
    function(repo_list, username, org, topics, github_token = gh::gh_token())
{
    message("Filtering by repository topics")
    if (!missing(org))
        username <- org
    Filter(
        function(repo) {
            repo_topics <- gh::gh(
                "GET /repos/{owner}/{repo}/topics",
                owner = username,
                repo = repo$name,
                .token = github_token
            ) |> unlist()
            any(
                tolower(topics) %in% tolower(repo_topics)
            )
        },
        repo_list
    )
}

#' @rdname commit_stats
#'
#' @returns `repo_list_df`: A `tibble` `data.frame` with rows corresponding to
#'   repositories including a column of calculated percentage of R code
#'
#' @export
repo_list_df <- function(repo_list) {
    purrr::map_df(
        repo_list,
        function(repo) {
            tibble::tibble(
                full_name = repo$full_name,
                name = repo$name,
                owner = repo[[c("owner", "login")]],
                description =
                    ifelse(is.null(repo$description), NA, repo$description),
                stars = repo$stargazers_count,
                forks = repo$forks_count,
                last_updated = repo$updated_at,
                is_fork = repo$fork,
                default_branch = repo$default_branch,
                r_percentage = repo$r_percentage
            )
        }
    )
}

#' @rdname commit_stats
#'
#' @param repos_df `tibble` A tibble of filtered R repositories as obtained from
#'   `filter_r_repos`
#'
#' @param start_date,end_date `character(1)` The start and end dates delimiting
#'   commit searches in the `YYYY-MM-DD` format
#'
#' @returns `repository_commits`: A `list` of commits for each row in the
#'   `repos_df` input
#'
#' @export
repository_commits <- function(
    repos_df, github_token = gh::gh_token(),
    start_date, end_date
) {
    message("Fetching commits for ", nrow(repos_df), " R repositories...")
    all_commits <- list()
    for (i in seq_len(nrow(repos_df))) {
        repo <- repos_df$full_name[i]
        message("Processing ", repo, " (", i, "/", nrow(repos_df), ")")
        commits <- tryCatch({
            gh::gh(
                "GET /repos/{owner}/{repo}/commits",
                owner = repos_df$owner[i],
                repo = repos_df$name[i],
                since = start_date,
                until = end_date,
                .token = github_token
            )
        }, error = function(e) {
            warning("Error fetching commits for ", repo, ": ", e$message)
            return(list())
        })
        repo_commits <- purrr::map(commits, function(commit) {
            list(
                repository = repo,
                sha = commit$sha,
                author = commit$commit$author$name,
                date = commit$commit$author$date,
                message = commit$commit$message,
                changes = tryCatch({
                    commit_detail <- gh::gh(
                        "GET /repos/{owner}/{repo}/commits/{sha}",
                        owner = repos_df$owner[i],
                        repo = repos_df$name[i],
                        sha = commit$sha,
                        since = start_date,
                        until = end_date,
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
        })
        all_commits <- c(all_commits, repo_commits)
    }
    all_commits
}

#' @rdname commit_stats
#'
#' @returns `commits_summary`: A `data.frame` of repositories and their
#'   statistics including a concatenation of all commit messages (in the
#'   `commit_log` column)
#'
#' @export
commits_summary <- function(commits_list, repos_df) {
    repo_list <- split(
        commits_list, sapply(commits_list, function(x) x$repository)
    )
    summary <- lapply(repo_list, function(repo) {
        paste(
            "\n\n----", unique(map_chr(repo, function(x) x$repository)), ":\n",
            paste(
                Filter(
                    nchar,
                    map_chr(repo, function(x) {
                        if (
                            startsWith(x$message, "version bump") ||
                            startsWith(x$message, "bump x.y.z")
                        )
                            character(1L)
                        else
                            paste0(
                                gsub("\n", "", x$message), " (", x$author, ")"
                            )
                    })
                ), collapse = "\n"
            )
        )
    }) |> stack()
    names(summary) <- c("commit_log", "org_repo")
    merge(repos_df, summary, by.x = "full_name", by.y = "org_repo")
}

#' @rdname commit_stats
#'
#' @param commits_list `list` The output of `repository_commits` that contains
#'   commit details for each repository
#'
#' @returns `repository_summary`: A `list` of `tibbles` that summarize activity
#'   in the associated `repositories` for the `username` / `org` account
#'
#' @importFrom dplyr group_by summarize n n_distinct
#' @importFrom rlang .data
#'
#' @export
repository_summary <- function(
    commits_list, repositories, start_date, end_date
) {
    commit_stats <- tibble::tibble(
        repository = map_chr(commits_list, "repository"),
        author = map_chr(commits_list, "author"),
        date = map_chr(commits_list, "date"),
        additions = map_dbl(commits_list, function(x) x$changes$additions),
        deletions = map_dbl(commits_list, function(x) x$changes$deletions),
        files_changed =
            map_dbl(commits_list, function(x) x$changes$files_changed)
    )
    repo_summary <- commit_stats |>
        group_by(.data$repository) |>
        summarize(
            total_commits = n(),
            unique_authors = n_distinct(.data$author),
            total_additions = sum(.data$additions, na.rm = TRUE),
            total_deletions = sum(.data$deletions, na.rm = TRUE),
            total_files_changed = sum(.data$files_changed, na.rm = TRUE)
        )
    summary <- list(
        time_period = tibble::tibble(
            start = start_date, end = end_date
        ),
        repositories = repositories,
        repository_stats = repo_summary,
        commit_details = commit_stats,
        overall_stats = tibble::tibble(
            total_repositories = nrow(repositories),
            total_commits = nrow(commit_stats),
            unique_authors = n_distinct(commit_stats$author),
            total_additions = sum(commit_stats$additions, na.rm = TRUE),
            total_deletions = sum(commit_stats$deletions, na.rm = TRUE),
            total_files_changed = sum(commit_stats$files_changed, na.rm = TRUE)
        ),
        commits_list = commits_list
    )
    class(summary) <- c("commit_summary", class(summary))
    summary
}

#' @rdname commit_stats
#'
#' @importFrom tibble tibble
#' @importFrom gh gh gh_token
#' @importFrom purrr map_df map_chr map_dbl map
#'
#' @returns `summarize_commit_activity`: A `list` of `tibbles` that summarize
#'   activity in the associated `repositories` for the `username` / `org`
#'   account
#'
#' @export
summarize_commit_activity <- function(
    username,
    org,
    repo_slugs,
    topics,
    start_date,
    end_date,
    github_token = gh::gh_token()
) {
    start_date <- as.POSIXct(start_date) |> format("%Y-%m-%dT%H:%M:%SZ")
    end_date <- as.POSIXct(end_date) |> format("%Y-%m-%dT%H:%M:%SZ")
    # Step 1: Find all repositories for the account
    if (!missing(repo_slugs))
        repos <- select_repositories(
            repo_slugs = repo_slugs, github_token = github_token
        )
    else
        repos <- account_repositories(
            username = username, org = org, github_token = github_token
        )
    # Step 2A: Filter for R repositories
    repos <- filter_r_repos(
        repos, username = username, org = org,
        github_token = github_token
    )
    if (!length(repos))
        stop("No R package repositories found in 'username' / 'org' account")
    # Step 2B: (optional) Filter by GitHub repository topics
    if (length(topics))
        repos <- filter_topic_repos(
            repos, username = username, org = org, topics = topics,
            github_token = github_token
        )
    # Step 2C: Convert repo list to tibble
    r_repos <- repo_list_df(repos)
    # Step 3: Fetch commits for each R repository
    commits_list <- repository_commits(
        repos_df = r_repos, github_token = github_token,
        start_date = start_date, end_date = end_date
    )
    # Step 4: Summarize statistics
    repository_summary(
        commits_list = commits_list, repositories = r_repos,
        start_date = start_date, end_date = end_date
    )
}

# Print method for nice output

#' @rdname commit_stats
#'
#' @exportS3Method base::print
print.commit_summary <- function(x, ...) {
    cat("\nR Development Activity Analysis\n")
    cat("============================\n")
    cat(
        "Period:", x$time_period$start, "to", x$time_period$end, "\n\n"
    )
    cat("Overall Statistics:\n")
    cat(sprintf("- R Repositories: %d\n", x$overall_stats$total_repositories))
    cat(sprintf("- Total Commits: %d\n", x$overall_stats$total_commits))
    cat(sprintf("- Unique Contributors: %d\n", x$overall_stats$unique_authors))
    cat(sprintf("- Lines Added: %d\n", x$overall_stats$total_additions))
    cat(sprintf("- Lines Deleted: %d\n", x$overall_stats$total_deletions))
    cat(sprintf("- Files Changed: %d\n\n", x$overall_stats$total_files_changed))

    cat("Repository Summary:\n")
    print(x$repository_stats)
}
