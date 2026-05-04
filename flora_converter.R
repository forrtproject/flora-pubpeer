# flora_converter.R
# Requires: jsonlite (install.packages("jsonlite"))

library(jsonlite)

.parse_val <- function(val) {
  if (is.null(val) || is.na(val) || val %in% c("NA", "")) NULL else val
}

.parse_int <- function(val) {
  if (is.null(val) || is.na(val) || val %in% c("NA", "")) return(NULL)
  result <- suppressWarnings(as.integer(val))
  if (is.na(result)) NULL else result
}

.parse_authors <- function(author_str) {
  if (is.null(author_str) || is.na(author_str) || author_str %in% c("NA", "")) return(list())
  tryCatch(
    jsonlite::fromJSON(author_str, simplifyDataFrame = FALSE),
    error = function(e) list()
  )
}

#' Convert the FLORA replication database CSV to a nested list (JSON-compatible).
#'
#' @param source A file path string or a data.frame already loaded with
#'   read.csv() / readr::read_csv(). File paths are read with UTF-8 encoding.
#'
#' @return A named list with a single \code{results} element whose names are
#'   original paper DOIs and whose values contain paper metadata and a
#'   \code{record} of replications/reproductions with aggregate stats.
#'
#' @examples
#' \dontrun{
#' # From a file path
#' data <- csv_to_flora_json("flora.csv")
#'
#' # From a data.frame
#' df <- read.csv("flora.csv", fileEncoding = "UTF-8-BOM")
#' data <- csv_to_flora_json(df)
#'
#' results <- data$results
#' paper   <- results[["10.1177/0956797610383437"]]
#' }
csv_to_flora_json <- function(source) {
  if (is.character(source)) {
    df <- read.csv(source, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                   na.strings = c("NA", ""))
  } else if (is.data.frame(source)) {
    df <- source
  } else {
    stop("source must be a file path string or a data.frame")
  }

  results <- list()

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    doi_o <- row[["doi_o"]]
    if (is.na(doi_o) || doi_o == "") next

    if (is.null(results[[doi_o]])) {
      results[[doi_o]] <- list(
        doi       = doi_o,
        types     = list("original"),
        doi_hash  = .parse_val(row[["doi_o_hash"]]),
        title     = .parse_val(row[["title_o"]]),
        authors   = .parse_authors(row[["author_o"]]),
        journal   = .parse_val(row[["journal_o"]]),
        year      = .parse_int(row[["year_o"]]),
        volume    = .parse_val(row[["volume_o"]]),
        issue     = .parse_val(row[["issue_o"]]),
        pages     = .parse_val(row[["pages_o"]]),
        apa_ref   = .parse_val(row[["apa_ref_o"]]),
        bibtex_ref = .parse_val(row[["bibtex_ref_o"]]),
        url       = .parse_val(row[["url_o"]]),
        record    = list(
          stats = list(
            n_replications_total     = 0L,
            n_replications_with_doi  = 0L,
            n_replications_only      = 0L,
            n_unique_replication_dois = 0L,
            n_reproductions_total    = 0L,
            n_reproductions_with_doi = 0L,
            n_reproductions_only     = 0L,
            n_originals_total        = 0L,
            n_unique_original_dois   = 0L
          ),
          replications  = list(),
          originals     = list(),
          reproductions = list()
        )
      )
    }

    row_type <- .parse_val(row[["type"]])
    if (is.null(row_type)) row_type <- ""
    doi_r <- .parse_val(row[["doi_r"]])

    related_entry <- list(
      doi                  = doi_r,
      doi_hash             = .parse_val(row[["doi_r_hash"]]),
      type                 = row_type,
      title                = .parse_val(row[["title_r"]]),
      authors              = .parse_authors(row[["author_r"]]),
      journal              = .parse_val(row[["journal_r"]]),
      year                 = .parse_int(row[["year_r"]]),
      volume               = .parse_val(row[["volume_r"]]),
      issue                = .parse_val(row[["issue_r"]]),
      pages                = .parse_val(row[["pages_r"]]),
      apa_ref              = .parse_val(row[["apa_ref_r"]]),
      bibtex_ref           = .parse_val(row[["bibtex_ref_r"]]),
      url                  = .parse_val(row[["url_r"]]),
      outcome              = .parse_val(row[["outcome"]]),
      outcome_quote        = .parse_val(row[["outcome_quote"]]),
      outcome_quote_source = .parse_val(row[["outcome_quote_source"]])
    )

    if (row_type == "replication") {
      results[[doi_o]][["record"]][["replications"]] <-
        c(results[[doi_o]][["record"]][["replications"]], list(related_entry))
      results[[doi_o]][["record"]][["stats"]][["n_replications_total"]] <-
        results[[doi_o]][["record"]][["stats"]][["n_replications_total"]] + 1L
      if (!is.null(doi_r)) {
        results[[doi_o]][["record"]][["stats"]][["n_replications_with_doi"]] <-
          results[[doi_o]][["record"]][["stats"]][["n_replications_with_doi"]] + 1L
      }
    } else if (row_type == "reproduction") {
      results[[doi_o]][["record"]][["reproductions"]] <-
        c(results[[doi_o]][["record"]][["reproductions"]], list(related_entry))
      results[[doi_o]][["record"]][["stats"]][["n_reproductions_total"]] <-
        results[[doi_o]][["record"]][["stats"]][["n_reproductions_total"]] + 1L
      if (!is.null(doi_r)) {
        results[[doi_o]][["record"]][["stats"]][["n_reproductions_with_doi"]] <-
          results[[doi_o]][["record"]][["stats"]][["n_reproductions_with_doi"]] + 1L
      }
    }
  }

  # Second pass: unique DOI counts
  for (doi_o in names(results)) {
    reps   <- results[[doi_o]][["record"]][["replications"]]
    repros <- results[[doi_o]][["record"]][["reproductions"]]

    rep_dois <- Filter(Negate(is.null), lapply(reps, `[[`, "doi"))
    results[[doi_o]][["record"]][["stats"]][["n_unique_replication_dois"]] <-
      length(unique(rep_dois))
    results[[doi_o]][["record"]][["stats"]][["n_replications_only"]] <-
      sum(vapply(reps, function(r) is.null(r[["doi"]]), logical(1)))

    repro_dois <- Filter(Negate(is.null), lapply(repros, `[[`, "doi"))
    results[[doi_o]][["record"]][["stats"]][["n_unique_original_dois"]] <-
      length(unique(repro_dois))
    results[[doi_o]][["record"]][["stats"]][["n_reproductions_only"]] <-
      sum(vapply(repros, function(r) is.null(r[["doi"]]), logical(1)))
  }

  list(results = results)
}
