# Load packages
library(httr2)
library(jsonlite)
library(FReD)
library(tidyverse)
library(glue)

# ── Step 1: Load data ──────────────────────────────────────────────────────────

flora_raw <- read_csv("flora.csv")

flora_raw |> count(doi_o) |> filter(n > 1) |> arrange(desc(n))

# ── Step 2: Clean data ─────────────────────────────────────────────────────────

flora_clean <- flora_raw |>
                filter(!is.na(doi_o), !is.na(doi_r)) |>
                select(
                  title_o,
                  doi_o,
                  journal_o,
                  title_r,
                  doi_r,
                  journal_r,
                  reported_success,
                  reported_success_quote
                )

# ── Step 3: Define comment generator function ──────────────────────────────────

generate_comment <- function(title_o, doi_o, journal_o,
                             title_r, doi_r, journal_r,
                             reported_success, reported_success_quote) {
  
  # Normalise outcome text
  outcome_lower <- tolower(as.character(reported_success))
  
  # Pick the right framing based on reported_success
  outcome_framing <- case_when(
    outcome_lower == "successful" ~
      "The replication **successfully replicated** the original findings.",
    
    outcome_lower == "failed" ~
      "The replication **did not replicate** the original findings. This does not necessarily indicate error - replication outcomes can differ for many reasons.",
    
    outcome_lower == "mixed" ~
      "The replication produced **mixed results** - some findings replicated and some did not.",
    
    outcome_lower == "statistically successful but flawed" ~
      "The replication was **statistically successful but flawed** - the original findings were reproduced statistically, but methodological concerns were noted by the replication authors.",
    
    outcome_lower == "descriptive only" ~
      "The replication was **descriptive only** - no inferential statistical comparison with the original findings was made.",
    
    outcome_lower == "uninformative" ~
      "The replication was **uninformative** - the results could not be used to draw conclusions about the original findings.",
    
    TRUE ~
      glue("Outcome: **{reported_success}**.")
  )
  
  # Build the quote block only if a quote exists
  quote_block <- if (!is.na(reported_success_quote) &
                     nchar(trimws(reported_success_quote)) > 0) {
    glue('\n\nFinding: "{reported_success_quote}"')
  } else {
    ""
  }
  
  # Build the full comment
  glue(
    "A replication of **{title_o}** (_{journal_o}_; ",
    "https://doi.org/{doi_o}) has been registered in the ",
    "FORRT Library of Replication Attempts (FLoRA).\n\n",
    "{outcome_framing}",
    "{quote_block}\n\n",
    "Replication study: {title_r} (_{journal_r}_)\n",
    "https://doi.org/{doi_r}\n\n",
    "For context on interpreting replication outcomes, visit:\n",
    "https://forrt.org/pubpeerreplication\n\n",
    "---\n",
    "*This comment was posted automatically by FORRT. ",
    "Learn more and provide feedback at ",
    "https://forrt.org/pubpeerreplication*"
  )
}

# ── Step 4: Generate comments for all studies ──────────────────────────────────

flora_with_comments <- flora_clean |>
  mutate(
    comment_text = pmap_chr(
      list(title_o, doi_o, journal_o,
           title_r, doi_r, journal_r,
           reported_success, reported_success_quote),
      generate_comment
    )
  )




# ── Step 5: Spot check ─────────────────────────────────────────────────────────

flora_with_comments |> select(doi_o, comment_text) |> head(3)

# ── Step 6: Save outputs ───────────────────────────────────────────────────────

# Full dataset with all columns
# write_csv(flora_with_comments, "pubpeer_comments.csv")

# Submission file - just DOI and comment
# flora_with_comments |>
#   select(doi_o, comment_text) |>
#   write_csv("pubpeer_comments_submission.csv")

# cat("Done! Generated", nrow(flora_with_comments), "comments.\n")