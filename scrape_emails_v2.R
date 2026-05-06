# scrape_emails_v2.R — API-first layers to run BEFORE the existing scraper.
#
# Layers, in order:
#   1. Europe PMC      — author affiliations + full-text XML <email> tags.
#                        High yield for biomed and any paper indexed in EPMC.
#   2. Citation meta   — direct GET, parse <meta name="citation_author_email">
#                        plus a fallback email regex over the full HTML.
#                        Works on Springer, Nature, Frontiers, PLOS, BMC,
#                        sometimes Wiley/Sage/T&F when not behind a 403.
#   3. OpenAlex+ORCID  — pull all author ORCIDs from OpenAlex, then query the
#                        ORCID public API for any publicly-listed emails.
#                        Recovers cases where the paper itself yields nothing.
#   4. Crossref        — kept as a near-no-cost final check (rarely helps).
#
# Each layer returns a tibble(emails, source). The orchestrator stops as soon
# as one layer returns at least one email, but you can also run all layers and
# union the results — controlled by `mode = c("first", "union")`.
#
# Set CONTACT_EMAIL to your address: it goes into Mailto/User-Agent for
# Crossref/OpenAlex polite-pool routing (faster + higher rate limits).

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(xml2); library(rvest)
  library(stringr); library(purrr); library(tibble); library(dplyr)
})

CONTACT_EMAIL <- Sys.getenv("CONTACT_EMAIL", "lukas.wallrich@gmail.com")
USER_AGENT <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
EMAIL_RE <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.browser_headers <- function() {
  add_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Sec-Ch-Ua" = '"Chromium";v="120", "Not(A:Brand";v="24", "Google Chrome";v="120"',
    "Sec-Ch-Ua-Mobile" = "?0",
    "Sec-Ch-Ua-Platform" = '"macOS"',
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none",
    "Sec-Fetch-User" = "?1",
    "Upgrade-Insecure-Requests" = "1"
  )
}

.get <- function(url, ..., timeout_s = 20) {
  tryCatch(
    GET(url, ..., timeout(timeout_s), user_agent(USER_AGENT)),
    error = function(e) NULL
  )
}

.clean_emails <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  # Trim trailing punctuation that EPMC affiliation strings sometimes carry
  x <- str_replace(x, "[\\.;,)]+$", "")
  # Drop obvious noise/boilerplate
  x[!str_detect(x, regex(
    "(example\\.com|sentry\\.io|wixpress|noreply|donotreply|@2x|@1x|sample@|journalpermissions@|permissions@|reprints@|customerservice@|subscriptions@|info@frontiers|webmaster@)",
    ignore_case = TRUE))]
}

# ---- 1. Europe PMC ---------------------------------------------------------

email_from_epmc <- function(doi) {
  q <- URLencode(paste0("DOI:\"", doi, "\""))
  r <- .get(paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", q,
                   "&format=json&resultType=core"))
  if (is.null(r) || status_code(r) != 200) return(tibble(emails = character(), source = character()))
  j <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  res <- j$resultList$result %||% list()
  if (length(res) == 0) return(tibble(emails = character(), source = character()))
  hit <- res[[1]]

  found <- character()
  # Affiliation strings — emails often appear at the end of the affiliation line
  for (a in hit$authorList$author %||% list()) {
    affs <- a$authorAffiliationDetailsList$authorAffiliation %||% list()
    for (af in affs) {
      found <- c(found, str_extract_all(af$affiliation %||% "", EMAIL_RE)[[1]])
    }
    found <- c(found, str_extract_all(a$affiliation %||% "", EMAIL_RE)[[1]])
  }
  found <- c(found, str_extract_all(hit$correspAffiliation %||% "", EMAIL_RE)[[1]])
  src <- if (length(found) > 0) rep("epmc_meta", length(found)) else character()

  # Full-text XML if available (PMC OA) — usually contains <email> tags
  if (identical(hit$inEPMC, "Y") && !is.null(hit$pmcid)) {
    r2 <- .get(paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/",
                      hit$source %||% "PMC", "/", hit$pmcid, "/fullTextXML"))
    if (!is.null(r2) && status_code(r2) == 200) {
      txt <- content(r2, "text", encoding = "UTF-8")
      ftx <- str_extract_all(txt, EMAIL_RE)[[1]]
      ftx <- setdiff(ftx, found)
      found <- c(found, ftx)
      src <- c(src, rep("epmc_fulltext", length(ftx)))
    }
  }

  out <- .clean_emails(found)
  tibble(emails = out,
         source = src[match(out, found)] %||% rep("epmc", length(out)))
}

# ---- 2. Citation meta tags + raw HTML email regex --------------------------

email_from_meta <- function(doi) {
  url <- paste0("https://doi.org/", doi)
  r <- .get(url, .browser_headers())
  if (is.null(r) || status_code(r) != 200) {
    return(tibble(emails = character(), source = character()))
  }
  txt <- content(r, "text", encoding = "UTF-8")
  pg <- tryCatch(read_html(txt), error = function(e) NULL)
  if (is.null(pg)) return(tibble(emails = character(), source = character()))

  meta_emails <- c(
    pg %>% html_nodes("meta[name='citation_author_email']") %>% html_attr("content"),
    pg %>% html_nodes("meta[name='dc.contributor.email']") %>% html_attr("content"),
    pg %>% html_nodes("meta[name='DC.contributor.email']") %>% html_attr("content")
  )
  mailto <- pg %>%
    html_nodes("a[href^='mailto']") %>% html_attr("href") %>%
    str_remove("^mailto:") %>% str_remove("\\?.*$")

  raw <- str_extract_all(txt, EMAIL_RE)[[1]]

  found <- c(meta_emails, mailto, raw)
  src <- c(rep("meta_tag", length(meta_emails)),
           rep("mailto",   length(mailto)),
           rep("html_regex", length(raw)))
  keep <- !is.na(found) & nzchar(found)
  found <- found[keep]; src <- src[keep]

  cleaned <- .clean_emails(found)
  tibble(emails = cleaned,
         source = src[match(cleaned, found)])
}

# ---- 3. OpenAlex -> ORCID public emails -----------------------------------

email_from_openalex_orcid <- function(doi) {
  r <- .get(paste0("https://api.openalex.org/works/doi:", doi,
                   "?mailto=", CONTACT_EMAIL))
  if (is.null(r) || status_code(r) != 200) {
    return(tibble(emails = character(), source = character()))
  }
  j <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  orcids <- character()
  for (a in j$authorships %||% list()) {
    o <- a$author$orcid
    if (!is.null(o)) orcids <- c(orcids, o)
  }
  orcids <- unique(orcids)
  if (length(orcids) == 0) return(tibble(emails = character(), source = character()))

  found <- character()
  for (o in orcids) {
    oid <- str_replace(o, ".*orcid\\.org/", "")
    r2 <- .get(paste0("https://pub.orcid.org/v3.0/", oid, "/email"),
               add_headers("Accept" = "application/json"))
    if (!is.null(r2) && status_code(r2) == 200) {
      j2 <- fromJSON(content(r2, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      for (e in j2$email %||% list()) {
        if (!is.null(e$email)) found <- c(found, e$email)
      }
    }
  }
  cleaned <- .clean_emails(found)
  tibble(emails = cleaned,
         source = rep("orcid", length(cleaned)))
}

# ---- 4. Crossref (low yield, free) ----------------------------------------

email_from_crossref <- function(doi) {
  r <- .get(paste0("https://api.crossref.org/works/", doi),
            add_headers("Mailto" = CONTACT_EMAIL))
  if (is.null(r) || status_code(r) != 200) {
    return(tibble(emails = character(), source = character()))
  }
  j <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  authors <- j$message$author %||% list()
  found <- character()
  for (a in authors) {
    found <- c(found, str_extract_all(paste(unlist(a), collapse = " "), EMAIL_RE)[[1]])
  }
  cleaned <- .clean_emails(found)
  tibble(emails = cleaned,
         source = rep("crossref", length(cleaned)))
}

# ---- Orchestrator ----------------------------------------------------------

get_email_v2 <- function(doi,
                         mode = c("first", "union"),
                         layers = c("epmc", "meta", "openalex", "crossref"),
                         quiet = TRUE) {
  mode <- match.arg(mode)
  doi <- str_replace(doi, "^https?://(dx\\.)?doi\\.org/", "")

  fns <- list(
    epmc      = email_from_epmc,
    meta      = email_from_meta,
    openalex  = email_from_openalex_orcid,
    crossref  = email_from_crossref
  )

  acc <- tibble(emails = character(), source = character())
  for (layer in layers) {
    if (!quiet) message("  layer: ", layer)
    res <- tryCatch(fns[[layer]](doi), error = function(e) {
      if (!quiet) message("    error: ", e$message)
      tibble(emails = character(), source = character())
    })
    if (nrow(res) > 0) {
      acc <- bind_rows(acc, res) %>% distinct(emails, .keep_all = TRUE)
      if (mode == "first") break
    }
  }

  if (nrow(acc) == 0) {
    return(tibble(doi = doi, emails = NA_character_, source = NA_character_))
  }
  tibble(doi = doi, emails = acc$emails, source = acc$source)
}
