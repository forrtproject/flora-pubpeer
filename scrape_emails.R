library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(httr)

decode_email <- function(string) {
  r <- strtoi(substr(string, 1, 2), base = 16)
  string <- substr(string, 3, nchar(string))
  email <- character(0)
  for (i in seq(1, nchar(string), by = 2)) {
    ascii_value <- strtoi(substr(string, i, i+1), base = 16)
    xor_value <- bitwXor(ascii_value, r)
    email <- paste0(email, rawToChar(as.raw(xor_value)))
  }
  return(email)
}

vectorized_decode <- Vectorize(decode_email)

build_proxy_url <- function(url, api_key = NULL, which = c("scrapedo", "scraperapi", "scrapeops")) {

  if (length(which) == 1 && is.numeric(which)) {
    scraper_options <- c("scrapedo", "scraperapi", "scrapeops")
    which <- scraper_options[(which - 1) %% length(scraper_options) + 1]
  }

  if (which[[1]] == "scrapedo") {
    proxy_url <- paste0("http://api.scrape.do?token=bf824f3d704545fb88f87eb3b10103e8f432282856a",
                        "&url=",
                        URLencode(url, reserved = TRUE, repeated = TRUE))
                        #"&render=true")
    message("proxy URL: ", proxy_url)
  }  else if (which[[1]] == "scraperapi") {
  proxy_url <- paste0("http://api.scraperapi.com/?api_key=",
                      SCRAPEOPS_API_KEY,
                      "&url=",
                      URLencode(url, reserved = TRUE, repeated = TRUE))
                      #"&render=true")
  message("proxy URL: ", proxy_url)
  } else if (which[[1]] == "scrapeops") {
    proxy_url <- paste0("https://proxy.scrapeops.io/v1/?api_key=f49620a1-3cc1-47a9-9521-905d71058d09",
                                     "&url=",
                                     URLencode(url, reserved = TRUE, repeated = TRUE))
                                     #"&render_js=true")

    message("proxy URL: ", proxy_url)
  }

  return(proxy_url)
}

navigate_to <- function(target_url, attempts = 0, proxy = FALSE) {
  orig_url <- target_url
  if (proxy) target_url <- build_proxy_url(target_url, which = attempts + 1)
  lp <- possibly(rvest::read_html_live, otherwise = NULL)(target_url)
  if (is.null(lp) ||
      str_detect(lp$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value, fixed("Request failed")) ||
      str_detect(lp$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
                 regex("please try again", ignore_case = TRUE)) ||
      str_detect(lp$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
                 regex("please retry the request", ignore_case = TRUE))) {
    if (attempts < 3) {
      Sys.sleep(1)
      navigate_to(orig_url, attempts + 1, proxy = TRUE)
    } else {
      message("proxy failed")
      return(NULL)
    }
  } else {
    return(lp)
  }
}

library(httr)
library(rvest)
library(stringr)
library(tibble)
library(chromote)

get_email_main <- function(doi_or_url,
                           scrapeops_api_key = NULL,
                           unpaywall_email = NULL,
                           quiet = FALSE,
                           force_chromote = FALSE,
                           force_proxy = FALSE,
                           chromote_timeout = 120,
                           debug_if_none = TRUE) {

  chrome <- chromote::default_chromote_object()
  chrome$default_timeout <- chromote_timeout
  chromote::set_default_chromote_object(chrome)

  # Predefined vectors for special handling
  chromote_needed <- c("linkinghub.elsevier.com", "sciencedirect.com", "academic.oup.com")
  proxy_needed <- "TKTK"  # Currently not needed
  no_proxy <- c("osf.io")

  # Helper function to extract emails from an HTML page
  extract_emails <- function(page) {
    # Extract 'mailto:' links
    mailto_links <- page %>%
      html_nodes("a[href^='mailto']") %>%
      html_attr("href") %>%
      str_remove("^mailto:") %>%
      unique()

    # Extract and decode email-protected links
    email_protection_links <- page %>%
      html_nodes("a[href*='email-protection#']") %>%
      html_attr("href") %>%
      str_replace_all("^.+email-protection#", "") %>%
      vectorized_decode() %>%
      unique()

    # Combine both email types
    all_emails <- c(mailto_links, email_protection_links)
    all_emails <- unique(na.omit(all_emails)) %>% unlist()
    return(all_emails)
  }



  # Helper function to navigate and handle special domains using Chromote
  chromote_fetch <- function(url, use_proxy = FALSE) {



    # Modify URL to use proxy if required
    if (use_proxy && !is.null(scrapeops_api_key)) {
      live_page <- navigate_to(url, proxy = FALSE)
      current_url <- tryCatch({
        live_page$session$Runtime$evaluate("window.location.href")$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve current URL: ", e$message)
        return(url)
      })
      live_page <- navigate_to(current_url, proxy = TRUE)
    } else {
      live_page <- navigate_to(url)
    }

    # Retrieve the page HTML
    page_html <- tryCatch({
      live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    }, error = function(e) {
      if (!quiet) warning("Failed to retrieve page HTML: ", e$message)
      return(NULL)
    })

    if (is.null(page_html)) {
      return(NULL)
    }

    if (!str_detect(page_html, fixed("<body"))) {
      Sys.sleep(2)
      page_html <- tryCatch({
        live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve page HTML: ", e$message)
        return(NULL)
      })
    }

    # Check for anti-scraping protections
    protection_detected <- FALSE
    if (str_detect(page_html, regex("just a moment", ignore_case = TRUE)) ||
        str_detect(page_html, "Request unsuccessful\\. Incapsula") ||
        str_detect(page_html, "There was a problem providing the content you requested")) {
      protection_detected <- TRUE
    }

    # If protection is detected and proxy is allowed, retry with proxy
    if (protection_detected && !use_proxy && !is.null(scrapeops_api_key)) {
      current_url <- tryCatch({
        live_page$session$Runtime$evaluate("window.location.href")$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve current URL: ", e$message)
        return(url)
      })

      message("Protection detected. Retrying with ScrapeOps proxy.\nOriginal URL: ", url,
              "\nCurrent URL: ", current_url)

      live_page <- navigate_to(current_url, proxy = TRUE)
      proxy_used <<- TRUE

      page_html <- tryCatch({
        live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve page HTML after proxy: ", e$message)
        return(NULL)
      })

      if (is.null(page_html)) {
        return(NULL)
      }

      # Re-check for anti-scraping protections
      protection_detected <- FALSE
      if (str_detect(page_html, regex("just a moment", ignore_case = TRUE)) ||
          str_detect(page_html, "Request unsuccessful\\. Incapsula") ||
          str_detect(page_html, "There was a problem providing the content you requested")) {
        protection_detected <- TRUE
      }

      if (protection_detected) {
        warning("Protection still detected after using ScrapeOps proxy.")
        return(NULL)
      }
    }

    url <- tryCatch({
      live_page$session$Runtime$evaluate("window.location.href")$result$value
    }, error = function(e) {
      if (!quiet) warning("Failed to retrieve current URL: ", e$message)
      return(url)
    })

    # Handle special domains requiring clicks
    if (str_detect(url, "sciencedirect.com")) {

      # Click the accept recommendations button
      tryCatch({
        live_page$session$Runtime$evaluate("document.querySelector('#accept-recommended-btn-handler').click()")
        Sys.sleep(2)  # Wait for the click action to take effect
      }, error = function(e) {
        if (!quiet) warning("Failed to click accept recommendations button on ScienceDirect: ", e$message)
      })

      # Find all envelope icons
      num_envelopes <- tryCatch({
        live_page$session$Runtime$evaluate('document.querySelectorAll("svg.icon-envelope").length')$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve envelope icons on ScienceDirect: ", e$message)
        return(0)
      })

      if (num_envelopes > 0) {
        message(sprintf("Found %d envelope icons on ScienceDirect page.", num_envelopes))
        emails_after_click <- c()

        for (i in 0:(num_envelopes - 1)) {  # JavaScript indices start at 0
          # Click the envelope icon
          click_js <- sprintf('document.querySelectorAll("svg.icon-envelope")[%d].parentElement.click();', i)
          tryCatch({
            live_page$session$Runtime$evaluate(click_js)
            Sys.sleep(2)  # Wait for the click action to load the content

            # Extract emails after each click
            page_html_after_click <- tryCatch({
              live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
            }, error = function(e) {
              if (!quiet) warning("Failed to retrieve page HTML after clicking envelope icon: ", e$message)
              return(NULL)
            })

            if (!is.null(page_html_after_click)) {
              page_after_click <- read_html(page_html_after_click)
              emails_after_click <- c(extract_emails(page_after_click), emails_after_click)
            }
          }, error = function(e) {
            if (!quiet) warning(sprintf("Failed to click envelope icon %d on ScienceDirect: %s", i, e$message))
          })
        }

        live_page$session$close()
        return(emails_after_click)

      }
    } else if (str_detect(url, "academic.oup.com")) {
      message("Clicking author button on OUP page.")
      live_page$session$Runtime$evaluate('document.querySelector(".linked-name.js-linked-name-trigger.btn-as-link").click();')
      Sys.sleep(2)
    }


    # After interactions, retrieve the updated HTML
    final_html <- tryCatch({
      live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    }, error = function(e) {
      if (!quiet) warning("Failed to retrieve final HTML after interactions: ", e$message)
      return(NULL)
    })


    if (is.null(final_html)) {
      return(NULL)
    }

    # Extract emails from the final page
    emails_final <- extract_emails(read_html(final_html))


    if (length(emails_final) > 0) {
      live_page$session$close()

      return(emails_final)
    } else {
      if (debug_if_none) browser() # live_page$session$view()
      live_page$session$close()
      return(NULL)
    }
  }


  # Validate and prepare the URL
  if (is.na(doi_or_url) || doi_or_url == "") {
    if (!quiet) warning("Invalid input: DOI or URL is NA or empty.")
    return(tibble(
      url = NA_character_,
      emails = NA_character_,
      extraction_method = NA_character_
    ))
  }

  if (!str_detect(doi_or_url, fixed("http"))) {
    doi_or_url <- paste0("https://doi.org/", doi_or_url)
  }

  if (!quiet) message("Scraping: ", doi_or_url)

  # Determine if Chromote or Proxy is needed based on URL
  requires_chromote <- any(str_detect(doi_or_url, chromote_needed))
  requires_proxy <- any(str_detect(doi_or_url, proxy_needed))
  skip_proxy <- any(str_detect(doi_or_url, no_proxy))

  use_chromote <- force_chromote || requires_chromote
  use_proxy <- force_proxy || requires_proxy

  # Sequential Attempts as per Specified Order

  # 1. Direct GET unless use_proxy or use_chromote is set
  if (!use_proxy && !use_chromote) {
    if (!quiet) message("Attempting direct GET.")
    page_direct <- tryCatch({
      response_direct <- GET(doi_or_url, add_headers("User-Agent" = "Mozilla/5.0"))
      if (status_code(response_direct) == 200) {
        read_html(content(response_direct, "text"))
      } else {
        if (!quiet) warning("Direct GET failed with status: ", status_code(response_direct))
        NULL
      }
    }, error = function(e) {
      if (!quiet) warning("Direct GET encountered an error: ", e$message)
      return(NULL)
    })

    if (!is.null(page_direct)) {
      emails_direct <- extract_emails(page_direct)
      if (length(emails_direct) > 0) {
        return(tibble(
          url = doi_or_url,
          emails = emails_direct,
          extraction_method = "direct_get"
        ))
      }
    }
  }

  # 2. GET via Proxy unless use_chromote is set
  if (!use_chromote) {
    if (!quiet) message("Attempting GET via Proxy.")
    page_proxy <- tryCatch({
      proxy_url <- build_proxy_url(doi_or_url, scrapeops_api_key)
      response_proxy <- GET(proxy_url, add_headers("User-Agent" = "Mozilla/5.0"))
      if (status_code(response_proxy) == 200) {
        read_html(content(response_proxy, "text"))
      } else {
        if (!quiet) warning("Proxy GET failed with status: ", status_code(response_proxy))
        NULL
      }
    }, error = function(e) {
      if (!quiet) warning("Proxy GET encountered an error: ", e$message)
      return(NULL)
    })

    if (!is.null(page_proxy)) {
      emails_proxy <- extract_emails(page_proxy)
      if (length(emails_proxy) > 0) {
        return(tibble(
          url = doi_or_url,
          emails = emails_proxy,
          extraction_method = "proxy_get"
        ))
      }
    }
  }

  proxy_used <- FALSE

  # 3. Chromote unless use_proxy is set
  if (!use_proxy) {
    if (!quiet) message("Attempting Chromote without Proxy.")
    emails_chromote <- tryCatch({
      fetched_emails <- chromote_fetch(url = doi_or_url, use_proxy = FALSE)
      fetched_emails  # Should be a character vector or NULL
    }, error = function(e) {
      if (!quiet) warning("Chromote fetch without proxy failed: ", e$message)
      return(NULL)
    })

    if (!is.null(emails_chromote) && length(emails_chromote) > 0) {
      return(tibble(
        url = doi_or_url,
        emails = emails_chromote,
        extraction_method = "chromote"
      ))
    }
  }

  # 4. Chromote via Proxy
  if (!skip_proxy && !proxy_used) {
    if (!quiet) message("Attempting Chromote with Proxy.")
    emails_chromote_proxy <- tryCatch({
      fetched_emails <- chromote_fetch(url = doi_or_url, use_proxy = TRUE)
      fetched_emails  # Should be a character vector or NULL
    }, error = function(e) {
      if (!quiet) warning("Chromote fetch with proxy failed: ", e$message)
      return(NULL)
    })

    if (!is.null(emails_chromote_proxy) && length(emails_chromote_proxy) > 0) {

      return(tibble(
        url = doi_or_url,
        emails = emails_chromote_proxy,
        extraction_method = "chromote_proxy"
      ))
    }
  }


  # 5. PDF fallback via Unpaywall (and Sci-Hub)
  if (!quiet) message("Attempting PDF extraction.")
  doi_only <- stringr::str_remove(doi_or_url, "https://doi.org/")
  emails_pdf <- tryCatch(
    get_email_from_pdf(doi_only, unpaywall_email = unpaywall_email, quiet = quiet),
    error = function(e) NULL
  )
  if (!is.null(emails_pdf) && !all(is.na(emails_pdf$emails))) {
    return(emails_pdf)
  }

  # 5b. Unpaywall landing pages — scrape HTML directly for all locations
  if (!is.null(unpaywall_email)) {
    if (!quiet) message("Attempting Unpaywall landing pages.")
    unpaywall_urls <- tryCatch(get_unpaywall_urls(doi_only, unpaywall_email), error = function(e) NULL)
    landing_urls <- unpaywall_urls$landing

    for (landing_url in landing_urls) {
      if (is.null(landing_url) || is.na(landing_url)) next
      if (!quiet) message("Trying landing page: ", landing_url)

      page_landing <- tryCatch({
        resp <- httr::GET(landing_url, httr::add_headers("User-Agent" = "Mozilla/5.0"))
        if (httr::status_code(resp) == 200) rvest::read_html(httr::content(resp, "text")) else NULL
      }, error = function(e) NULL)

      if (!is.null(page_landing)) {
        emails_landing <- extract_emails(page_landing)
        if (length(emails_landing) > 0) {
          return(tibble(url = landing_url, emails = emails_landing,
                        extraction_method = "unpaywall_landing"))
        }
        # No mailto found — try PDF links on the page
        for (pdf_link in extract_pdf_links(page_landing)) {
          pdf_link <- if (!stringr::str_detect(pdf_link, "^http")) paste0(httr::parse_url(landing_url)$scheme, "://", httr::parse_url(landing_url)$hostname, pdf_link) else pdf_link
          if (!quiet) message("Trying PDF link from landing page: ", pdf_link)
          emails_from_link <- emails_from_pdf_url(pdf_link, quiet = quiet)
          if (length(emails_from_link) > 0) {
            return(tibble(url = pdf_link, emails = emails_from_link,
                          extraction_method = "unpaywall_landing_pdf"))
          }
        }
      }

      # If direct GET found nothing, try Chromote on this landing page
      if (!quiet) message("Trying Chromote on landing page: ", landing_url)
      live_page <- tryCatch(navigate_to(landing_url), error = function(e) NULL)
      if (!is.null(live_page)) {
        page_html <- tryCatch(
          live_page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
          error = function(e) NULL
        )
        if (!is.null(page_html)) {
          page_chromote <- rvest::read_html(page_html)
          emails_chromote <- extract_emails(page_chromote)
          if (length(emails_chromote) > 0) {
            return(tibble(url = landing_url, emails = emails_chromote,
                          extraction_method = "unpaywall_landing_chromote"))
          }
          # Try PDF links found by Chromote
          for (pdf_link in extract_pdf_links(page_chromote)) {
            pdf_link <- if (!stringr::str_detect(pdf_link, "^http")) paste0(httr::parse_url(landing_url)$scheme, "://", httr::parse_url(landing_url)$hostname, pdf_link) else pdf_link
            if (!quiet) message("Trying PDF link from Chromote landing page: ", pdf_link)
            emails_from_link <- emails_from_pdf_url(pdf_link, quiet = quiet)
            if (length(emails_from_link) > 0) {
              return(tibble(url = pdf_link, emails = emails_from_link,
                            extraction_method = "unpaywall_landing_chromote_pdf"))
            }
          }
        }
        tryCatch(live_page$session$close(), error = function(e) NULL)
      }
    }
  }

  # If all attempts fail to find emails, return NA with consistent tibble structure
  if (!quiet) message("No emails found for URL: ", doi_or_url)
  return(tibble(
    url = doi_or_url,
    emails = NA_character_,
    extraction_method = NA_character_
  ))
}



get_chromote_html <- function(url, chromote_session, scrapeops_api_key = NULL,
                              force_proxy = FALSE, email_click = TRUE, chromote_timeout = 120) {
  b <- chromote_session
  b$default_timeout <- chromote_timeout

  # Function to navigate to a URL and wait for the page to load
  navigate_to_url <- function(target_url) {
    # Listen for the load event
    load_event <- b$Page$loadEventFired(wait_ = FALSE)
    # Navigate to the URL
    b$Page$navigate(target_url, wait_ = FALSE)
    # Wait for the page to finish loading
    b$wait_for(load_event)
  }

  # Function to check for anti-scraping protections
  check_protection <- function() {
    page <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>%
      read_html()

    cloudflare_check <- page %>%
      html_node("title") %>%
      html_text() %>%
      str_detect(regex("just a moment", ignore_case = TRUE))

    incapsula_check <- page %>%
      html_text() %>%
      str_detect("Request unsuccessful. Incapsula")

    return(cloudflare_check || incapsula_check)
  }

  # Modify URL to route through ScrapeOps proxy if force_proxy is TRUE
  if (force_proxy && !is.null(scrapeops_api_key)) {
    # Identify final destination (as proxy does not like DOI)
    navigate_to_url(url)
    url <- b$Runtime$evaluate("window.location.href")$result$value

    proxy_url <- paste0("http://api.scraperapi.com/?api_key=",
                        scrapeops_api_key,
                        "&url=",
                        URLencode(url),
                        "&render=true")
    navigate_to_url(proxy_url)
  } else {
    navigate_to_url(url)
  }

  # After initial navigation, check if protection is detected
  protection_detected <- check_protection()

  # If protection is detected and force_proxy is FALSE, attempt to use proxy as fallback
  if (protection_detected && !force_proxy && !is.null(scrapeops_api_key)) {
    message("Protection detected. Attempting to use ScrapeOps proxy as fallback.")
    proxy_url <- paste0("http://api.scraperapi.com/?api_key=",
                        scrapeops_api_key,
                        "&url=",
                        URLencode(url),
                        "&render=true")
    navigate_to_url(proxy_url)

    # Re-check for protection after using proxy
    protection_detected <- check_protection()

    if (protection_detected) {
      warning("Protection still detected after using ScrapeOps proxy.")
      # Optionally, you can decide to return NA or proceed
      return(NA)
    }
  }

  # If email_click is TRUE, handle clicking envelope icons
  if (email_click) {
    current_url <- b$Runtime$evaluate("window.location.href")$result$value

    # Handle Elsevier URLs (e.g., linkinghub.elsevier.com)
    if (str_detect(current_url, "linkinghub.elsevier.com")) {
      b$Runtime$evaluate('document.querySelector("div.author-group button").click();')
      Sys.sleep(2)  # Wait for the click action to take effect
    }

    # Handle ScienceDirect URLs (e.g., sciencedirect.com)
    if (str_detect(current_url, "sciencedirect.com")) {

      b$Runtime$evaluate("document.querySelector('#accept-recommended-btn-handler').click()")

      num_envelopes <- b$Runtime$evaluate('document.querySelectorAll("svg.icon-envelope").length')$result$value

      if (num_envelopes > 0) {
        message(sprintf("Found %d envelope icons on ScienceDirect page.", num_envelopes))

        envelope_pages <- list()

        for (i in 0:(num_envelopes - 1)) {  # JavaScript indices start at 0
          # Click the envelope icon
          click_js <- sprintf('document.querySelectorAll("svg.icon-envelope")[%d].parentElement.click();', i)
          b$Runtime$evaluate(click_js)

          Sys.sleep(2)  # Wait for the click action to load the content

          # Extract the updated HTML after each click
          page_html <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value

          envelope_pages[[i + 1]] <- read_html(page_html)
        }

        return(envelope_pages)  # Return all envelope pages as a list
      }
    }

    # Handle Oxford University Press (OUP) URLs
    if (str_detect(current_url, "academic.oup.com")) {
      message("Clicking author button on OUP page.")
      b$Runtime$evaluate('document.querySelector(".linked-name.js-linked-name-trigger.btn-as-link").click();')
      Sys.sleep(2)  # Wait for the click action to take effect
    }
  }

  # If no envelope icons were found or email_click is FALSE, return the current page
  page_html <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  page <- list(read_html(page_html))  # Always return a list

  return(page)
}

get_email <- function(doi_or_url,
                      chromote_session = NULL,
                      scrapeops_api_key = NULL,
                      quiet = FALSE,
                      force_chromote = FALSE,
                      force_proxy = FALSE,
                      chromote_timeout = 120) {



  # Validate input
  if (is.na(doi_or_url) || doi_or_url == "") return(tibble())

  # Convert DOI to URL if necessary
  if (!str_detect(doi_or_url, fixed("http"))) {
    doi_or_url <- paste0("https://doi.org/", doi_or_url)
  }

  if (!quiet) message("Scraping: ", doi_or_url)

  # If force_proxy is TRUE and force_chromote is FALSE, modify the URL for GET
  if (force_proxy && !force_chromote && !is.null(scrapeops_api_key)) {
    doi_or_url <- paste0("http://api.scraperapi.com/?api_key=",
                         scrapeops_api_key,
                         "&url=",
                         URLencode(doi_or_url),
                         "&render=true")
  }

  started_chromote <- FALSE

  # Initialize Chromote session if force_chromote is TRUE
  if (force_chromote && is.null(chromote_session)) {
    started_chromote <- TRUE
    chromote_session <- ChromoteSession$new()
  }

  used_chromote <- FALSE
  pages <- list()

  # If force_chromote is TRUE, use Chromote for scraping
  if (force_chromote) {
    used_chromote <- TRUE

    # Use Chromote to get HTML, passing the force_proxy argument
    pages <- get_chromote_html(url = doi_or_url,
                               chromote_session = chromote_session,
                               scrapeops_api_key = scrapeops_api_key,
                               force_proxy = force_proxy,
                               email_click = TRUE,
                               chromote_timeout = chromote_timeout)

    # If no pages were returned (e.g., due to protection), return NA
    if (all(is.na(pages))) {
      return(NA)
    }
  } else {
    # Attempt a direct HTTP GET request (using the modified URL if force_proxy is TRUE)
    response <- httr::GET(doi_or_url, httr::add_headers("User-Agent" = "Mozilla/5.0"))

    if (response$status_code == 200) {
      # If GET is successful, parse the response
      page_html <- read_html(response)
      pages <- list(page_html)  # Always return a list
    } else if (response$status_code == 403 && !force_proxy && !is.null(scrapeops_api_key)) {
      # If 403 forbidden and proxy fallback is available, retry with ScrapeOps proxy
      doi_or_url <- paste0("http://api.scraperapi.com/?api_key=",
                           scrapeops_api_key,
                           "&url=",
                           URLencode(doi_or_url),
                           "&render=true")
      response <- httr::GET(doi_or_url, httr::add_headers("User-Agent" = "Mozilla/5.0"))

      if (response$status_code == 200) {
        page_html <- read_html(response)
        pages <- list(page_html)
      } else {
        warning("Unexpected HTTP response status after fallback: ", response$status_code)
        return(NA)
      }
    } else {
      warning("Unexpected HTTP response status: ", response$status_code)
      return(NA)
    }
  }


  # Initialize a vector to store all emails
  all_emails <- c()

  # Loop through each page in the list and extract emails
  for (page in pages) {
    # Extract 'mailto:' links
    mailto_links <- page %>%
      html_nodes("a[href^='mailto']") %>%
      html_attr("href") %>%
      str_remove("^mailto:") %>%
      unique()


    message(mailto_links)

    # Extract and decode email-protected links (if applicable)
    email_protection_links <- page %>%
      html_nodes("a[href*='email-protection#']") %>%
      html_attr("href") %>%
      str_replace_all("^.+email-protection#", "") %>%
      vectorized_decode() %>%
      unique()

    # Combine both email types
    emails <- c(mailto_links, email_protection_links)
    all_emails <- c(all_emails, emails)
  }

  # Remove duplicates and NAs
  all_emails <- all_emails %>% unlist() %>% unique() %>% na.omit()

  # If no emails were found, return NA
  if (length(all_emails) == 0) {
    return(tibble(url = doi_or_url, emails = NA_character_))
  }

  # Return a tidy dataframe with the extracted emails
  email_df <- tibble(
    url = doi_or_url,
    emails = all_emails %>% unlist()
  )

  if (started_chromote) {
    chromote_session$close()
  }

  return(email_df)
}



extract_pdf_links <- function(page) {
  links <- page %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  links <- links[!is.na(links)]
  unique(links[stringr::str_detect(links, stringr::regex("\\.pdf|/pdf|pdf/|download", ignore_case = TRUE))])
}

emails_from_pdf_url <- function(pdf_url, quiet = FALSE) {
  pdf_file <- tempfile(fileext = ".pdf")
  tryCatch({
    download.file(pdf_url, pdf_file, mode = "wb", quiet = quiet)
    text <- pdftools::pdf_text(pdf_file) %>% paste(collapse = " ")
    emails <- stringr::str_extract_all(text, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
    unique(emails)
  }, error = function(e) {
    if (!quiet) message("PDF extraction failed (", pdf_url, "): ", e$message)
    character(0)
  })
}

get_unpaywall_urls <- function(doi, email) {
  tryCatch({
    resp <- httr::GET(paste0("https://api.unpaywall.org/v2/", doi, "?email=", email))
    if (httr::status_code(resp) != 200) return(NULL)
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    locs <- data$oa_locations
    pdfs    <- unique(Filter(Negate(is.null), locs$url_for_pdf[!is.na(locs$url_for_pdf)]))
    landing <- unique(Filter(Negate(is.null), locs$url_for_landing_page[!is.na(locs$url_for_landing_page)]))
    list(pdf = pdfs, landing = landing)
  }, error = function(e) NULL)
}

get_email_from_pdf <- function(doi_or_url, unpaywall_email = NULL, scrapeops_api_key = NULL, quiet = FALSE) {
  if (!quiet) message("Scraping PDF: ", doi_or_url)

  pdf_file <- tempfile(fileext = ".pdf")
  downloaded <- FALSE

  # Try all Unpaywall PDF URLs if email provided and input looks like a DOI
  if (!is.null(unpaywall_email) && !stringr::str_detect(doi_or_url, stringr::fixed("http"))) {
    if (!quiet) message("Trying Unpaywall for: ", doi_or_url)
    pdf_urls <- get_unpaywall_urls(doi_or_url, unpaywall_email)$pdf
    for (pdf_url in pdf_urls) {
      if (!is.null(pdf_url) && !is.na(pdf_url)) {
        tryCatch({
          download.file(pdf_url, pdf_file, mode = "wb", quiet = quiet)
          downloaded <- TRUE
          if (!quiet) message("Downloaded PDF via Unpaywall: ", pdf_url)
          break
        }, error = function(e) {
          if (!quiet) message("Unpaywall PDF download failed (", pdf_url, "): ", e$message)
        })
      }
      if (downloaded) break
    }
  }

  if (!downloaded && !stringr::str_detect(doi_or_url, stringr::fixed("http"))) {
    url <- paste0("https://sci-hub.se/", doi_or_url)

    response <- purrr::possibly(httr::GET)(url, httr::add_headers("User-Agent" = "Mozilla/5.0"))

    if (is.null(response) || response$status_code == 403) {
      live_response <- navigate_to(url, proxy = TRUE)
      response <- tryCatch({
        live_response$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      }, error = function(e) {
        if (!quiet) warning("Failed to retrieve page HTML: ", e$message)
        return(NULL)
      })
    } else {
      response <- rvest::read_html(response)
    }

    if (str_detect(response, "Scientific mutual aid community")) {
      message("Sci-Hub does not have: ", doi_or_url)
      return(tibble(doi_or_url = doi_or_url,
                    emails = NA_character_,
                    status = "not on SciHub"))
    }

    paper_url_raw <- response %>%
      xml2::xml_find_all("//*[@id='pdf']") %>%
      xml2::xml_attr("src") %>%
      httr::parse_url()

    unescape_html <- function(str) {
      lapply(str, function(x) {
        xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))
      })
    }

    paper_url_raw$scheme <- "https"
    paper_url_raw$fragment <- NULL

    paper_url <- paper_url_raw %>% httr::build_url()

    readr::read_file_raw(paper_url) %>% writeBin(pdf_file)
    downloaded <- TRUE

  } else if (!downloaded && stringr::str_detect(doi_or_url, "osf.io")) {
    doi_or_url <- paste0(doi_or_url, "/download")
    download.file(doi_or_url, pdf_file, mode = "wb")
    downloaded <- TRUE
  }

  if (!downloaded) {
    return(tibble::tibble(url = doi_or_url, emails = NA_character_, extraction_method = NA_character_))
  }

  pdf_text <- pdftools::pdf_text(pdf_file)

  pdf_text_flat <- paste(pdf_text, collapse = " ")

  emails <- stringr::str_extract_all(pdf_text_flat, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
  emails <- unique(emails)

  if (length(emails) > 0) {
    email_df <- tibble::tibble(url = doi_or_url, emails = emails, extraction_method = "pdf")
  } else {
    email_df <- tibble::tibble(url = doi_or_url, emails = NA_character_, extraction_method = NA_character_)
  }
  email_df
}
