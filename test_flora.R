source("flora_converter.R")

data    <- csv_to_flora_json("flora.csv")
results <- data$results

cat("Total original papers:", length(results), "\n")

doi   <- "10.1177/0956797610383437"
paper <- results[[doi]]

cat("\nSample paper:", paper$title, "\n")
cat("Journal:", paper$journal, "(", paper$year, ")\n")
cat("Authors:", paste(sapply(paper$authors, `[[`, "family"), collapse = ", "), "\n")

cat("\nReplication stats:\n")
for (key in names(paper$record$stats)) {
  cat(sprintf("  %s: %d\n", key, paper$record$stats[[key]]))
}

cat("\nReplications:\n")
for (rep in paper$record$replications) {
  cat(sprintf("  [%s] %s (%s)\n", rep$outcome, rep$title, rep$year))
}
