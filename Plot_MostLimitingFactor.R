# =====================================================================
# "Most limiting barrier per respondent (ties split)" — % of respondents
# Built directly from the raw survey response file.
#
# For each respondent, their highest-rated barrier is their "top" barrier;
# when several tie at that respondent's maximum, one vote is split equally
# among them. Bars show each barrier's share of respondents.
# =====================================================================

save_png <- TRUE     # also write fig_most_limiting.png ?
txt_cex  <- 0.9      # single text size used for ALL text on the plot

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. obstacle matrix: parse "N (label)" -> integer 1..5 -------
stem     <- "To what extent do the following factors limit"
obs_cols <- names(resp)[startsWith(names(resp), stem)]
if (length(obs_cols) == 0) stop("Could not find the obstacle matrix columns.")

label_for <- function(sub) {
  s <- tolower(sub)
  if (grepl("free time", s))              return("Limited free time")
  if (grepl("schedule conflict", s))      return("Schedule conflicts")
  if (grepl("work/organization", s))      return("Overwhelmed by organization")
  if (grepl("mental health", s))          return("Mental health")
  if (grepl("state of the country", s))   return("Overwhelmed by country")
  if (grepl("social anxiety", s))         return("Social anxiety")
  if (grepl("racial and ethnic", s))      return("Racial/ethnic diversity")
  if (grepl("childcare", s))              return("Childcare/domestic")
  if (grepl("physical accessibility", s)) return("Physical accessibility")
  if (grepl("safety concern", s))         return("Safety concerns")
  if (grepl("not interested", s))         return("Not interested")
  NA_character_
}
to_int <- function(x) suppressWarnings(as.numeric(sub("^\\s*([0-9]).*$", "\\1", x)))

dat <- data.frame(row.names = seq_len(nrow(resp)))
disp_names <- character(0)
for (col in obs_cols) {
  sub <- sub(".*\\[(.*)\\].*", "\\1", col)
  lab <- label_for(sub)
  if (is.na(lab)) next
  dat[[lab]] <- to_int(resp[[col]])
  disp_names <- c(disp_names, lab)
}

## ---- 3. most-limiting credit (ties split), as % of respondents ----
Obs    <- as.matrix(dat[, disp_names])
credit <- setNames(numeric(length(disp_names)), disp_names)
n_used <- 0L
for (i in seq_len(nrow(Obs))) {
  row <- Obs[i, ]
  if (all(is.na(row))) next
  n_used <- n_used + 1L
  mx   <- max(row, na.rm = TRUE)
  tied <- which(row == mx)                 # which() ignores the NA comparisons
  credit[tied] <- credit[tied] + 1 / length(tied)
}
pct <- 100 * credit / n_used
ord <- names(sort(pct, decreasing = TRUE))
vals <- pct[ord]

## ---- 4. figure (base R) -----------------------------------------
draw_plot <- function() {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  # mgp[2] = 1.3 -> extra bumper between y-axis ticks and their labels
  par(mar = c(8, 5, 4, 2), mgp = c(3, 1.3, 0))
  bp <- barplot(vals, col = "#0072B2", border = "black", las = 1,
                ylim = c(0, max(vals) * 1.18), names.arg = rep("", length(vals)),
                cex.axis = txt_cex, cex.lab = txt_cex, cex.main = txt_cex,
                ylab = "Respondents (%) scoring as most limiting",
                main = "Factor resulting in greatest limitation (per respondent)")
  rng <- diff(par("usr")[3:4])
  # x-axis: full-width baseline (reaches the y-axis), then ticks under bars
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = bp, labels = FALSE)
  # value labels above bars
  text(bp, vals + 0.015 * rng, sprintf("%.1f%%", vals), adj = c(0.5, 0), cex = txt_cex)
  # angled barrier labels below the axis
  text(bp, par("usr")[3] - 0.03 * rng, labels = ord,
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_most_limiting.png", width = 1500, height = 780, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_most_limiting.png"), "\n")
}
print(round(rbind(votes = credit[ord], pct = vals), 2))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))