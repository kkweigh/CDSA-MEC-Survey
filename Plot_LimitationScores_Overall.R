# =====================================================================
# "Barriers ranked by mean rating, with significance of each step"
# Built directly from the raw survey response file.
#
# Barriers ranked high->low by mean 1-5 limitation rating; each barrier
# is tested against the one ranked just below it with a paired Wilcoxon
# signed-rank test (same respondents), Holm-corrected across the steps.
# =====================================================================

save_png <- TRUE     # also write fig_obstacle_ranking.png ?
txt_cex  <- 0.9      # single text size used for ALL text on the plot

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. obstacle matrix: parse "N (label)" -> integer 1..5 -------
stem     <- "To what extent do the following factors limit"
obs_cols <- names(resp)[startsWith(names(resp), stem)]
if (length(obs_cols) == 0) stop("Could not find the obstacle matrix columns.")

# '*' marks the two items not shown to the earliest respondents (n = 185)
label_for <- function(sub) {
  s <- tolower(sub)
  if (grepl("free time", s))              return("Limited free time")
  if (grepl("schedule conflict", s))      return("Schedule conflicts")
  if (grepl("work/organization", s))      return("Overwhelmed by org")
  if (grepl("mental health", s))          return("Mental health")
  if (grepl("state of the country", s))   return("Overwhelmed by country")
  if (grepl("social anxiety", s))         return("Social anxiety")
  if (grepl("racial and ethnic", s))      return("Racial/ethnic diversity*")
  if (grepl("childcare", s))              return("Childcare/domestic")
  if (grepl("physical accessibility", s)) return("Physical accessibility*")
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

## ---- 3. means + adjacent paired Wilcoxon (Holm) ------------------
means <- sapply(disp_names, function(o) mean(dat[[o]], na.rm = TRUE))
ord   <- names(sort(means, decreasing = TRUE))
vals  <- means[ord]

pvals <- numeric(length(ord) - 1)
for (i in seq_len(length(ord) - 1)) {
  a <- dat[[ord[i]]]; b <- dat[[ord[i + 1]]]
  ok <- !is.na(a) & !is.na(b)
  pvals[i] <- wilcox.test(a[ok], b[ok], paired = TRUE, exact = FALSE)$p.value
}
holm <- p.adjust(pvals, method = "holm")
stars <- function(p) if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else "ns"

## ---- 4. figure (base R) -----------------------------------------
draw_plot <- function() {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(9, 5, 2, 2), mgp = c(3, 1.3, 0))    # mgp[2]=1.3 -> y tick/label bumper
  bp <- barplot(vals, col = "#0072B2", border = "black", las = 1,
                ylim = c(0, 5), names.arg = rep("", length(vals)),
                cex.axis = txt_cex, cex.lab = txt_cex,
                ylab = "Average limitation score (1-5)",
                main= "Limiting factors for CDSA participation")
  rng <- diff(par("usr")[3:4])
  # x-axis: full-width baseline (reaches the y-axis), then ticks under bars
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = bp, labels = FALSE)
  # significance bracket between each adjacent pair
  for (i in seq_len(length(vals) - 1)) {
    y <- max(vals[i], vals[i + 1]) + 0.22
    x1 <- bp[i]; x2 <- bp[i + 1]
    segments(x1, y, x2, y, lwd = 0.8)
    segments(x1, y, x1, y - 0.06, lwd = 0.8); segments(x2, y, x2, y - 0.06, lwd = 0.8)
    text((x1 + x2) / 2, y + 0.02, stars(holm[i]), adj = c(0.5, 0), cex = txt_cex)
  }
  # angled barrier labels below the axis
  text(bp, par("usr")[3] - 0.03 * rng, labels = ord,
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
  # caption
  #mtext("Brackets test each obstacle vs. the next-ranked one (paired Wilcoxon signed-rank, Holm-corrected).   * p<.05   ** p<.01   *** p<.001   ns = not significant.",
  #      side = 1, line = 7.6, adj = 0, cex = txt_cex * 0.7, font = 3, col = "#555555")
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_obstacle_ranking.png", width = 1500, height = 820, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_obstacle_ranking.png"), "\n")
}
print(round(rbind(mean = vals, `p vs next (Holm)` = c(holm, NA)), 3))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))