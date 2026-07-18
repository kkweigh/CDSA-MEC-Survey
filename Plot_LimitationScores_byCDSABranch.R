# =====================================================================
# "Limiting factors for CDSA participation by branch"
# Grouped bar figure built directly from the raw survey response file.
# Branch means per barrier + all-branch average line.
#
# Significance: within each barrier, each branch is compared to the OTHER
# branches pooled (one-sided Mann-Whitney U, alternative = "greater") — i.e.
# does this branch score HIGHER than the rest, and so above the overall
# average. A branch scoring above the others (raw p < 0.10, no multiple-
# comparison correction) is marked with a "*" above its bar. Branches at or
# below the average are never marked. (No brackets: the reference is the
# average line, not a bar. No random numbers: the test is deterministic.)
# =====================================================================

save_png      <- TRUE     # also write fig_branch_vs_pooled.png ?
txt_cex       <- 0.9      # single text size used for ALL text on the plot
sig_threshold <- 0.10     # a bar gets a "*" when its raw p < this

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. branch (already clean; drop the no-branch 'None' rows) ----
branch_col <- grep("territory branch", names(resp), ignore.case = TRUE, value = TRUE)[1]
if (is.na(branch_col)) stop("Could not find the territory-branch column.")
branch   <- resp[[branch_col]]
branches <- c("North Side Blue Line", "North Side Red Line", "South Side", "West Cook")

## ---- 3. obstacle matrix: parse "N (label)" -> integer 1..5 -------
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

dat <- data.frame(branch = branch, stringsAsFactors = FALSE)
disp_names <- character(0)
for (col in obs_cols) {
  sub <- sub(".*\\[(.*)\\].*", "\\1", col)
  lab <- label_for(sub)
  if (is.na(lab)) next
  dat[[lab]] <- to_int(resp[[col]])
  disp_names <- c(disp_names, lab)
}

dat        <- dat[dat$branch %in% branches, , drop = FALSE]
dat$branch <- factor(dat$branch, levels = branches)

## ---- 4. means: per-branch and all-branch average, ordered high->low
overall_mean <- sapply(disp_names, function(o) mean(dat[[o]], na.rm = TRUE))
ord   <- names(sort(overall_mean, decreasing = TRUE))
meanM <- sapply(ord, function(o) tapply(dat[[o]], dat$branch, mean, na.rm = TRUE))
meanM <- meanM[branches, , drop = FALSE]      # 4 branches x 11 barriers
poolV <- overall_mean[ord]                    # all-branch average per barrier

## ---- 5. each branch vs the other branches (does it differ from avg) ----
diff_from_avg <- function(o) {
  v <- dat[[o]]; keep <- !is.na(v)
  vals <- v[keep]; labs <- as.character(dat$branch[keep])
  rawp <- sapply(branches, function(br)
    wilcox.test(vals[labs == br], vals[labs != br], exact = FALSE, alternative = "greater")$p.value)
  rawp < sig_threshold
}
sigM <- sapply(ord, diff_from_avg)            # logical 4 branches x 11 barriers
sigM <- sigM[branches, , drop = FALSE]

## ---- 6. figure (base R grouped bars) -----------------------------
cols <- c("North Side Blue Line" = "#0072B2",   # blue
          "North Side Red Line"  = "#E69F00",   # orange
          "South Side"           = "#009E73",   # green
          "West Cook"            = "#F0E442")    # yellow

# draw one centered row of legend entries in the (0..1) legend panel
legend_row <- function(items, y, cex) {
  sw <- 0.026; gap <- 0.010; itemgap <- 0.020   # itemgap = space between legend items
  wds <- sapply(items, function(it) sw + gap + strwidth(it$label, cex = cex))
  total <- sum(wds) + itemgap * (length(items) - 1)
  x <- 0.5 - total / 2
  for (it in items) {
    if (identical(it$type, "line")) segments(x, y, x + sw, y, lwd = 2.4, col = "black")
    else rect(x, y - 0.055, x + sw, y + 0.055, col = it$col, border = "black")
    text(x + sw + gap, y, it$label, adj = 0, cex = cex)
    x <- x + sw + gap + strwidth(it$label, cex = cex) + itemgap
  }
}

draw_plot <- function() {
  op <- par(no.readonly = TRUE); on.exit({ par(op); layout(1) })
  layout(matrix(c(1, 2), nrow = 2), heights = c(5, 1.6))   # bars on top, legend below
  
  ## panel 1: bars
  par(mar = c(7, 5, 4, 2), mgp = c(3, 1.3, 0))
  bp <- barplot(meanM, beside = TRUE, col = cols[branches], border = "black",
                ylim = c(0, 5), names.arg = rep("", ncol(meanM)), las = 1,
                cex.axis = txt_cex, cex.lab = txt_cex, cex.main = txt_cex,
                ylab = "Average limitation score (1-5)",
                main = "Limiting factors for CDSA participation by branch")
  # x-axis: full-width baseline (reaches the y-axis), then group ticks on top
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = colMeans(bp), labels = FALSE)
  for (j in seq_len(ncol(meanM)))                     # all-branch average line
    segments(min(bp[, j]) - 0.5, poolV[j], max(bp[, j]) + 0.5, poolV[j], lwd = 2.4)
  for (j in seq_len(ncol(meanM)))                     # "*" over bars differing from the average
    for (i in seq_len(nrow(meanM)))
      if (isTRUE(sigM[i, j]))
        text(bp[i, j], meanM[i, j] + 0.12, "*", adj = c(0.5, 0), cex = txt_cex, xpd = NA)
  text(colMeans(bp), par("usr")[3] - 0.12, labels = ord,   # angled x labels
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
  
  ## panel 2: legend, centered below, stacked in rows
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  legend_row(list(list(type = "line", label = "Overall average")), 0.86, txt_cex)
  legend_row(list(list(type = "box", col = cols["North Side Blue Line"], label = "North Side Blue Line"),
                  list(type = "box", col = cols["North Side Red Line"],  label = "North Side Red Line")),
             0.57, txt_cex)
  legend_row(list(list(type = "box", col = cols["South Side"], label = "South Side"),
                  list(type = "box", col = cols["West Cook"],  label = "West Cook")),
             0.28, txt_cex)
  text(0.5, 0.02, "*  branch scores higher than the other branches (p < 0.10)",
       adj = c(0.5, 0), cex = txt_cex)
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_branch_vs_pooled.png", width = 1600, height = 840, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_branch_vs_pooled.png"), "\n")
}
print(round(rbind(`all-branch avg` = poolV, meanM), 2))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))