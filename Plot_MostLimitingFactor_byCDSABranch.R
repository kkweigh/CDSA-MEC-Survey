# =====================================================================
# "Most limiting barrier per respondent (ties split)" — by branch
# Clustered bar graph: barriers on x, the four branches within each
# cluster. Bars = % of that branch's respondents naming the barrier
# their top barrier (ties split). Built from the raw response file.
# =====================================================================

save_png <- TRUE     # also write fig_most_limiting_by_branch.png ?
txt_cex  <- 0.9      # single text size used for ALL text on the plot

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. branch (drop the no-branch 'None' rows) ------------------
branch_col <- grep("territory branch", names(resp), ignore.case = TRUE, value = TRUE)[1]
if (is.na(branch_col)) stop("Could not find the territory-branch column.")
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

dat <- data.frame(branch = resp[[branch_col]], stringsAsFactors = FALSE)
disp_names <- character(0)
for (col in obs_cols) {
  sub <- sub(".*\\[(.*)\\].*", "\\1", col)
  lab <- label_for(sub)
  if (is.na(lab)) next
  dat[[lab]] <- to_int(resp[[col]])
  disp_names <- c(disp_names, lab)
}
dat <- dat[dat$branch %in% branches, , drop = FALSE]

## ---- 4. most-limiting % (ties split) for a set of respondents ----
ml_pct <- function(M) {                    # M: respondents x barriers
  credit <- setNames(numeric(ncol(M)), colnames(M)); n <- 0L
  for (i in seq_len(nrow(M))) {
    row <- M[i, ]; if (all(is.na(row))) next
    n <- n + 1L; mx <- max(row, na.rm = TRUE); tied <- which(row == mx)
    credit[tied] <- credit[tied] + 1 / length(tied)
  }
  100 * credit / n
}

pctM <- sapply(branches, function(b) ml_pct(as.matrix(dat[dat$branch == b, disp_names])))
pooled <- ml_pct(as.matrix(dat[, disp_names]))            # all four branches combined
ord    <- names(sort(pooled, decreasing = TRUE))          # order barriers high -> low
mat    <- t(pctM[ord, branches])                          # rows = branches, cols = barriers
poolV  <- pooled[ord]

## ---- 5. figure (base R clustered bars) ---------------------------
cols <- c("North Side Blue Line" = "#0072B2",   # blue
          "North Side Red Line"  = "#E69F00",   # orange
          "South Side"           = "#009E73",   # green
          "West Cook"            = "#F0E442")    # yellow

legend_row <- function(items, y, cex) {          # one centered row in the (0..1) legend panel
  sw <- 0.026; gap <- 0.010; itemgap <- 0.020
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
  layout(matrix(c(1, 2), nrow = 2), heights = c(5, 1.5))   # bars on top, legend below
  
  ## panel 1: clustered bars
  par(mar = c(8, 5, 4, 2), mgp = c(3, 1.3, 0))
  bp <- barplot(mat, beside = TRUE, col = cols[branches], border = "black",
                ylim = c(0, max(mat, na.rm = TRUE) * 1.18),
                names.arg = rep("", ncol(mat)), las = 1,
                cex.axis = txt_cex, cex.lab = txt_cex, cex.main = txt_cex,
                ylab = "% of branch's respondents naming it their top barrier",
                main = "Most limiting barrier per respondent, by branch")
  rng <- diff(par("usr")[3:4])
  # x-axis: full-width baseline (reaches the y-axis), then group ticks
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = colMeans(bp), labels = FALSE)
  # overall (all-branch) reference line across each barrier's group
  for (j in seq_len(ncol(mat)))
    segments(min(bp[, j]) - 0.5, poolV[j], max(bp[, j]) + 0.5, poolV[j], lwd = 2.4)
  # angled barrier labels below the axis
  text(colMeans(bp), par("usr")[3] - 0.03 * rng, labels = ord,
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
  
  ## panel 2: legend, centered below, stacked in rows
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  legend_row(list(list(type = "line", label = "Overall (all branches)")), 0.82, txt_cex)
  legend_row(list(list(type = "box", col = cols["North Side Blue Line"], label = "North Side Blue Line"),
                  list(type = "box", col = cols["North Side Red Line"],  label = "North Side Red Line")),
             0.50, txt_cex)
  legend_row(list(list(type = "box", col = cols["South Side"], label = "South Side"),
                  list(type = "box", col = cols["West Cook"],  label = "West Cook")),
             0.18, txt_cex)
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_most_limiting_by_branch.png", width = 1600, height = 820, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_most_limiting_by_branch.png"), "\n")
}
print(round(rbind(`overall` = poolV, t(mat)), 1))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))