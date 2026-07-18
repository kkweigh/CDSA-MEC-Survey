# =====================================================================
# "What members get out of Chicago DSA — overall, ranked"
# Built directly from the raw survey response file.
#
# The six outcomes are ranked high->low by % of members selecting them.
# Each bar is compared with the one ranked just below it (McNemar's paired
# test, same members), Holm-corrected across the five consecutive steps.
# A significant step (p < 0.10) gets a "*" bracket; a non-significant
# neighbouring pair is left unmarked.
# =====================================================================

save_png      <- TRUE
txt_cex       <- 0.9
sig_threshold <- 0.10     # a consecutive step gets a bracket when its Holm p < this

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. get-out multi-select ------------------------------------
go_col <- grep("^What do you get out", names(resp), value = TRUE)[1]
if (is.na(go_col)) stop("Could not find the get-out column.")
gv     <- tolower(ifelse(is.na(resp[[go_col]]), "", resp[[go_col]]))
has_go <- nzchar(gv)
n_go   <- sum(has_go)

outs <- list(
  "Political difference (Chicago)" = "making a political difference",
  "Sense of purpose"               = "sense of purpose",
  "Community / friendships"        = "community/friendships",
  "Political education"            = "political education",
  "Organizer training"             = "training and experience",
  "Networking"                     = "networking/professional")
onames <- names(outs)
IND <- sapply(onames, function(o) grepl(outs[[o]], gv, fixed = TRUE) & has_go)

## ---- 3. % selecting + consecutive McNemar (Holm over the steps) --
pct  <- sapply(onames, function(o) 100 * mean(IND[has_go, o]))
ord  <- names(sort(pct, decreasing = TRUE))
vals <- pct[ord]
k    <- length(ord)

rawp <- numeric(k - 1)
for (i in seq_len(k - 1)) {
  a <- IND[has_go, ord[i]]; b <- IND[has_go, ord[i + 1]]
  tab <- matrix(c(sum(a & b), sum(a & !b), sum(!a & b), sum(!a & !b)), nrow = 2, byrow = TRUE)
  rawp[i] <- mcnemar.test(tab, correct = TRUE)$p.value
}
holm <- p.adjust(rawp, method = "holm")
sig  <- holm < sig_threshold

## ---- 4. figure (base R) -----------------------------------------
draw_plot <- function() {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(9, 5, 3, 2), mgp = c(3, 1.3, 0))
  bp <- barplot(vals, col = "#0072B2", border = "black", las = 1,
                ylim = c(0, 100), names.arg = rep("", length(vals)),
                yaxt = "n", cex.axis = txt_cex, cex.lab = txt_cex,
                ylab = "Respondents (%) selecting",
                main = sprintf("What members get out of Chicago DSA  (n = %d)", n_go))
  axis(2, at = seq(0, 100, 25), las = 1, cex.axis = txt_cex)
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = bp, labels = FALSE)
  # "*" bracket over each SIGNIFICANT consecutive step; nothing on tied neighbours
  for (i in seq_len(k - 1)) {
    if (!isTRUE(sig[i])) next
    y <- max(vals[i], vals[i + 1]) + 4
    x1 <- bp[i]; x2 <- bp[i + 1]
    segments(x1, y, x2, y, lwd = 0.9)
    segments(x1, y, x1, y - 1.5, lwd = 0.9); segments(x2, y, x2, y - 1.5, lwd = 0.9)
    text((x1 + x2) / 2, y + 0.4, "*", adj = c(0.5, 0), cex = txt_cex)
  }
  text(bp, par("usr")[3] - 0.03 * diff(par("usr")[3:4]), labels = ord,
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
  mtext("Brackets mark adjacent outcomes that differ significantly (McNemar paired test, Holm-corrected across the 5 steps, p < 0.10);",
        side = 1, line = 7.5, cex = txt_cex * 0.72, font = 3, col = "#555555")
  mtext("neighbouring bars with no bracket are not significantly different.",
        side = 1, line = 8.2, cex = txt_cex * 0.72, font = 3, col = "#555555")
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_members_get_overall.png", width = 1400, height = 820, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_members_get_overall.png"), "\n")
}
print(round(rbind(pct = vals, `Holm p vs next` = c(holm, NA)), 3))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))