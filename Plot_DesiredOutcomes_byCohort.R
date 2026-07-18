# =====================================================================
# "What members get out of Chicago DSA — by first-event cohort"
# Single clustered bar figure built from the raw survey response file.
# Clusters = the six outcomes; within each cluster a grey OVERALL
# reference bar plus the four first-event cohorts. For every outcome the
# four cohorts are compared pairwise (Fisher's exact test on the 2x2
# selected/not table), and SIGNIFICANT pairs are drawn as nested brackets.
# =====================================================================

save_png      <- TRUE          # also write fig_members_get_by_cohort.png ?
txt_cex       <- 0.9           # single text size used for ALL text on the plot
sig_threshold <- 0.10          # a pair gets a bracket when its (adjusted) p < this
correction    <- "holm"        # "holm" = corrected within each outcome; "none" = raw p

## ---- 1. read -----------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

## ---- 2. first-event cohort + get-out multi-select ----------------
fe_col <- grep("^When was the first event", names(resp), value = TRUE)[1]
go_col <- grep("^What do you get out",       names(resp), value = TRUE)[1]
if (is.na(fe_col) || is.na(go_col)) stop("Could not find the first-event or get-out column.")

yr <- as.integer(format(as.Date(resp[[fe_col]], format = "%m/%d/%Y"), "%Y"))
cohort <- ifelse(is.na(yr), NA_character_,
                 ifelse(yr <  2016, "Pre-2016",
                        ifelse(yr <= 2019, "2016-2019",
                               ifelse(yr <= 2023, "2020-2023", "2024-present"))))
groups <- c("Pre-2016", "2016-2019", "2020-2023", "2024-present")

gv     <- tolower(ifelse(is.na(resp[[go_col]]), "", resp[[go_col]]))
has_go <- nzchar(gv)                                   # answered the "what do you get out" item

# outcome label -> distinctive lowercase substring of the option text
outs <- list(
  "Political difference (Chicago)" = "making a political difference",
  "Sense of purpose"               = "sense of purpose",
  "Community / friendships"        = "community/friendships",
  "Political education"            = "political education",
  "Organizer training"             = "training and experience",
  "Networking"                     = "networking/professional")
ind_of <- function(o) grepl(outs[[o]], gv, fixed = TRUE) & has_go

## ---- 3. selection % : overall + per cohort, ordered high->low ----
series <- c("Overall", groups)
onames <- names(outs)
overall_pct <- sapply(onames, function(o) 100 * mean(ind_of(o)[has_go]))
ord <- names(sort(overall_pct, decreasing = TRUE))

mat <- matrix(NA_real_, length(series), length(ord), dimnames = list(series, ord))
for (o in ord) {
  ind <- ind_of(o)
  mat["Overall", o] <- 100 * mean(ind[has_go])
  for (g in groups) {
    m <- has_go & !is.na(cohort) & cohort == g
    mat[g, o] <- 100 * mean(ind[m])
  }
}

## ---- 4. all-pairwise cohort tests within each outcome ------------
pair_idx <- combn(length(groups), 2)                   # 2 x 6 (positions in `groups`)
sig_pairs_for <- function(o) {
  ind <- ind_of(o)
  rawp <- apply(pair_idx, 2, function(pr) {
    ma <- has_go & !is.na(cohort) & cohort == groups[pr[1]]
    mb <- has_go & !is.na(cohort) & cohort == groups[pr[2]]
    tab <- matrix(c(sum(ind[ma]), sum(!ind[ma]), sum(ind[mb]), sum(!ind[mb])), nrow = 2, byrow = TRUE)
    fisher.test(tab)$p.value
  })
  padj <- if (correction == "holm") p.adjust(rawp, method = "holm") else rawp
  lapply(which(padj < sig_threshold), function(k) sort(pair_idx[, k]) + 1L)  # +1: grey bar is pos 1
}

bracket_layout <- function(sig_pairs, heights, step = 6, reserve = 2) {
  occ <- heights
  if (length(sig_pairs) == 0) return(list())
  ordy <- order(sapply(sig_pairs, function(p) p[2] - p[1]), sapply(sig_pairs, function(p) p[1]))
  out <- list()
  for (idx in ordy) {
    p <- sig_pairs[[idx]]; i <- p[1]; j <- p[2]
    y <- max(occ[i:j]) + step
    out[[length(out) + 1]] <- c(i, j, y)
    occ[i:j] <- y + reserve
  }
  out
}

# pre-compute brackets per outcome to size the y-axis
layouts <- lapply(ord, function(o) bracket_layout(sig_pairs_for(o), as.numeric(mat[, o])))
maxy    <- max(100, unlist(lapply(layouts, function(L) sapply(L, function(b) b[3]))), na.rm = TRUE)
ylim_top <- maxy + 6

## ---- 5. figure (base R clustered bars) ---------------------------
cols <- c("Overall" = "#BDBDBD", "Pre-2016" = "#0072B2", "2016-2019" = "#E69F00",
          "2020-2023" = "#009E73", "2024-present" = "#F0E442")

legend_row <- function(items, y, cex) {
  sw <- 0.026; gap <- 0.010; itemgap <- 0.020
  wds <- sapply(items, function(it) sw + gap + strwidth(it$label, cex = cex))
  total <- sum(wds) + itemgap * (length(items) - 1)
  x <- 0.5 - total / 2
  for (it in items) {
    rect(x, y - 0.06, x + sw, y + 0.06, col = it$col, border = "black")
    text(x + sw + gap, y, it$label, adj = 0, cex = cex)
    x <- x + sw + gap + strwidth(it$label, cex = cex) + itemgap
  }
}

draw_plot <- function() {
  op <- par(no.readonly = TRUE); on.exit({ par(op); layout(1) })
  layout(matrix(c(1, 2), nrow = 2), heights = c(5, 1.4))
  
  ## panel 1: clustered bars
  par(mar = c(9, 5, 4, 2), mgp = c(3, 1.3, 0))
  bp <- barplot(mat, beside = TRUE, col = cols[series], border = "black",
                ylim = c(0, ylim_top), names.arg = rep("", ncol(mat)), las = 1,
                yaxt = "n", cex.axis = txt_cex, cex.lab = txt_cex, cex.main = txt_cex,
                ylab = "Responding (%) selecting outcome",
                main = "What members hope to get out of Chicago DSA, by first-event cohort")
  axis(2, at = seq(0, 100, 25), las = 1, cex.axis = txt_cex)
  axis(1, at = par("usr")[1:2], labels = FALSE, tcl = 0)
  axis(1, at = colMeans(bp), labels = FALSE)
  # nested brackets for significant cohort pairs (grey Overall bar is never tested)
  for (jc in seq_along(ord)) for (b in layouts[[jc]]) {
    xa <- bp[b[1], jc]; xb <- bp[b[2], jc]; y <- b[3]
    segments(xa, y, xb, y, lwd = 0.8)
    segments(xa, y, xa, y - 1.2, lwd = 0.8); segments(xb, y, xb, y - 1.2, lwd = 0.8)
    text((xa + xb) / 2, y + 0.4, "*", adj = c(0.5, 0), cex = txt_cex)
  }
  # angled outcome labels below the axis
  text(colMeans(bp), par("usr")[3] - 0.03 * diff(par("usr")[3:4]), labels = ord,
       srt = 40, adj = 1, xpd = NA, cex = txt_cex)
  
  ## panel 2: legend + key
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  legend_row(list(list(col = cols["Overall"],      label = "Overall (all members)"),
                  list(col = cols["Pre-2016"],      label = "Pre-2016"),
                  list(col = cols["2016-2019"],     label = "2016-2019")), 0.74, txt_cex)
  legend_row(list(list(col = cols["2020-2023"],    label = "2020-2023"),
                  list(col = cols["2024-present"], label = "2024-present")), 0.44, txt_cex)
  key <- if (correction == "holm")
    "*  cohorts differ in selection rate (Fisher's exact, Holm-corrected within outcome, p < 0.10)" else
      "*  cohorts differ in selection rate (Fisher's exact, uncorrected, p < 0.10)"
  text(0.5, 0.10, key, adj = c(0.5, 0), cex = txt_cex * 0.95)
}

draw_plot()                                   # -> RStudio Plots pane
if (isTRUE(save_png)) {
  png("fig_members_get_by_cohort.png", width = 1600, height = 860, res = 130)
  draw_plot(); dev.off()
  cat("Saved:", normalizePath("fig_members_get_by_cohort.png"), "\n")
}
cat("Cohort sizes (answered get-out & have a first-event year):\n")
print(sapply(groups, function(g) sum(has_go & !is.na(cohort) & cohort == g)))
print(round(mat, 1))

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))