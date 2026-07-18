# =====================================================================
# Sense of belonging by identity (collapsed groups) — two figures
# Built directly from the raw survey response file.
#
# Groups per question (mutually exclusive):
#   Race        : White (only) / any non-White identity / prefer not to answer
#   Gender      : cisgender (man or woman only) / any non-cis identity / prefer not to answer
#   Orientation : heterosexual / any other orientation / prefer not to answer
#
# ALL pairs of groups are compared (independent Mann-Whitney U), Holm-
# corrected within each question. Only SIGNIFICANT differences are drawn,
# as nested brackets (narrow spans low, wider spans stacked above).
# =====================================================================

save_png      <- TRUE
txt_cex       <- 0.9      # single text size used for all text on the plot
sig_threshold <- 0.10     # a pair gets a bracket when its Holm p < this

## ---- read --------------------------------------------------------
input_file <- "CDSA_MEC_SurveyResponses_July2026.csv"   # adjust name/extension if needed
stopifnot(file.exists(input_file))
resp <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

g1 <- function(k) grep(k, names(resp), ignore.case = TRUE, value = TRUE)[1]
race_raw   <- resp[[g1("racial identity")]]
gender_raw <- resp[[g1("gender identity")]]
orient_raw <- resp[[g1("sexual orientation")]]
similar <- suppressWarnings(as.numeric(resp[[g1("how similar do you feel")]]))
welcome <- suppressWarnings(as.numeric(resp[[g1("how welcomed do you feel")]]))

## ---- collapse each question into three groups --------------------
Rl <- tolower(ifelse(is.na(race_raw), "", race_raw))
nonwhite <- grepl("black or african american|asian or asian american|latino, latina, or latine|hispanic \\(often defined|native american or alaska native|southwest asian and north african", Rl)
white    <- grepl("white \\(e\\.g\\.", Rl)
rpnts    <- grepl("prefer not to say", Rl)
race_group <- ifelse(nonwhite, "BIPOC",
                     ifelse(white, "White",
                            ifelse(rpnts, "Prefer not to answer", NA)))

gtok <- function(t) {
  tl <- tolower(t)
  if (grepl("transgender", tl)) return("nc")
  if (t == "Man" || t == "Woman") return("cis")
  if (grepl("non-binary|genderqueer|genderfluid|agender|demiboy|gnc|gender non|two-spirit|bigender|pangender|intersex", tl)) return("nc")
  if (grepl("prefer not|questioning|don't know", tl)) return("pnts")
  "nc"
}
gender_group <- unname(sapply(gender_raw, function(v) {
  if (is.na(v) || v == "") return(NA_character_)
  cls <- vapply(trimws(strsplit(v, ",")[[1]]), gtok, character(1))
  if ("nc"   %in% cls) return("Non-cisgender")
  if ("cis"  %in% cls) return("Cisgender")
  if ("pnts" %in% cls) return("Prefer not to answer")
  NA_character_
}))

Ol <- tolower(ifelse(is.na(orient_raw), "", orient_raw))
orient_group <- ifelse(grepl("hetero|straight", Ol), "Heterosexual",
                       ifelse(grepl("bisexual|queer|\\bgay\\b|lesbian|pansexual|asexual|polysexual|\\bace\\b|\\bbi\\b", Ol),
                              "Not heterosexual", "Prefer not to answer"))

Q <- list(
  list(name = "Race/ Ethnicity",  grp = race_group,   col = "#E69F00",
       cats = c("White", "BIPOC", "Prefer not to answer"),
       short = c("White" = "White", "BIPOC" = "BIPOC", "Prefer not to answer" = "Prefer not to answer")),
  list(name = "Gender", grp = gender_group, col = "#56B4E9",
       cats = c("Cisgender", "Non-cisgender", "Prefer not to answer"),
       short = c("Cisgender" = "Cisgender", "Non-cisgender" = "Non-cisgender", "Prefer not to answer" = "Prefer not to answer")),
  list(name = "Sexual Orientation", grp = orient_group, col = "#009E73",
       cats = c("Heterosexual", "Not heterosexual", "Prefer not to answer"),
       short = c("Heterosexual" = "Heterosexual", "Not heterosexual" = "Non-heterosexual", "Prefer not to answer" = "Prefer not to answer"))
)

## ---- analysis: ALL pairwise, Holm within question ----------------
analyze <- function(grp, cats, y) {
  ok <- !is.na(y) & !is.na(grp) & grp %in% cats
  g <- grp[ok]; yy <- y[ok]
  present <- cats[cats %in% unique(g)]
  means <- sapply(present, function(c) mean(yy[g == c]))
  ns    <- sapply(present, function(c) sum(g == c))
  ord   <- names(sort(means, decreasing = TRUE))
  pairs <- combn(length(ord), 2)                       # 2 x npairs, positions in ord
  rawp  <- apply(pairs, 2, function(pr)
    wilcox.test(yy[g == ord[pr[1]]], yy[g == ord[pr[2]]], exact = FALSE)$p.value)
  list(order = ord, means = means, ns = ns, pairs = pairs, holm = p.adjust(rawp, method = "holm"))
}

## ---- stack significant brackets (narrow spans low, wide ones up) --
bracket_layout <- function(sig_pairs, heights, step = 0.42, reserve = 0.14) {
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

## ---- figure builder (one outcome = one figure) -------------------
draw_fig <- function(y, main, fname) {
  A <- lapply(Q, function(q) analyze(q$grp, q$cats, y))
  barw <- 0.8; gap <- 1.2
  barx <- vector("list", length(Q)); centers <- numeric(length(Q)); lr <- matrix(NA, length(Q), 2)
  pos <- 0
  for (qi in seq_along(Q)) {
    k <- length(A[[qi]]$order); xs <- pos + seq_len(k) - 1
    barx[[qi]] <- xs; lr[qi, ] <- range(xs); centers[qi] <- mean(xs)
    pos <- max(xs) + 1 + gap
  }
  totalR <- pos - gap - 0.3
  
  # pre-compute bracket layouts to size the plot
  layouts <- vector("list", length(Q)); maxy <- 5.0
  for (qi in seq_along(Q)) {
    ord <- A[[qi]]$order; holm <- A[[qi]]$holm; pr <- A[[qi]]$pairs
    hvec <- as.numeric(A[[qi]]$means[ord])
    sig <- which(holm < sig_threshold)
    sig_pairs <- lapply(sig, function(k) pr[, k])
    layouts[[qi]] <- bracket_layout(sig_pairs, hvec)
    if (length(layouts[[qi]])) maxy <- max(maxy, max(sapply(layouts[[qi]], function(b) b[3])))
  }
  name_y   <- maxy + 0.40
  under_y  <- maxy + 0.28
  ylim_top <- name_y + 0.20     # <- less empty headroom above the question labels
  
  render <- function() {
    op <- par(no.readonly = TRUE); on.exit(par(op))
    par(mar = c(9, 5, 2.5, 2), mgp = c(3, 1.3, 0))   # <- smaller top margin pulls the title in
    plot(NA, xlim = c(-0.7, totalR), ylim = c(0, ylim_top), xaxs = "i", axes = FALSE,
         xlab = "", ylab = "Average score",
         main = main, cex.lab = txt_cex, cex.main = txt_cex)
    axis(2, at = 0:5, las = 1, cex.axis = txt_cex)
    axis(1, at = c(par("usr")[1], par("usr")[2]), labels = FALSE, tcl = 0)   # full-width baseline
    axis(1, at = unlist(barx), labels = FALSE, tcl = -0.35)                   # short ticks under bars
    lab_y <- -0.1 * ylim_top                                                 # buffer below the ticks
    for (qi in seq_along(Q)) {
      q <- Q[[qi]]; ord <- A[[qi]]$order; xs <- barx[[qi]]
      for (p in seq_along(ord)) {
        m <- as.numeric(A[[qi]]$means[ord[p]])
        rect(xs[p] - barw/2, 0, xs[p] + barw/2, m, col = q$col, border = "black")
        text(xs[p], lab_y, sprintf("%s (n=%d)", q$short[[ord[p]]], A[[qi]]$ns[ord[p]]),
             srt = 40, adj = 1, xpd = NA, cex = txt_cex)
      }
      text(centers[qi], name_y, q$name, font = 2, cex = txt_cex, xpd = NA)
      segments(lr[qi, 1] - 0.4, under_y, lr[qi, 2] + 0.4, under_y, col = q$col, lwd = 2.6, xpd = NA)
      for (b in layouts[[qi]]) {                       # significant brackets only
        xa <- xs[b[1]]; xb <- xs[b[2]]; yb <- b[3]
        segments(xa, yb, xb, yb, lwd = 0.8)
        segments(xa, yb, xa, yb - 0.07, lwd = 0.8); segments(xb, yb, xb, yb - 0.07, lwd = 0.8)
        text((xa + xb) / 2, yb + 0.02, "*", adj = c(0.5, 0), cex = txt_cex)
      }
    }
  }
  
  render()                                   # -> RStudio Plots pane
  if (isTRUE(save_png)) {
    png(fname, width = 1200, height = 700, res = 130); render(); dev.off()
    cat("Saved:", normalizePath(fname), "\n")
  }
  invisible(A)
}

## ---- build the two figures separately ----------------------------
draw_fig(similar, "How similar do you feel to the average member?  (1 = not similar, 5 = extremely similar)",
         "fig_belonging_similar.png")
draw_fig(welcome, "How welcomed do you feel at events?  (1 = not welcome, 5 = extremely welcome)",
         "fig_belonging_welcome.png")

# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))