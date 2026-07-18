# =====================================================================
# Sense of belonging by identity (FULL categories) — two figures
# Built directly from the raw survey response file.
#
# Detailed groups per question:
#   Race        : White / Black / Asian / Latine-Hisp. / Native Am. / SWANA / PNTS  (select-all: overlapping)
#   Gender      : Man / Woman / Transgender / Non-binary-GD / PNTS-Quest.           (select-all: overlapping)
#   Orientation : Heterosexual / Bisexual / Queer / Gay-Lesbian / Pansexual / Asexual / PNTS  (single-select)
#
# ALL pairs of groups are compared (independent Mann-Whitney U), Holm-
# corrected within each question. Every present group is tested, including
# small ones. Only SIGNIFICANT differences are drawn, as nested brackets.
# Significance threshold p < 0.10.
# =====================================================================

save_png      <- TRUE
txt_cex       <- 0.9
sig_threshold <- 0.10

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

## ---- membership indicators --------------------------------------
# Race: select-all, a person is counted in EACH identity they chose (overlapping)
Rl <- tolower(ifelse(is.na(race_raw), "", race_raw))
race_ind <- list(
  "White"        = grepl("white \\(e\\.g\\.", Rl),
  "Black"        = grepl("black or african american", Rl),
  "Asian"        = grepl("asian or asian american", Rl),
  "Latine/Hisp." = grepl("latino, latina, or latine|hispanic \\(often defined", Rl),
  "Native Am."   = grepl("native american or alaska native", Rl),
  "SWANA"        = grepl("southwest asian and north african", Rl),
  "Prefer not to answer"         = grepl("prefer not to say", Rl))

# Gender: select-all, overlapping
gtokcat <- function(t) {
  tl <- tolower(t)
  if (grepl("transgender", tl)) return("Transgender")
  if (t == "Man") return("Man")
  if (t == "Woman") return("Woman")
  if (grepl("non-binary|genderqueer|genderfluid|agender|demiboy|gnc|gender non|two-spirit|bigender|pangender|intersex", tl)) return("Non-binary/GD")
  if (grepl("prefer not|questioning|don't know", tl)) return("Prefer not to answer")
  "Non-binary/GD"
}
gender_sets <- lapply(gender_raw, function(v) {
  if (is.na(v) || v == "") return(character(0))
  unique(vapply(trimws(strsplit(v, ",")[[1]]), gtokcat, character(1)))
})
gcats <- c("Man", "Woman", "Transgender", "Non-binary/GD", "Prefer not to answer")
gender_ind <- setNames(lapply(gcats, function(c) vapply(gender_sets, function(s) c %in% s, logical(1))), gcats)

# Orientation: single-select (mutually exclusive)
Ol <- tolower(ifelse(is.na(orient_raw), "", orient_raw))
ocat <- rep("Prefer not to answer", length(Ol))
ocat[grepl("hetero|straight", Ol)]      <- "Heterosexual"
ocat[grepl("bisexual|ace/bi", Ol)]      <- "Bisexual"
ocat[grepl("queer", Ol)]                <- "Queer"
ocat[grepl("\\bgay\\b|lesbian", Ol)]    <- "Gay/Lesbian"
ocat[grepl("pansexual|polysexual", Ol)] <- "Pansexual"
ocat[grepl("asexual", Ol)]              <- "Asexual"
ocats <- c("Heterosexual", "Bisexual", "Queer", "Gay/Lesbian", "Pansexual", "Asexual", "Prefer not to answer")
orient_ind <- setNames(lapply(ocats, function(c) ocat == c), ocats)

Q <- list(
  list(name = "Race/ Ethnicity",               col = "#E69F00", ind = race_ind),
  list(name = "Gender Identity",             col = "#56B4E9", ind = gender_ind),
  list(name = "Sexual Orientation", col = "#009E73", ind = orient_ind))

## ---- analysis: all pairwise among ALL present groups ------------
analyze <- function(ind, y) {
  cats <- names(ind)
  nn <- sapply(cats, function(c) sum(ind[[c]] & !is.na(y)))
  present <- cats[nn > 0]
  means <- sapply(present, function(c) mean(y[ind[[c]] & !is.na(y)]))
  ns <- nn[present]
  ord <- names(sort(means, decreasing = TRUE))
  posf <- setNames(seq_along(ord), ord)
  pairs <- list(); rawp <- numeric(0)
  if (length(ord) >= 2) {
    cb <- combn(ord, 2)
    for (k in seq_len(ncol(cb))) {
      a <- cb[1, k]; b <- cb[2, k]
      pairs[[k]] <- sort(c(posf[a], posf[b]))
      rawp[k] <- wilcox.test(y[ind[[a]] & !is.na(y)], y[ind[[b]] & !is.na(y)], exact = FALSE)$p.value
    }
  }
  holm <- if (length(rawp)) p.adjust(rawp, method = "holm") else numeric(0)
  list(order = ord, means = means, ns = ns, pairs = pairs, holm = holm)
}

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

## ---- figure builder ---------------------------------------------
draw_fig <- function(y, main, fname) {
  A <- lapply(Q, function(q) analyze(q$ind, y))
  barw <- 0.8; gap <- 1.2
  barx <- vector("list", length(Q)); centers <- numeric(length(Q)); lr <- matrix(NA, length(Q), 2)
  pos <- 0
  for (qi in seq_along(Q)) {
    k <- length(A[[qi]]$order); xs <- pos + seq_len(k) - 1
    barx[[qi]] <- xs; lr[qi, ] <- range(xs); centers[qi] <- mean(xs)
    pos <- max(xs) + 1 + gap
  }
  totalR <- pos - gap - 0.3
  
  layouts <- vector("list", length(Q)); maxy <- 5.0
  for (qi in seq_along(Q)) {
    ord <- A[[qi]]$order; holm <- A[[qi]]$holm; prs <- A[[qi]]$pairs
    hvec <- as.numeric(A[[qi]]$means[ord])
    sig <- if (length(holm)) which(holm < sig_threshold) else integer(0)
    layouts[[qi]] <- bracket_layout(prs[sig], hvec)
    if (length(layouts[[qi]])) maxy <- max(maxy, max(sapply(layouts[[qi]], function(b) b[3])))
  }
  name_y <- maxy + 0.40; under_y <- maxy + 0.28; ylim_top <- name_y + 0.20   # tighter headroom above labels
  
  render <- function() {
    op <- par(no.readonly = TRUE); on.exit(par(op))
    par(mar = c(9, 5, 2.5, 2), mgp = c(3, 1.3, 0))   # smaller top margin pulls the title in
    plot(NA, xlim = c(-0.7, totalR), ylim = c(0, ylim_top), xaxs = "i", axes = FALSE,
         xlab = "", ylab = "Average score", main = main, cex.lab = txt_cex, cex.main = txt_cex)
    axis(2, at = 0:5, las = 1, cex.axis = txt_cex)
    axis(1, at = c(par("usr")[1], par("usr")[2]), labels = FALSE, tcl = 0)
    axis(1, at = unlist(barx), labels = FALSE, tcl = -0.35)
    lab_y <- -0.1 * ylim_top
    for (qi in seq_along(Q)) {
      q <- Q[[qi]]; ord <- A[[qi]]$order; xs <- barx[[qi]]
      for (p in seq_along(ord)) {
        m <- as.numeric(A[[qi]]$means[ord[p]])
        rect(xs[p] - barw/2, 0, xs[p] + barw/2, m, col = q$col, border = "black")
        text(xs[p], lab_y, sprintf("%s (n=%d)", ord[p], A[[qi]]$ns[ord[p]]),
             srt = 40, adj = 1, xpd = NA, cex = txt_cex)
      }
      text(centers[qi], name_y, q$name, font = 2, cex = txt_cex, xpd = NA)
      segments(lr[qi, 1] - 0.4, under_y, lr[qi, 2] + 0.4, under_y, col = q$col, lwd = 2.6, xpd = NA)
      for (b in layouts[[qi]]) {
        xa <- xs[b[1]]; xb <- xs[b[2]]; yb <- b[3]
        segments(xa, yb, xb, yb, lwd = 0.8)
        segments(xa, yb, xa, yb - 0.07, lwd = 0.8); segments(xb, yb, xb, yb - 0.07, lwd = 0.8)
        text((xa + xb) / 2, yb + 0.02, "*", adj = c(0.5, 0), cex = txt_cex)
      }
    }
  }
  
  render()
  if (isTRUE(save_png)) {
    w <- max(1500, as.integer((totalR + 2) * 82))
    png(fname, width = w, height = 700, res = 130); render(); dev.off()
    cat("Saved:", normalizePath(fname), "\n")
  }
  invisible(A)
}

## ---- build the two figures --------------------------------------
draw_fig(similar, "How similar do you feel to the average member?  (1 = not similar, 5 = extremely similar)",
         "fig_belonging_detailed_similar.png")
draw_fig(welcome, "How welcomed do you feel at events?  (1 = not welcome, 5 = extremely welcome)",
         "fig_belonging_detailed_welcome.png")

# Notes:
#  - Race & gender are select-all, so those groups can overlap (a person is
#    counted in each identity chosen); tests treat samples as independent and
#    may be mildly anticonservative where membership overlaps.
#  - All groups are tested, including small ones (e.g. Native Am. n=3, Asexual
#    n=5); such groups have very low power and rarely reach significance.
#
# If the file is .xlsx not .csv, replace the read.csv line with:
#   library(readxl); resp <- as.data.frame(read_excel("CDSA_MEC_SurveyResponses_July2026.xlsx"))