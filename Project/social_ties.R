## social_ties.R
## Do social ties explain Eurovision favouritism?
##
## Social ties: Social Connectedness Index (SCI) from Facebook friendships,
## January 2026 (Johnston, Kuchler, Kulkarni & Stroebel 2026). For countries
## A and B it measures how likely a user in A is to be friends with a user in
## B (relative scale, symmetric: SCI[A,B] = SCI[B,A]).
## Favouritism: excess points as in Act 1,
##   e[A->B,t] = p[A->B,t] - mean points B got from all other voters in show t
## averaged per directed pair (A -> B).
## Test: Spearman correlation between pair excess and SCI, with a QAP
## permutation test (the country labels of the SCI matrix are shuffled, which
## keeps the dependence between pairs that share a country).
##
## Samples
##   main:        1975-2026, points that counted ("total"; 2016+ halved to the
##                12-point scale), pairs with >= 5 meetings
##   recent:      2016-2026 only (closer to the 2026 SCI snapshot), >= 3
## Russia and Belarus are excluded in both: Facebook is little used there and
## is blocked in Russia since 2022, so their SCI is unreliable. A version
## that keeps them is reported as a check. Monaco and San Marino have no SCI.
##
## Data: data/votes.csv (build_data.R) and data/sci/country.csv from
## https://data.humdata.org/dataset/social-connectedness-index
##
## Run from the project folder:  Rscript social_ties.R
## Output: results/social_ties_tests.csv, results/social_ties_pairs.csv,
##         figures/social_ties.pdf (+ .png)

## ---- settings ----------------------------------------------------------

B    <- 10000
seed <- 2026
unreliable_sci <- c("RU", "BY")

## ---- data --------------------------------------------------------------

sci <- read.csv(file.path("data", "sci", "country.csv"))
sci <- sci[sci$user_country != sci$friend_country, ]

v <- read.csv(file.path("data", "votes.csv"))
v <- v[v$vote_type == "total" & v$year >= 1975 & v$from_code != "WLD", ]
v$points <- ifelse(v$year >= 2016, v$points / 2, v$points)  # 12-point scale

## excess points within each show (all voters count for the averages)
key <- paste(v$edition, v$to_code)
s   <- ave(v$points, key, FUN = sum)
n   <- ave(v$points, key, FUN = length)
v$excess <- v$points - (s - v$points) / (n - 1)

## ---- SCI matrix --------------------------------------------------------

countries_all <- sort(intersect(unique(c(v$from_code, v$to_code)),
                                unique(sci$user_country)))
S <- matrix(NA_real_, length(countries_all), length(countries_all),
            dimnames = list(countries_all, countries_all))
k <- sci$user_country %in% countries_all & sci$friend_country %in% countries_all
S[cbind(sci$user_country[k], sci$friend_country[k])] <- sci$scaled_sci[k]

## ---- pair table --------------------------------------------------------

pair_table <- function(v, min_meetings, drop) {
  v <- v[v$from_code %in% countries_all & v$to_code %in% countries_all &
         !(v$from_code %in% drop) & !(v$to_code %in% drop), ]
  k <- paste(v$from_code, v$to_code)
  pr <- data.frame(
    from_code = tapply(v$from_code, k, `[`, 1),
    to_code   = tapply(v$to_code, k, `[`, 1),
    pair      = tapply(paste(v$from_country, "->", v$to_country), k, `[`, 1),
    meetings  = as.vector(table(k)[sort(unique(k))]),
    excess    = tapply(v$excess, k, mean),
    row.names = NULL
  )
  pr$sci <- S[cbind(pr$from_code, pr$to_code)]
  pr[pr$meetings >= min_meetings & !is.na(pr$sci), ]
}

## ---- QAP permutation test ----------------------------------------------

qap_test <- function(pr, B, seed) {
  set.seed(seed)
  cn <- sort(unique(c(pr$from_code, pr$to_code)))
  M  <- S[cn, cn]
  ia <- match(pr$from_code, cn); ib <- match(pr$to_code, cn)
  rho_obs  <- cor(pr$excess, M[cbind(ia, ib)], method = "spearman")
  rho_null <- replicate(B, {
    pi <- sample(length(cn))
    cor(pr$excess, M[cbind(pi[ia], pi[ib])], method = "spearman",
        use = "complete.obs")
  })
  data.frame(countries = length(cn), pairs = nrow(pr),
             rho = round(rho_obs, 3),
             null_q95 = round(unname(quantile(rho_null, 0.95)), 3),
             p_value = signif((1 + sum(rho_null >= rho_obs)) / (B + 1), 3))
}

samples <- list(
  "1975-2026"             = list(v = v, min = 5, drop = unreliable_sci),
  "2016-2026"             = list(v = v[v$year >= 2016, ], min = 3,
                                 drop = unreliable_sci),
  "1975-2026 incl. RU/BY" = list(v = v, min = 5, drop = character(0))
)

time <- system.time(
  tests <- do.call(rbind, lapply(names(samples), function(nm) {
    x <- samples[[nm]]
    cbind(sample = nm, qap_test(pair_table(x$v, x$min, x$drop), B, seed))
  }))
)
cat(sprintf("QAP tests (%d permutations each) in %.1f s\n\n", B,
            time[["elapsed"]]))
print(tests, row.names = FALSE)

## ---- effect size: excess by SCI quintile (main sample) -----------------

pairs <- pair_table(v, 5, unreliable_sci)
pairs$sci_quintile <- cut(rank(pairs$sci), 5, labels = paste0("Q", 1:5))
quint <- aggregate(excess ~ sci_quintile, pairs, mean)
quint$excess <- round(quint$excess, 2)
quint$median_sci <- tapply(pairs$sci, pairs$sci_quintile, median)
cat("\nMean excess points by SCI quintile (Q5 = most connected):\n")
print(quint, row.names = FALSE)

pairs <- pairs[order(-pairs$sci), ]
cat("\nMost connected pairs:\n")
top <- head(pairs, 12)
top$excess <- round(top$excess, 2)
print(top[, c("pair", "meetings", "excess", "sci")], row.names = FALSE)

## ---- output ------------------------------------------------------------

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
write.csv(tests, file.path("results", "social_ties_tests.csv"),
          row.names = FALSE)
write.csv(pairs, file.path("results", "social_ties_pairs.csv"),
          row.names = FALSE)

## ---- plot: pair excess vs SCI ------------------------------------------

plot_ties <- function(pairs, rho,
                      label = c("GR CY", "RO MD", "RS ME", "SI BA",
                                "PT LU", "AM GE")) {
  blue <- "#2a78d6"; ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"
  x <- pairs$sci; y <- pairs$excess

  op <- par(mar = c(3.2, 3.6, 0.6, 0.8), mgp = c(2.1, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))
  plot(x, y, log = "x", type = "n", bty = "n", xaxt = "n", yaxt = "n",
       xlab = "", ylab = "")
  xt <- 10^(2:6)
  abline(v = xt, h = pretty(y), col = "#ecebe6", lwd = 0.8)
  abline(h = 0, col = muted, lwd = 1)
  axis(1, at = xt, labels = c("100", "1k", "10k", "100k", "1M"), lwd = 0,
       line = -0.6)
  axis(2, at = pretty(y), lwd = 0)
  title(xlab = "Facebook social connectedness between A and B (SCI)",
        ylab = "Mean excess points A gives B", col.lab = ink2, cex.lab = 0.85)
  points(x, y, pch = 19, cex = 0.55, col = adjustcolor(blue, 0.4))
  sm <- lowess(log10(x), y, f = 0.5)
  lines(10^sm$x, sm$y, col = ink, lwd = 2)

  lab <- match(label, paste(pairs$from_code, pairs$to_code))
  lab <- lab[!is.na(lab)]
  points(x[lab], y[lab], pch = 21, cex = 0.8, bg = blue, col = "white")
  text(x[lab], y[lab], paste(pairs$from_code[lab], ">", pairs$to_code[lab]),
       pos = 2, cex = 0.6, col = ink2, offset = 0.4)
  legend("topleft", bty = "n", cex = 0.75, text.col = ink2,
         legend = sprintf("Spearman rho = %.2f", rho))
}

rho_main <- tests$rho[tests$sample == "1975-2026"]
pdf(file.path("figures", "social_ties.pdf"), width = 6, height = 3.6)
plot_ties(pairs, rho_main); dev.off()
png(file.path("figures", "social_ties.png"), width = 1800, height = 1080,
    res = 300)
plot_ties(pairs, rho_main); dev.off()

## ---- plot: boxplot of pair excess by SCI quintile ----------------------
## Same pairs as above (1975-2026, >= 5 meetings, without RU/BY), split into
## five equal groups by Facebook connectedness. Boxes show the spread of the
## pairs' mean excess points; pink dots and labels are the quintile means.

plot_quintiles <- function(pairs) {
  blue <- "#2a78d6"; pink <- "#FF3FC0"; ink2 <- "#52514e"; muted <- "#898781"
  y  <- pairs$excess
  yl <- range(y) + c(-0.5, 1.8)          # room for the mean labels on top

  op <- par(mar = c(3.6, 3.6, 0.6, 0.6), mgp = c(2.1, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))
  plot(NA, xlim = c(0.5, 5.5), ylim = yl, xaxt = "n", yaxt = "n", bty = "n",
       xlab = "", ylab = "")
  abline(h = pretty(range(y)), col = "#ecebe6", lwd = 0.8)
  abline(h = 0, col = muted, lwd = 1, lty = 2)
  boxplot(excess ~ sci_quintile, data = pairs, add = TRUE, axes = FALSE,
          col = adjustcolor(blue, 0.25), border = blue, boxwex = 0.55,
          lwd = 1, pch = 16, cex = 0.45, outcol = adjustcolor(blue, 0.45),
          medcol = blue, medlwd = 2, whisklty = 1, staplelwd = 1)
  axis(2, at = pretty(range(y)), lwd = 0)
  axis(1, at = 1:5, lwd = 0, line = -0.4, col.axis = ink2,
       labels = c("Q1\nleast", "Q2", "Q3", "Q4", "Q5\nmost"), padj = 0.5)
  title(xlab = "Facebook connectedness (SCI quintile)", line = 2.5,
        col.lab = ink2, cex.lab = 0.85)
  title(ylab = "Mean excess points A gives B", col.lab = ink2, cex.lab = 0.85)

  m <- tapply(pairs$excess, pairs$sci_quintile, mean)
  points(1:5, m, pch = 21, bg = pink, col = "white", cex = 1.3, lwd = 1.5)
  lab <- ifelse(abs(m) < 0.05, "0.0", sprintf("%+.1f", m))
  ## row of mean labels along the top of the plot
  text(1:5, yl[2] - 0.3, paste("mean", lab), cex = 0.72, col = pink, font = 2)
}

pdf(file.path("figures", "sci_quintiles_boxplot.pdf"), width = 6, height = 3.6)
plot_quintiles(pairs); dev.off()
png(file.path("figures", "sci_quintiles_boxplot.png"), width = 1800,
    height = 1080, res = 300)
plot_quintiles(pairs); dev.off()
