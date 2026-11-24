## diaspora_effect.R
## Does diaspora explain why the public favours some countries more than the
## jury does?
##
## Idea: since 2016 every country gives two sets of points, one from a
## professional jury and one from a public televote. Diaspora communities can
## only vote in the televote. For the same voter, recipient and show (same
## song!) we compare public and jury excess points:
##   g[A->B,t] = e_public[A->B,t] - e_jury[A->B,t]
## where e = points A gave B minus the mean B got from all other voters of the
## same type in that show. We average g per pair and relate it to the
## diaspora share
##   d[A->B] = people born in B living in A / population of A   (UN, 2020)
## with a Spearman correlation, tested by a QAP permutation test (country
## labels of the diaspora matrix are shuffled, which keeps the dependence
## between pairs that share a country).
##
## Data: data/votes.csv (build_data.R), shows with both jury and public points
## (finals 2016-2026, semi-finals 2016-2022), "Rest of the World" dropped.
## UN DESA International Migrant Stock 2024, saved in data/un_migrant_stock/:
##   undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx
##   undesa_pd_2024_ims_stock_by_sex_and_destination.xlsx
## from https://www.un.org/development/desa/pd/content/international-migrant-stock
##
## Run from the project folder:  Rscript diaspora_effect.R
## Output: results/diaspora_pairs.csv, results/diaspora_tests.csv,
##         figures/diaspora_scatter.pdf (+ .png)

library(readxl)

## ---- settings ----------------------------------------------------------

B            <- 10000   # QAP permutations
seed         <- 2026
min_meetings <- 3       # pairs need at least this many shows together
un_year      <- "2020"  # UN snapshot closest to the middle of 2016-2026

un_dir   <- file.path("data", "un_migrant_stock")
un_od    <- file.path(un_dir,
              "undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx")
un_dest  <- file.path(un_dir,
              "undesa_pd_2024_ims_stock_by_sex_and_destination.xlsx")

## Eurovision two-letter codes -> UN numeric location codes (ISO 3166)
iso_num <- c(AD = 20, AL = 8, AM = 51, AT = 40, AU = 36, AZ = 31, BA = 70,
             BE = 56, BG = 100, BY = 112, CH = 756, CY = 196, CZ = 203,
             DE = 276, DK = 208, EE = 233, ES = 724, FI = 246, FR = 250,
             GB = 826, GE = 268, GR = 300, HR = 191, HU = 348, IE = 372,
             IL = 376, IS = 352, IT = 380, LT = 440, LU = 442, LV = 428,
             MA = 504, MC = 492, MD = 498, ME = 499, MK = 807, MT = 470,
             NL = 528, NO = 578, PL = 616, PT = 620, RO = 642, RS = 688,
             RU = 643, SE = 752, SI = 705, SK = 703, SM = 674, TR = 792,
             UA = 804)

## ---- votes: jury and public excess points -------------------------------

v <- read.csv(file.path("data", "votes.csv"))
v <- v[v$year >= 2016 & v$vote_type %in% c("jury", "public") &
       v$from_code != "WLD", ]

## excess points within each show and vote type
key <- paste(v$edition, v$vote_type, v$to_code)
s   <- ave(v$points, key, FUN = sum)
n   <- ave(v$points, key, FUN = length)
v$excess <- v$points - (s - v$points) / (n - 1)

## one row per vote with both components (drops shows with only one type)
j <- v[v$vote_type == "jury",   c("edition", "year", "from_code", "to_code",
                                  "from_country", "to_country", "excess")]
p <- v[v$vote_type == "public", c("edition", "from_code", "to_code", "excess")]
names(j)[names(j) == "excess"] <- "e_jury"
names(p)[names(p) == "excess"] <- "e_public"
w <- merge(j, p, by = c("edition", "from_code", "to_code"))
w$gap <- w$e_public - w$e_jury

cat(sprintf("Votes with jury and public points: %d (%d-%d, %d shows)\n",
            nrow(w), min(w$year), max(w$year), length(unique(w$edition))))

## ---- UN migrant stock: diaspora share -----------------------------------

read_un <- function(file, sheet) {
  x <- suppressWarnings(suppressMessages(
         read_excel(file, sheet = sheet, skip = 10)))
  as.data.frame(x)
}

## migrants by destination and origin (both sexes = first block of years)
od <- read_un(un_od, "Table 1")
od <- data.frame(dest   = as.numeric(od[["Location code of destination"]]),
                 origin = as.numeric(od[["Location code of origin"]]),
                 stock  = as.numeric(od[[grep(paste0("^", un_year),
                                              names(od))[1]]]))

## population of each destination = migrants / (migrants as % of population)
## (Table 2 has population directly, but its code column is unreadable)
t1 <- read_un(un_dest, "Table 1")
t3 <- read_un(un_dest, "Table 3")
col <- function(x) as.numeric(x[[grep(paste0("^", un_year), names(x))[1]]])
pop <- data.frame(code = as.numeric(t1[["Location code"]]),
                  pop  = col(t1) / (col(t3) / 100))
pop <- pop[!is.na(pop$code) & is.finite(pop$pop), ]

## diaspora of B living in A, as a share of A's population
countries <- sort(unique(c(w$from_code, w$to_code)))
stopifnot(all(countries %in% names(iso_num)))
D <- matrix(0, length(countries), length(countries),
            dimnames = list(countries, countries))   # rows = A, cols = B
for (a in countries) for (b in countries) if (a != b) {
  st <- od$stock[od$dest == iso_num[[a]] & od$origin == iso_num[[b]]]
  ## pairs the UN does not list are treated as 0 migrants
  D[a, b] <- if (length(st) && !is.na(st[1])) st[1] else 0
}
pop_a <- pop$pop[match(iso_num[countries], pop$code)]
stopifnot(!anyNA(pop_a))
D <- D / pop_a                                  # divide each row by pop of A

## ---- pair averages -----------------------------------------------------

pair_table <- function(w) {
  k <- paste(w$from_code, w$to_code)
  pr <- data.frame(
    from_code = tapply(w$from_code, k, `[`, 1),
    to_code   = tapply(w$to_code, k, `[`, 1),
    pair      = tapply(paste(w$from_country, "->", w$to_country), k, `[`, 1),
    meetings  = as.vector(table(k)[sort(unique(k))]),
    e_public  = tapply(w$e_public, k, mean),
    e_jury    = tapply(w$e_jury, k, mean),
    gap       = tapply(w$gap, k, mean),
    row.names = NULL
  )
  pr$diaspora_pct <- 100 * D[cbind(pr$from_code, pr$to_code)]
  pr[pr$meetings >= min_meetings, ]
}

## ---- QAP permutation test ----------------------------------------------

## Spearman correlation of y with the diaspora share; the null distribution
## comes from relabelling the countries of the diaspora matrix at random
qap_test <- function(y, from, to, B, seed) {
  set.seed(seed)
  ia <- match(from, countries); ib <- match(to, countries)
  rho_obs <- cor(y, D[cbind(ia, ib)], method = "spearman")
  rho_null <- replicate(B, {
    pi <- sample(length(countries))
    cor(y, D[cbind(pi[ia], pi[ib])], method = "spearman")
  })
  c(rho = rho_obs,
    p_one_sided = (1 + sum(rho_null >= rho_obs)) / (B + 1),
    null_q95 = unname(quantile(rho_null, 0.95)))
}

run_all <- function(w, label) {
  pr <- pair_table(w)
  res <- rbind(
    gap      = qap_test(pr$gap,      pr$from_code, pr$to_code, B, seed),
    e_public = qap_test(pr$e_public, pr$from_code, pr$to_code, B, seed),
    e_jury   = qap_test(pr$e_jury,   pr$from_code, pr$to_code, B, seed)
  )
  data.frame(sample = label, outcome = rownames(res), pairs = nrow(pr),
             rho = round(res[, "rho"], 3),
             null_q95 = round(res[, "null_q95"], 3),
             p_value = signif(res[, "p_one_sided"], 3), row.names = NULL)
}

time <- system.time({
  tests <- rbind(run_all(w, "2016-2026"),
                 run_all(w[w$year != 2022, ], "2016-2026 without 2022"))
})
cat(sprintf("QAP tests (%d permutations each) in %.1f s\n\n", B,
            time[["elapsed"]]))
print(tests, row.names = FALSE)

pairs <- pair_table(w)
pairs <- pairs[order(-pairs$gap), ]
cat("\nPairs where the public favours most compared with the jury:\n")
show <- head(pairs, 15)
show[, 5:8] <- round(show[, 5:8], 2)
print(show[, c("pair", "meetings", "e_public", "e_jury", "gap",
               "diaspora_pct")], row.names = FALSE)

## ---- output ------------------------------------------------------------

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
write.csv(pairs, file.path("results", "diaspora_pairs.csv"), row.names = FALSE)
write.csv(tests, file.path("results", "diaspora_tests.csv"), row.names = FALSE)

## ---- plot: public-minus-jury gap vs diaspora share ---------------------

plot_diaspora <- function(pairs, rho,
                          label = c("IE LT", "IE PL", "IT RO", "CH AL",
                                    "IT AL")) {
  blue <- "#2a78d6"; ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"
  floor_pct <- 1e-3                               # show 0 migrants at 0.001%
  x <- pmax(pairs$diaspora_pct, floor_pct)
  y <- pairs$gap

  op <- par(mar = c(3.2, 3.6, 0.6, 0.8), mgp = c(2.1, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))
  plot(x, y, log = "x", type = "n", bty = "n", xaxt = "n", yaxt = "n",
       xlab = "", ylab = "")
  xt <- 10^(-3:1)
  abline(v = xt, h = pretty(y), col = "#ecebe6", lwd = 0.8)
  abline(h = 0, col = muted, lwd = 1)
  axis(1, at = xt, labels = c("<0.001%", "0.01%", "0.1%", "1%", "10%"),
       lwd = 0, line = -0.6)
  axis(2, at = pretty(y), lwd = 0)
  title(xlab = "People born in B living in A (share of A's population)",
        ylab = "Public minus jury excess points", col.lab = ink2,
        cex.lab = 0.85)
  points(x, y, pch = 19, cex = 0.55, col = adjustcolor(blue, 0.45))
  sm <- lowess(log10(x), y, f = 0.5)
  lines(10^sm$x, sm$y, col = ink, lwd = 2)

  ## label a few well-known pairs (voter > recipient, country codes)
  lab <- match(label, paste(pairs$from_code, pairs$to_code))
  lab <- lab[!is.na(lab)]
  points(x[lab], y[lab], pch = 21, cex = 0.8, bg = blue, col = "white")
  text(x[lab], y[lab], paste(pairs$from_code[lab], ">", pairs$to_code[lab]),
       pos = ifelse(x[lab] > 0.5, 2, 4), cex = 0.6, col = ink2, offset = 0.4)
  legend("bottomright", bty = "n", cex = 0.75, text.col = ink2,
         legend = sprintf("Spearman rho = %.2f", rho))
}

rho_main <- tests$rho[tests$sample == "2016-2026" & tests$outcome == "gap"]
pdf(file.path("figures", "diaspora_scatter.pdf"), width = 6, height = 3.6)
plot_diaspora(pairs, rho_main); dev.off()
png(file.path("figures", "diaspora_scatter.png"), width = 1800, height = 1080,
    res = 300)
plot_diaspora(pairs, rho_main); dev.off()
