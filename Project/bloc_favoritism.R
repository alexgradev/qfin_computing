## bloc_favoritism.R
## Act 1, question 1: do countries in the same bloc give each other more
## points than the song's quality explains?
##
## Excess points for voter A, recipient B, show t:
##   e[A->B,t] = p[A->B,t] - mean points B got in show t from all voters except A
## Test statistic per bloc: T = mean of e over all within-bloc votes (A != B).
## Null: within each voter and show, the voter's points are assigned to the
## entries at random (the voter keeps the same set of points).
## p-value = (1 + #{T* >= T_obs}) / (B + 1), then Holm across blocs.
##
## Data: data/votes.csv from build_data.R; vote_type "total" (the points that
## counted), finals and semi-finals 1975-2026, "Rest of the World" dropped.
## Points from 2016 on are halved (jury + public), so all excess points are on
## the 12-point scale.
##
## Output: results/act1_blocs.csv, 
##         results/act1_top_pairs.csv,
##         results/act1_null.rds, 
##         figures/act1_blocs.pdf (+ .png)

## ---- settings ----------------------------------------------------------

B    <- 10000      # number of permutations
seed <- 2026
from_year <- 1975  # 12-point system

blocs <- list(
  "Nordics"           = c("SE", "NO", "DK", "FI", "IS"),
  "ex-Yugoslavia"     = c("RS", "HR", "SI", "BA", "MK", "ME", "YU", "CS"),
  "ex-USSR"           = c("RU", "UA", "BY", "MD", "AM", "AZ", "GE"),
  "Baltics"           = c("EE", "LV", "LT"),
  "Iberia"            = c("ES", "PT", "AD"),
  "Greece & Cyprus"   = c("GR", "CY"),
  "Romania & Moldova" = c("RO", "MD"),
  "Western Balkans"   = c("RS", "HR", "SI", "BA", "MK", "ME", "YU", "CS", "AL"),
  "Central & Eastern Balkans" = c("BG", "HR", "RS", "BA", "MK", "ME", "YU", "CS", "RO", "GR"),
  "Control"           = c("IE", "IL", "MT", "CH")
)

## ---- data --------------------------------------------------------------

v <- read.csv(file.path("data", "votes.csv"))
v <- v[v$vote_type == "total" & v$year >= from_year & v$from_code != "WLD", ]

## since 2016 "total" = jury + public (max 24 per country): halve it so every
## year is on the same 12-point scale and recent years don't count double
v$points <- ifelse(v$year >= 2016, v$points / 2, v$points)

## sort by voter-show so the within-group shuffle below works
v <- v[order(v$edition, v$from_code, v$to_code), ]
rownames(v) <- NULL

voter_grp <- as.integer(factor(paste(v$edition, v$from_code)))   # shuffle unit
recip_grp <- as.integer(factor(paste(v$edition, v$to_code)))     # recipient
n_recip   <- tabulate(recip_grp)          # voters per recipient and show
stopifnot(all(n_recip >= 2))

## within-bloc votes: for each bloc, the rows where voter AND recipient belong
## to it. Blocs may overlap (a country can be in several), so a vote can count
## for more than one bloc. We stack the row indices of all blocs:
##   bloc_rows = row numbers, bloc_idx = which bloc each entry belongs to
rows_per_bloc <- lapply(blocs, function(b)
  which(v$from_code %in% b & v$to_code %in% b))
bloc_rows <- unlist(rows_per_bloc, use.names = FALSE)
bloc_idx  <- rep(seq_along(blocs), lengths(rows_per_bloc))
n_bloc    <- lengths(rows_per_bloc)

if (any(n_bloc == 0))
  stop("No within-bloc votes for: ",
       paste(names(blocs)[n_bloc == 0], collapse = ", "),
       " (check the country codes)")

cat(sprintf("Votes used: %d (%d-%d), within-bloc votes: %d (%d unique)\n",
            nrow(v), min(v$year), max(v$year), length(bloc_rows),
            length(unique(bloc_rows))))

## ---- statistic ---------------------------------------------------------

## excess points for every vote, given a vector of points
excess <- function(p) {
  s <- tabulate_sum(p, recip_grp)[recip_grp]   # total points of recipient
  p - (s - p) / (n_recip[recip_grp] - 1)       # minus mean of other voters
}
tabulate_sum <- function(x, g) as.vector(rowsum(x, g, reorder = TRUE))

## T per bloc: mean excess over within-bloc votes
bloc_stat <- function(p) {
  e <- excess(p)[bloc_rows]
  tabulate_sum(e, bloc_idx) / n_bloc
}

## ---- observed ----------------------------------------------------------

p_obs <- v$points
T_obs <- bloc_stat(p_obs)

## ---- permutation test --------------------------------------------------

## shuffle points within each voter-show: rows are sorted by voter_grp, so
## ordering by (voter_grp, random key) permutes values inside each group
shuffle <- function(p) p[order(voter_grp, runif(length(p)))]

set.seed(seed)
time <- system.time(
  T_null <- vapply(seq_len(B), function(b) bloc_stat(shuffle(p_obs)),
                   numeric(length(blocs)))
)
T_null <- t(T_null)                 # B x blocs
colnames(T_null) <- names(blocs)
cat(sprintf("%d permutations in %.1f s\n", B, time[["elapsed"]]))

p_val  <- (1 + colSums(sweep(T_null, 2, T_obs, ">="))) / (B + 1)
p_holm <- p.adjust(p_val, method = "holm")

res <- data.frame(
  bloc       = names(blocs),
  countries  = sapply(blocs, paste, collapse = " "),
  votes      = n_bloc,
  T_obs      = round(T_obs, 3),
  null_mean  = round(colMeans(T_null), 3),
  null_q95   = round(apply(T_null, 2, quantile, 0.95), 3),
  p_value    = signif(p_val, 3),
  p_holm     = signif(p_holm, 3),
  row.names  = NULL
)
res <- res[order(-res$T_obs), ]

## ---- top pairs (for the slides) ----------------------------------------

## mean excess per directed pair, pairs that met at least 10 times
e_obs <- excess(p_obs)
pair  <- paste(v$from_country, "->", v$to_country)
meet  <- table(pair)
pm    <- tapply(e_obs, pair, mean)
top   <- data.frame(pair = names(pm), meetings = as.vector(meet[names(pm)]),
                    mean_excess = round(as.vector(pm), 2))
top   <- top[top$meetings >= 10, ]
top   <- head(top[order(-top$mean_excess), ], 15)
rownames(top) <- NULL

## ---- output ------------------------------------------------------------

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
write.csv(res, file.path("results", "act1_blocs.csv"), row.names = FALSE)
write.csv(top, file.path("results", "act1_top_pairs.csv"), row.names = FALSE)
saveRDS(list(T_obs = T_obs, T_null = T_null, B = B, seed = seed),
        file.path("results", "act1_null.rds"))

print(res[, -2], row.names = FALSE)
{
cat("\nTop directed pairs by mean excess points (>= 10 meetings):\n")
print(top, row.names = FALSE)
}

## ---- plot: observed T vs permutation null ------------------------------

plot_blocs <- function(res, T_null) {
  blue <- "#2a78d6"; gray <- "#d3d1ca"
  ink2 <- "#52514e"; muted <- "#898781"
  r  <- res[order(res$T_obs), ]                 # largest at the top
  k  <- nrow(r)
  lo <- apply(T_null[, r$bloc, drop = FALSE], 2, quantile, 0.005)
  hi <- apply(T_null[, r$bloc, drop = FALSE], 2, quantile, 0.995)
  xr <- range(c(lo, hi, r$T_obs, 0))

  op <- par(mar = c(3, 0.6 + max(strwidth(r$bloc, "inches", cex = 0.85)) /
                    par("csi"), 0.6, 0.8), mgp = c(1.9, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))
  plot(NA, xlim = xr + c(0, 0.6), ylim = c(0.5, k + 0.5), yaxt = "n",
       xlab = "", ylab = "", bty = "n", xaxt = "n")
  ax <- pretty(xr)
  abline(v = ax, col = "#ecebe6", lwd = 0.8)
  axis(1, at = ax, lwd = 0, lwd.ticks = 0, line = -0.6)
  title(xlab = "Mean excess points within the bloc", col.lab = ink2,
        cex.lab = 0.9)
  axis(2, at = seq_len(k), labels = r$bloc, lwd = 0, col.axis = ink2,
       cex.axis = 0.85)
  segments(lo, seq_len(k), hi, seq_len(k), col = gray, lwd = 7, lend = 1)
  points(r$T_obs, seq_len(k), pch = 19, col = blue, cex = 1.3)
  text(r$T_obs, seq_len(k), sprintf("%+.1f", r$T_obs), pos = 4,
       cex = 0.75, col = ink2)
  legend("bottomright", bty = "n", cex = 0.75, text.col = ink2,
         legend = c("observed", "99% range if blocs didn't matter"),
         pch = c(19, 15), col = c(blue, gray), pt.cex = c(1.2, 1.8))
}

pdf(file.path("figures", "act1_blocs.pdf"), width = 6, height = 3.4)
plot_blocs(res, T_null); dev.off()
png(file.path("figures", "act1_blocs.png"), width = 1800, height = 1020,
    res = 300)
plot_blocs(res, T_null); dev.off()
