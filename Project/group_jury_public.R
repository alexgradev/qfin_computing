## group_jury_public.R
## Case studies: Greece-Cyprus, Romania-Moldova, ex-Yugoslavia.
## Is the favouritism inside each group driven by the jury, the public, or
## both? ("family" voting: jury and public both biased;
##  "diaspora" voting: mainly the public)
##
## Data: data/votes.csv (build_data.R), shows with both jury and public points
## (finals 2016-2026, semi-finals 2016-2022 and 2026), "Rest of the World"
## dropped. Excess points as in Act 1, computed separately per vote type:
##   e[A->B,t] = p[A->B,t] - mean points B got from the other voters of the
##               same type (jury or public) in show t
##
## Three permutation tests per group (statistics = mean over within-group
## votes, A != B):
##   1. jury bias    T = mean jury excess.    Null: shuffle each jury's points
##                   over the entries within its show (as in Act 1)
##   2. public bias  T = mean public excess.  Same, for the public points
##   3. gap          T = mean(public excess - jury excess).
##                   Null: "jury" and "public" labels don't matter, so for each
##                   voter and show, swap its jury and public points with
##                   probability 1/2 (paired permutation test)
## One-sided p-values with the +1 correction, Holm across the 3 groups within
## each test. The groups were picked after looking at the data, so treat the
## results as exploratory.
##
## Run from the project folder:  Rscript group_jury_public.R
## Output: results/group_jury_public.csv, figures/group_jury_public.pdf (+ .png)

## ---- settings ----------------------------------------------------------

B    <- 10000
seed <- 2026

groups <- list(
  "Greece & Cyprus"   = c("GR", "CY"),
  "Romania & Moldova" = c("RO", "MD"),
  "ex-Yugoslavia"     = c("RS", "HR", "SI", "BA", "MK", "ME")
)

## ---- data: one row per vote with jury and public points ----------------

v <- read.csv(file.path("data", "votes.csv"))
v <- v[v$year >= 2016 & v$vote_type %in% c("jury", "public") &
       v$from_code != "WLD", ]
j <- v[v$vote_type == "jury",   c("edition", "from_code", "to_code", "points")]
p <- v[v$vote_type == "public", c("edition", "from_code", "to_code", "points")]
names(j)[4] <- "pj"; names(p)[4] <- "pp"
w <- merge(j, p, by = c("edition", "from_code", "to_code"))

## sort by voter-show so within-group shuffles work by ordering
w <- w[order(w$edition, w$from_code, w$to_code), ]
rownames(w) <- NULL

voter_grp <- as.integer(factor(paste(w$edition, w$from_code)))  # shuffle unit
recip_grp <- as.integer(factor(paste(w$edition, w$to_code)))    # recipient
n_recip   <- tabulate(recip_grp)
stopifnot(all(n_recip >= 2))

## within-group votes, stacked over groups (as in bloc_favoritism.R)
rows_per_group <- lapply(groups, function(g)
  which(w$from_code %in% g & w$to_code %in% g))
grp_rows <- unlist(rows_per_group, use.names = FALSE)
grp_idx  <- rep(seq_along(groups), lengths(rows_per_group))
n_grp    <- lengths(rows_per_group)
stopifnot(all(n_grp > 0))

cat(sprintf("Votes: %d in %d shows; within-group votes: %s\n", nrow(w),
            length(unique(w$edition)),
            paste(names(groups), n_grp, sep = " = ", collapse = ", ")))

## ---- statistics --------------------------------------------------------

group_sum <- function(x, g) as.vector(rowsum(x, g, reorder = TRUE))

excess <- function(p) {
  s <- group_sum(p, recip_grp)[recip_grp]
  p - (s - p) / (n_recip[recip_grp] - 1)
}

group_mean <- function(e) group_sum(e[grp_rows], grp_idx) / n_grp

## shuffle points within each voter-show (rows are sorted by voter_grp)
shuffle <- function(p) p[order(voter_grp, runif(length(p)))]

## swap jury and public for a random half of the voter-shows
swap_types <- function() {
  flip <- (runif(max(voter_grp)) < 0.5)[voter_grp]
  list(pj = ifelse(flip, w$pp, w$pj), pp = ifelse(flip, w$pj, w$pp))
}

## ---- observed ----------------------------------------------------------

ej <- excess(w$pj)
ep <- excess(w$pp)
T_jury   <- group_mean(ej)
T_public <- group_mean(ep)
T_gap    <- group_mean(ep - ej)

## ---- permutation tests -------------------------------------------------

set.seed(seed)
time <- system.time({
  null_jury   <- replicate(B, group_mean(excess(shuffle(w$pj))))
  null_public <- replicate(B, group_mean(excess(shuffle(w$pp))))
  null_gap    <- replicate(B, {
    s <- swap_types()
    group_mean(excess(s$pp) - excess(s$pj))
  })
})
cat(sprintf("3 x %d permutations in %.1f s\n\n", B, time[["elapsed"]]))

## null_* are groups x B matrices
p_one <- function(obs, null) (1 + rowSums(null >= obs)) / (B + 1)
p_jury   <- p_one(T_jury, null_jury)
p_public <- p_one(T_public, null_public)
p_gap    <- p_one(T_gap, null_gap)

res <- data.frame(
  group          = names(groups),
  votes          = n_grp,
  public_excess  = round(T_public, 2),
  jury_excess    = round(T_jury, 2),
  gap            = round(T_gap, 2),
  gap_null_q95   = round(apply(null_gap, 1, quantile, 0.95), 2),
  p_public       = signif(p_public, 3),
  p_jury         = signif(p_jury, 3),
  p_gap          = signif(p_gap, 3),
  p_public_holm  = signif(p.adjust(p_public, "holm"), 3),
  p_jury_holm    = signif(p.adjust(p_jury, "holm"), 3),
  p_gap_holm     = signif(p.adjust(p_gap, "holm"), 3),
  row.names      = NULL
)
print(res, row.names = FALSE)

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
write.csv(res, file.path("results", "group_jury_public.csv"), row.names = FALSE)

## ---- plot: public vs jury excess per group -----------------------------

plot_groups <- function(res) {
  blue <- "#2a78d6"; orange <- "#eb6834"
  ink2 <- "#52514e"; muted <- "#898781"
  m <- rbind(Public = res$public_excess, Jury = res$jury_excess)
  colnames(m) <- res$group
  ymax <- ceiling(max(m) + 1.5)

  op <- par(mar = c(2.4, 3.6, 0.6, 0.5), mgp = c(2.2, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))
  x <- barplot(m, beside = TRUE, col = c(blue, orange), border = "white",
               lwd = 2, ylim = c(0, ymax), axes = FALSE, names.arg = rep("", ncol(m)),
               space = c(0.15, 0.9))
  abline(h = seq(0, ymax, by = 2), col = "#ecebe6", lwd = 0.8)
  barplot(m, beside = TRUE, col = c(blue, orange), border = "white", lwd = 2,
          axes = FALSE, add = TRUE, names.arg = rep("", ncol(m)), space = c(0.15, 0.9))
  axis(2, at = seq(0, ymax, by = 2), lwd = 0)
  axis(1, at = colMeans(x), labels = colnames(m), lwd = 0, line = -0.8,
       col.axis = ink2, cex.axis = 0.85)
  title(ylab = "Mean excess points within the group", col.lab = ink2,
        cex.lab = 0.85)
  text(x, m, sprintf("%+.1f", m), pos = 3, cex = 0.7, col = ink2,
       offset = 0.25)
  ## gap above each pair of bars
  stars <- ifelse(res$p_gap_holm < 0.05, " *", "")
  text(colMeans(x), apply(m, 2, max) + 1.2,
       sprintf("gap %+.1f%s", res$gap, stars), cex = 0.72, col = ink2)
  legend("topright", bty = "n", cex = 0.75, text.col = ink2,
         legend = c("Public", "Jury"), fill = c(blue, orange), border = NA)
}

pdf(file.path("figures", "group_jury_public.pdf"), width = 6, height = 3.4)
plot_groups(res); dev.off()
png(file.path("figures", "group_jury_public.png"), width = 1800,
    height = 1020, res = 300)
plot_groups(res); dev.off()
