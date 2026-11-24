## hook.R
## Slide 5 "Does one country's vote matter?"
## How close were the finals (1975 on), measured in one country's top vote?
##
## Scale change: up to 2015 a country could give one entry at most 12 points;
## since 2016 jury and public each award 12, so the maximum is 24. The winning
## margin is divided by that maximum ("top votes"), so the threshold
## "smaller than one country's top vote" is 1 in every year
##
## Run from the project folder:  Rscript 02_scripts/hook.R
## Output: 01_data/margins.csv (read by the slides),
##         04_figures/hook_margins.pdf (+ .png), 04_figures/hook_table.tex

## ---- functions ---------------------------------------------------------

## One row per final: winner, runner-up, margin in points and in top votes
final_margins <- function(entries, from = 1975) {
  f <- entries[entries$round == "final" & entries$year >= from &
               !is.na(entries$place), ]
  rows <- lapply(split(f, f$year), function(d) {
    w <- d[d$place == 1, ][1, ]
    s <- d[d$place == 2, ][1, ]
    data.frame(year = w$year, winner = w$country, runner_up = s$country,
               points_winner = w$points_total, points_second = s$points_total)
  })
  m <- do.call(rbind, rows)
  m$margin    <- m$points_winner - m$points_second
  m$top_vote  <- ifelse(m$year >= 2016, 24, 12)
  m$margin_tv <- m$margin / m$top_vote
  rownames(m) <- NULL
  m
}

## Bar chart: margin in top votes per year, close finals highlighted
## (not used in the slides)
plot_margins <- function(m) {
  blue <- "#2a78d6"; gray <- "#d3d1ca"
  ink  <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"

  yrs <- seq(min(m$year), max(m$year))
  h   <- m$margin_tv[match(yrs, m$year)]          # NA for 2020 (cancelled)
  close <- !is.na(h) & h < 1
  ymax <- ceiling(max(h, na.rm = TRUE))
  ## a tie (1991) has height 0: draw a thin stub so it stays visible
  hb <- ifelse(is.na(h), 0, pmax(h, ifelse(close, 0.12, 0)))

  op <- par(mar = c(2.2, 3.6, 0.6, 0.5), mgp = c(2.2, 0.5, 0), las = 1,
            col.axis = muted, fg = muted, cex.axis = 0.8)
  on.exit(par(op))

  x <- barplot(hb, col = ifelse(close, blue, gray),
               border = NA, space = 0.25, ylim = c(0, ymax), axes = FALSE)
  abline(h = seq(0, ymax, by = 2), col = "#ecebe6", lwd = 0.8)
  barplot(hb, col = ifelse(close, blue, gray),
          border = NA, space = 0.25, axes = FALSE, add = TRUE)
  axis(2, at = seq(0, ymax, by = 2), lwd = 0, lwd.ticks = 0)
  title(ylab = "Winning margin (top votes)", col.lab = ink2, cex.lab = 0.9)

  ## x axis: every 5th year
  tick <- yrs %% 5 == 0
  axis(1, at = x[tick], labels = yrs[tick], lwd = 0, lwd.ticks = 0, line = -0.6)

  ## threshold: one country's top vote
  abline(h = 1, col = ink, lwd = 1.5, lty = 2)

  ## rule change
  i16 <- which(yrs == 2016)
  abline(v = x[i16] - 0.625, col = muted, lwd = 1, lty = 3)
  text(x[i16] - 0.625, ymax * 0.97, "2016: jury + public\nvote separately",
       adj = c(-0.05, 1), col = ink2, cex = 0.7)

  ## legend in the empty top-left corner
  legend("topleft", inset = c(0.01, 0), bty = "n", cex = 0.75,
         text.col = ink2, y.intersp = 1.3,
         legend = c(sprintf("%d finals won by less than one top vote",
                            sum(close)),
                    "one country's top vote (12 pts; 24 since 2016)"),
         fill = c(blue, NA), border = NA, col = c(NA, ink),
         lty = c(NA, 2), lwd = c(NA, 1.5), merge = TRUE)
  invisible(x)
}

## LaTeX (booktabs) table of the finals decided by less than one top vote
## (not used in the slides)
margin_table_tex <- function(m, max_tv = 1) {   # strictly below max_tv
  esc <- function(s) gsub("&", "\\\\&", s)
  t <- m[m$margin_tv < max_tv, ]
  t <- t[order(t$margin_tv, t$year), ]
  body <- sprintf("%d & %s & %s & %d--%d & \\textbf{%d} & %.2f \\\\",
                  t$year, esc(t$winner), esc(t$runner_up),
                  t$points_winner, t$points_second, t$margin, t$margin_tv)
  c("\\begin{tabular}{rllrrr}",
    "\\toprule",
    "Year & Winner & Runner-up & Points & Margin & Top votes \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}")
}

## ---- main (only when run as a script, not when sourced) --------------------------------------------------------------

if (sys.nframe() == 0) {
  entries <- read.csv(file.path("01_data", "entries.csv"))
  m <- final_margins(entries)
  dir.create("04_figures", showWarnings = FALSE)

  write.csv(m, file.path("01_data", "margins.csv"), row.names = FALSE)

  pdf(file.path("04_figures", "hook_margins.pdf"), width = 6, height = 3.4)
  plot_margins(m); dev.off()
  png(file.path("04_figures", "hook_margins.png"), width = 1800, height = 1020,
      res = 300)
  plot_margins(m); dev.off()

  tex <- margin_table_tex(m)
  writeLines(tex, file.path("04_figures", "hook_table.tex"))

  close <- m[m$margin_tv < 1, ]
  cat(sprintf("Finals 1975-%d: %d; decided by < one top vote: %d\n",
              max(m$year), nrow(m), nrow(close)))
  print(close[order(close$margin_tv), c("year", "winner", "runner_up",
                                        "margin", "margin_tv")],
        row.names = FALSE)
}
