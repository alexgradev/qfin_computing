## network.R
## Network graph of the strongest voting ties: an arrow A -> B means country
## A gives B on average more than `threshold` excess points (points A gave B
## minus the mean B got from all other voters in the same show).
## Same pair definition as the top-pairs table in bloc_favoritism.R:
## finals and semi-finals 1975-2026, "total" points (2016+ halved to the
## 12-point scale), pairs that met at least 10 times.
##
## Run from the project folder:  Rscript network.R
## Output: figures/voting_network.pdf (+ .png), results/network_edges.csv

library(igraph)

threshold    <- 3.0
min_meetings <- 10

## ---- pair excess points ------------------------------------------------

v <- read.csv(file.path("data", "votes.csv"))
v <- v[v$vote_type == "total" & v$year >= 1975 & v$from_code != "WLD", ]
v$points <- ifelse(v$year >= 2016, v$points / 2, v$points)

key <- paste(v$edition, v$to_code)
s   <- ave(v$points, key, FUN = sum)
n   <- ave(v$points, key, FUN = length)
v$excess <- v$points - (s - v$points) / (n - 1)

k <- paste(v$from_code, v$to_code)
pairs <- data.frame(
  from_code = tapply(v$from_code, k, `[`, 1),
  to_code   = tapply(v$to_code, k, `[`, 1),
  meetings  = as.vector(table(k)[sort(unique(k))]),
  excess    = tapply(v$excess, k, mean),
  row.names = NULL
)
pairs <- pairs[pairs$meetings >= min_meetings, ]

## ---- network graph of strong ties --------------------------------------

## only very strong ties (more than `threshold` points above the average)
strong_ties <- pairs[pairs$excess > threshold, c("from_code", "to_code", "excess")]
strong_ties <- strong_ties[order(-strong_ties$excess), ]

g <- graph_from_data_frame(strong_ties, directed = TRUE)
cat(sprintf("Strong ties (> %.1f excess points, >= %d meetings): %d edges, %d countries\n",
            threshold, min_meetings, ecount(g), vcount(g)))

plot_network <- function(g) {
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  set.seed(2026)                                # reproducible layout
  plot(g,
       layout = layout_with_fr(g),              # Fruchterman-Reingold: clusters
       vertex.size = 12,
       vertex.color = "#cfe0f5",
       vertex.frame.color = "white",
       vertex.label.color = "#1E1B6E",
       vertex.label.cex = 0.75,
       vertex.label.family = "sans",
       vertex.label.font = 2,
       edge.arrow.size = 0.4,
       edge.color = adjustcolor("#52514e", 0.6),
       edge.width = E(g)$excess / 2,            # thicker arrow = more excess
       edge.curved = 0.15)
}

dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
pdf(file.path("figures", "voting_network.pdf"), width = 7, height = 7)
plot_network(g); dev.off()
png(file.path("figures", "voting_network.png"), width = 1800, height = 1800,
    res = 300)
plot_network(g); dev.off()

write.csv(strong_ties, file.path("results", "network_edges.csv"),
          row.names = FALSE)

## clusters (connected groups of countries, ignoring arrow direction)
cl <- components(g, mode = "weak")
for (i in seq_len(cl$no))
  cat(sprintf("Cluster %d: %s\n", i, paste(names(cl$membership)[cl$membership == i],
                                          collapse = ", ")))
