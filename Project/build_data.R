## build_data.R
## Convert the EurovisionAPI dataset (JSON, one file per show) into flat CSVs
##
## Source: https://github.com/EurovisionAPI/dataset  
##
## Output (in data/):
##   votes.csv    one row per year x round x vote_type x voter x recipient
##                (zero-point votes included; self-votes never appear)
##   entries.csv  one row per performance: running order, place, points
##   contests.csv one row per year: host city / country
##
## vote_type: "total"  = points that counted (all years)
##            "jury", "public" = separate components, finals and semis 2016 onward
##            From 2023 semi-finals are public-only, so there only "total" exists
## from_code "WLD" = "Rest of the World" online televote (2023 onward), not a country

library(jsonlite)

src  <- file.path("data", "eurovision-api", "data")
root <- file.path(src, "senior")
stopifnot(dir.exists(root))

countries <- unlist(fromJSON(file.path(src, "countries.json")))
countries["WLD"] <- "Rest of the World"
cname <- function(code) unname(countries[code])

years <- sort(as.integer(list.files(root, pattern = "^[0-9]{4}$")))

votes_list   <- list()
entries_list <- list()
contest_list <- list()

for (yr in years) {
  ydir <- file.path(root, yr)

  ## contest metadata
  ct <- fromJSON(file.path(ydir, "contest.json"), simplifyVector = FALSE)
  contest_list[[length(contest_list) + 1]] <- data.frame(
    year = yr,
    host_city = if (is.null(ct$city)) NA else ct$city,
    host_code = if (is.null(ct$country)) NA else ct$country,
    arena = if (is.null(ct$arena)) NA else ct$arena
  )

  ## contestants: id -> country, artist, song
  cdirs <- list.files(file.path(ydir, "contestants"), full.names = TRUE)
  cons <- do.call(rbind, lapply(cdirs, function(d) {
    x <- fromJSON(file.path(d, "contestant.json"), simplifyVector = FALSE)
    data.frame(contestant_id = x$id, country_code = x$country,
               artist = if (is.null(x$artist)) NA else x$artist,
               song   = if (is.null(x$song)) NA else x$song)
  }))

  ## rounds
  for (rf in list.files(file.path(ydir, "rounds"), pattern = "\\.json$",
                        full.names = TRUE)) {
    round <- sub("\\.json$", "", basename(rf))
    r <- fromJSON(rf, simplifyVector = FALSE)
    dq <- unlist(r$disqualifieds)

    for (p in r$performances) {
      to <- cons$country_code[cons$contestant_id == p$contestantId]
      pts <- setNames(rep(NA_integer_, 3), c("total", "jury", "public"))
      for (s in p$scores) {
        if (!is.null(s$points)) pts[s$name] <- s$points
        v <- unlist(s$votes)
        if (length(v)) {
          votes_list[[length(votes_list) + 1]] <- data.frame(
            year = yr, round = round, vote_type = s$name,
            from_code = names(v), to_code = to, points = as.integer(v)
          )
        }
      }
      ci <- cons$contestant_id == p$contestantId
      entries_list[[length(entries_list) + 1]] <- data.frame(
        year = yr, round = round, contestant_id = p$contestantId,
        country_code = to, artist = cons$artist[ci], song = cons$song[ci],
        running = if (is.null(p$running)) NA else p$running,
        place   = if (is.null(p$place)) NA else p$place,
        points_total = pts[["total"]], points_jury = pts[["jury"]],
        points_public = pts[["public"]],
        disqualified = p$contestantId %in% dq
      )
    }
  }
}

votes    <- do.call(rbind, votes_list)
entries  <- do.call(rbind, entries_list)
contests <- do.call(rbind, contest_list)

## readable names and edition key (e.g. "2019-final")
votes$from_country  <- cname(votes$from_code)
votes$to_country    <- cname(votes$to_code)
votes$edition       <- paste(votes$year, votes$round, sep = "-")
entries$country     <- cname(entries$country_code)
entries$edition     <- paste(entries$year, entries$round, sep = "-")
contests$host_country <- cname(contests$host_code)

votes <- votes[order(votes$year, votes$round, votes$vote_type,
                     votes$from_code, votes$to_code),
               c("year", "round", "edition", "vote_type", "from_code",
                 "from_country", "to_code", "to_country", "points")]
entries <- entries[order(entries$year, entries$round, entries$running),
                   c("year", "round", "edition", "contestant_id",
                     "country_code", "country", "artist", "song", "running",
                     "place", "points_total", "points_jury", "points_public",
                     "disqualified")]

write.csv(votes,    file.path("data", "votes.csv"),    row.names = FALSE)
write.csv(entries,  file.path("data", "entries.csv"),  row.names = FALSE)
write.csv(contests, file.path("data", "contests.csv"), row.names = FALSE)

cat("votes:", nrow(votes), "rows,", paste(range(votes$year), collapse = "-"), "\n")
cat("entries:", nrow(entries), "rows\n")
cat("contests:", nrow(contests), "rows\n")
