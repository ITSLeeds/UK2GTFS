make_data <- function(n) {
  lst <- list()
  for (i in 1:n) {
    nr <- sample(1:20, 1)
    tmp <- data.frame(
      Freq = rep(sample(1:5, 1), nr),
      Sequence = seq(1, nr),
      Stations = sample(LETTERS, nr, replace = TRUE),
      stringsAsFactors = FALSE
    )
    lst[[i]] <- tmp
  }
  lst <- dplyr::bind_rows(lst)
  return(lst)
}

data <- make_data(100)

# Much fater method of duplicatting data fram rows.
# Need to check that sequences always start with a 1
# Need to produce and index for duplication i.e. 1st duplication, second duplication etc

duplicated_ids <- function(Freq, Sequence) {
  row <- seq(1, length(Freq))
  row_start <- row[Sequence == 1] # Check the Sequneces always start at 1
  row_end <- row_start - 1
  row_end <- c(row_end[seq(2, length(row_end))], length(row))
  freq_start <- Freq[row_start]
  inputs <- matrix(c(row_start, row_end, freq_start), ncol = 3)

  int_func <- function(start, end, freq) {
    rep(seq(start, end), times = freq)
  }

  ids <- mapply(FUN = int_func, start = row_start, end = row_end, freq = freq_start)
  ids <- unlist(ids)
}

ids <- duplicated_ids(Freq = data$Freq, Sequence = data$Sequence)
data2 <- data[ids, ]
index <- sapply(1:length(ids), function(i) sum(ids[i] == ids[1:i]))

plot(ids)
