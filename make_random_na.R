make_random_na <- function(df,thresh,numcols) {

  # Function to introduce NAs at random into a data frame
  #
  # df - the data frame into which to introduce NAs
  # thresh - what percentage of a column to assign NAs
  # numcols - how many columns into which to introduce NAs

  # To make the introduction of NAs even more random
  # we'll use the threshold in conjunction with some noise

  idx <- sample(ncol(df),numcols)
  noise <- runif(numcols,0,2)

  # For each of those column numbers, introduce some number of
  # NAs into that column. How many is determined by the threshold
  # in combination with a random noise factor

  for (ii in 1:length(idx)) {
    # Get random index for columns into which we introduce NAs

    how_many <- round(length((sample(nrow(df),nrow(df)*thresh)))*runif(1,0,1.4))+1
    row_idx  <- sample(1:nrow(df),how_many)
    df[row_idx,idx[ii]] <- NA
  }

  return(df)
}
