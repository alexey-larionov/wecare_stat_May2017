# f01_calculate_eigenvectors.R
# Function to calculate eigenvectors
# Implements procedure described by Price et al 2006 (PMID: 16862161)
# Started: Alexey Larionov, 23Mar2017
# Last updated: Alexey Larionov, 23Mar2017

normalise_and_calculate_eigenvectors.udf <- function(x) {
  
  # --- Center and normalise variants (rows) --- #
  
  # Center by mean
  avg.rows <- apply(x, 1, mean, na.rm=TRUE)
  x.c <- x - avg.rows
  
  # Normalise by sqrt(p(1-p)) where p~"posterior estimate of unobserved allele frequency"
  # This is motivated by the fact that genetic drift per generation is proportional to this normalisation value (Patterson 2006)
  # Also this makes each column to have same variance
  # 
  p.fnc <- function(x) (1 + sum(x, na.rm=TRUE)) / (2 + 2 * sum(!is.na(x)))
  p <- apply(x, 1, p.fnc)
  eaf <- sqrt(p*(1-p))
  x.cn <- x.c/eaf
  
  # Substitute NAs to zeros
  0 -> x.cn[is.na(x)]
  
  # --- Calculate eigenvectors of covariance matrix of cases --- #
  
  cov.mx <- cov(x.cn)
  eig <- eigen(cov.mx) # eigenvectors in columns
  
  return(eig)
  
}

