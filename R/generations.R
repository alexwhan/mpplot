#' Finds rows in a data.frame pedigree where the
#' generation changes
#'
#' Takes a data.frame made up of four variables,
#' id, Female, Male, Observed, and returns the
#' row where the generation changes.
#'
#' @param ped A data.frame made up of four
#' variables, id, Female, Male, Observed.
#' @param start The row to start from
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'   Build and Reload Package:  'Ctrl + Shift + B'
#'   Check Package:             'Ctrl + Shift + E'
#'   Test Package:              'Ctrl + Shift + T'

find_gen <- function(ped, start = 1) {
  i <- start
  k <- nrow(ped)
  while(i < k) {
    id <- ped$id[i]
    f <- which(ped$Female == id)[1]
    m <- which(ped$Male == id)[1]
    if(all(is.na(c(f, m)))) k <- k
    else {
      k <- c(k, f, m)[which.min(c(k, f, m))]
    }
    i <- i + 1
  }
  return(k)
}

#' Return a vector of positions defining new generations in a pedigree
#'
#' Takes a data.frame made up of four variables,
#' id, Female, Male, Observed, and returns a
#' vector of rows where the next generation starts.
#'
#' @param ped A data.frame made up of four
#' variables, id, Female, Male, Observed.
#' @param start The row to start from
#'

find_all_gen <- function(ped, start = 1) {
j <- start
gen_starts <- c(j)
while(j < nrow(ped)) {
  j <- find_gen(ped, start = j)
  if(j < nrow(ped)) gen_starts <- c(gen_starts, j)
}
return(gen_starts)
}

#' Return a vector describing each generation of an entry in a pedigree
#'
#' Takes a data.frame pedigree and returns a vector of the same length defining which generation each row of the pedigree fits.
#'
#' @param ped A data.frame made up of four
#' variables, id, Female, Male, Observed.
#' @export
#'

make_gen <- function(ped) {
  gen_starts <- find_all_gen(ped)
  gen_vec <- rep(1:length(gen_starts), diff(c(gen_starts, (nrow(ped) + 1))))
  return(gen_vec)
}
