#' Create a result vector for relations with distinct endpoints
#'
#' The six relations with distinct endpoints are commonly observed when
#' comparing indefinite intervals, such as those returned by a Bayesian
#' calibration
#'
#' @author Thomas S. Dye
#'
#' @return A named vector with distinct endpoint relations set to 1
#' and all others set to 0.
#'
  allen.create.distinct.endpoint.vector <- function()
  {
    result.vector <- allen.create.result.vector(initial.value = 0)
    names(result.vector) <- allen.basic.relation.set()
    six <- allen.six.value.set()
    result.vector[six] <- 1
    result.vector
  }

#' Update a result vector
#'
#' Increment the element of the result vector corresponding to the given
#' relation.
#'
#' @param result.vector The result vector to update
#' @param relation The relation to increment
#'
#' @return The updated result vector
#'
#' @author Thomas S. Dye
#'
  allen.update.result <- function(result.vector, relation)
  {
    result.vector[relation] <- result.vector[relation] + 1
    result.vector
  }

#' Calculate the Allen relation of two intervals
#'
#' Given a result vector and four MCMC chains, calculate the Allen relation
#' and update the result vector with the observed relation.
#'
#' @param result.vector A result vector
#' @param mcmc.chain.list A list with four MCMC chains representing the
#' beginning and ending boundaries of two intervals.
#'
#' @return An updated result vector
#'
#' @author Thomas S. Dye
#'
  allen.calculate.relations.2 <- function(result.vector, mcmc.chain.list)
  {
    for(x in seq_len(dim(mcmc.chain.list)[1]))
    {
      relation <- allen.relation(mcmc.chain.list[x, 1],
                                 mcmc.chain.list[x, 2],
                                 mcmc.chain.list[x, 3],
                                 mcmc.chain.list[x, 4])
      result.vector <- allen.update.result(result.vector, relation)
    }
    result.vector
  }

#' Calculate the proportion of each relation in a result vector
#'
#' Divides through by the sum of observations in the result vector.
#' Assigns the names of the result vector to the optionally sorted
#' return vector.
#'
#' @param result_vector A result vector
#' @param sort if TRUE sort in decreasing order else return unsorted vector
#'
#' @return A named vector with proportions
#'
#' @author Thomas S. Dye
#'
allen_proportion_results <- function(result_vector, sort = TRUE)
{
    res <- result_vector / sum(result_vector)
    names(res) <- names(result_vector)
    if(sort)
        res <- sort(res, decreasing = TRUE)
    res
}

#' Compare indefinite intervals
#'
#' Compare two indefinite intervals estimated by four MCMC chains and tally the
#' relations recorded there.  Report the results as a named vector.
#'
#' @param result.vector A result vector
#' @param mcmc.chains A list of four MCMC chains where the first and second
#' members represent the start and end of the first interval, respectively, and
#' the third and fourth represent the start and end of the second interval,
#' respectively.
#'
#' @author Thomas S. Dye
#'
  allen.compare.indeterminate.intervals <- function(result.vector, mcmc.chains)
  {
    if((!is.vector(result.vector)) || (length(result.vector) != 13)
       || (names(result.vector) != c("p", "m", "o", "F", "s", "D", "e", "d",
                                     "S", "f", "O", "M", "P"))
       || (sum(result.vector) != 0))
      stop("result vector is malformed or contains data")
    if(length(unique(lengths(mcmc.chains))) != 1L)
      stop("data parameters must be vectors of the same length")
    for(x in seq_along(mcmc.chains[[1]]))
    {
        result <- allen.relation(mcmc.chains[[1]][x], mcmc.chains[[2]][x],
                                 mcmc.chains[[3]][x], mcmc.chains[[4]][x])
        result.vector <- allen.update.result(result.vector, result)
    }
    result.vector
  }

#' Convert a 13 value result vector to a 6 value vector
#'
#' Converts a full result vector to a vector representing relations
#' with distinct endpoints. By default, the tallies for relations
#' with indistinct endpoints are randomly assigned to one of the
#' relations adjacent in the Nokel lattice.  Optionally, the tallies
#' for relations with indistinct endpoints can be ignored.
#'
#' @param result.vector A full result vector
#' @param include.indistinct When TRUE (default), randomly assign tallies for
#' relations with indistinct endpoints to an adjacent relation.  When FALSE,
#' ignore tallies for relations with indistinct endpoints.
#'
#' @author Thomas S. Dye
#'
allen.coerce.six <- function(result.vector, include.indistinct = TRUE)
{
  ret <- result.vector
  allen.six <- allen.six.value.set()
  if (include.indistinct) {
    allen.other <- allen.complement.set(allen.six)
    for (relation in allen.other)
    {
      neighbors <- switch(relation,
                          "m" = c("p","o"),
                          "F" = c("o", "D"),
                          "s" = c("o", "d"),
                          "e" = c("o", "O", "d", "D"),
                          "S" = c("D", "O"),
                          "f" = c("d", "O"),
                          "M" = c("O", "P"),
                          stop("unrecognized relation"))
      for (foo in seq_len(ret[relation]))
      {
        ret <- allen.update.result(ret, sample(neighbors, 1, replace=TRUE))
        ret[relation] <- ret[relation] - 1
      }
    }
    if (sum(ret[allen.other]) != 0) stop("coercion failed")
  }
  ret <- ret[allen.six]
  ret
}

#' Create a result vector identifying concurrent relations
#'
#' Create a result vector where concurrent relations are set to 1
#' and non-concurrent relations are set to 0.
#'
#' @return A result vector
#'
#' @author Thomas S. Dye
#'
allen.create.concurrent.vector <- function()
{
  result.vector <- allen.create.result.vector(initial.value = 0)
  names(result.vector) <- allen.basic.relation.set()
  concur <- allen.concurrent.relation.set()
  result.vector[concur] <- 1
  result.vector
}

#' Create a named result vector
#'
#' Create a named result vector initialized to zero by default or to some
#' other value.
#'
#' @param initial.value A value used to initialize the vector. typically 0
#' (default) or 1.
#'
#' @return An initialized result vector.
#'
#' @author Thomas S. Dye
#'
allen.create.result.vector <- function(initial.value = 0)
{
  result.vector <- rep(initial.value, times = 13)
  names(result.vector) <- allen.basic.relation.set()
  result.vector
}
