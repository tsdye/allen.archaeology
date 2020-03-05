#' The basic Allen relation set
#'
#' A vector of one-letter codes for the thirteen basic Allen relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of thirteen one-letter codes
#'
#' @author Thomas S. Dye
#'
allen.basic.relation.set <- function()
{
 ret <- c("p", "m", "o", "F", "s", "D", "e", "d", "S", "f", "O", "M", "P")
  ret
}

#' Allen concurrent relation set
#'
#' A vector of nine one-letter codes for the Allen concurrent relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of nine one-letter codes.
#'
  allen.concurrent.relation.set <- function()
  {
    ret <- c("o", "F", "D", "s", "e", "S", "d", "f", "O")
    ret
  }

#' Allen basic relation set complement
#'
#' Returns the complement of the set passed as an argument.
#'
#' @param allen.set An Allen set or result vector
#'
#' @return An Allen set, a vector of one-letter codes.
#'
#' @author Thomas S. Dye
#'
  allen.complement.set <- function(allen.set)
  {
    allen.full.set <- allen.basic.relation.set()
    switch(mode(allen.set),
           "NULL" = result.set <- "",
           "character" = result.set <- allen.set,
           "numeric" = result.set <- names(allen.set[allen.set != 0]),
           stop("unrecognized Allen set"))
    complement.set <- setdiff(allen.full.set, result.set)
    complement.set
  }

#' Intersection of Allen relation sets
#'
#' Calculates the intersection of two Allen relation sets.  Attempts to
#' handle sets represented by result vectors, as well as sets represented
#' by vectors of one-letter codes.
#'
#' @param allen.set.1 An Allen set
#' @param allen.set.2 An Allen set
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
  allen.relations.intersection <- function(allen.set.1, allen.set.2)
  {
    switch(mode(allen.set.1),
           NULL = result.set.1 <- NULL,
           "character" = result.set.1 <- allen.set.1,
           "numeric" = result.set.1 <- names(allen.set.1[allen.set.1 != 0]),
           stop("unrecognized Allen set"))
    switch(mode(allen.set.2),
           NULL = result.set.2 <- NULL,
           "character" = result.set.2 <- allen.set.2,
           "numeric" = result.set.2 <- names(allen.set.2[allen.set.2 != 0]),
           stop("unrecognized Allen set"))
    ret <- intersect(result.set.1, result.set.2)
    ret
  }

#' The Allen relation set represented by the non-zero elements of a result vector
#'
#' Given a result vector, returns a vector of the one-letter codes corresponding to
#' the non-zero elements of the result vector.
#'
#' @param result.vector A result vector
#'
#' @return An Allen set
#'
#' @author Thomas S. Dye
#'
  allen.set.vector.from.result <- function(result.vector)
  {
    ret <- names(result.vector[result.vector != 0])
    ret
  }

#' Coerce a string to an Allen relation set.
#'
#' Coerces a string to a vector representing an Allen relation set.
#' Does not check if the string represents an Allen set correctly.
#'
#' @param allen.set.string A string whose elements are expected to be one-letter
#' codes for Allen relations following the convention proposed by Thomas
#' Alspaugh.
#'
#' @author Thomas S. Dye
#'
  allen.set.vector <- function(allen.set.string)
  {
    ret <- unlist(strsplit(allen.set.string, ""))
    ret
  }

#' Union of two Allen relation sets.
#'
#' Returns the union of two Allen relation sets, taking care to handle
#' empty sets and the sets represented by result vectors.
#'
#' @param allen.set.1 The first Allen relation set or result vector
#' @param allen.set.2 The second Allen relation set or result vector
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
  allen.relations.union <- function(allen.set.1, allen.set.2)
  {
    switch(mode(allen.set.1),
           NULL = result.set.1 <- NULL,
           "character" = result.set.1 <- allen.set.1,
           "numeric" = result.set.1 <- names(allen.set.1[allen.set.1 != 0]),
           stop("unrecognized Allen set"))
    switch(mode(allen.set.2),
           NULL = result.set.2 <- NULL,
           "character" = result.set.2 <- allen.set.2,
           "numeric" = result.set.2 <- names(allen.set.2[allen.set.2 != 0]),
           stop("unrecognized Allen set"))
    ret <- union(result.set.1, result.set.2)
    ret
  }

#' Allen relation set converse.
#'
#' Calculates the converse of an Allen relation set, taking care to
#' convert a result vector to a relation set.
#'
#' @param allen.set An Allen relation set or result vector
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
  allen.converse.set <- function(allen.set)
  {
    allen.converse <- function(relation)
    {
      result <- switch(relation,
                       "p" = "P",
                       "m" = "M",
                       "o" = "O",
                       "F" = "f",
                       "D" = "d",
                       "s" = "S",
                       "e" = "e",
                       "S" = "s",
                       "d" = "D",
                       "f" = "F",
                       "O" = "o",
                       "M" = "m",
                       "P" = "p",
                       NULL = NULL,
                       stop("unrecognized Allen relation identifier"))
      result
    }
    switch(mode(allen.set),
           NULL = result.set <- NULL,
           "character" = result.set <- allen.set,
           "numeric" = result.set <- names(allen.set[allen.set != 0]),
           stop("unrecognized Allen set"))
    if (is.null(result.set))
      converse.set <- NULL
    else
      converse.set <- sapply(result.set, allen.converse)
    converse.set
  }

#' Allen relation set for intervals with distinct endpoints.
#'
#' Return the six value Allen relation set for intervals with distinct
#' endpoints.
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
  allen.six.value.set <- function()
  {
    ret <- c("p", "o", "D", "d", "O", "P");
    ret
  }
