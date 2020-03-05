#' Dataframe for comparing one or more stratigraphic phases
#' with a cultural phase.
#'
#' Reads MCMC output from Chronomodel to create a dataframe suitable
#' for plotting the Allen relations between stratigraphic phases and
#' a cultural phase using the six-value logic for phases with distinct
#' endpoints.
#'
#' @author Thomas S. Dye
#'
#' @param chronomodel_mcmc path to a csv file with Chronomodel MCMC output
#' @param stratigraphic_phases vector of column indices, typically for the
#' beginning and end of one or more stratigraphic phases
#' @param cultural_phase vector of column indices for the beginning and end
#' of a cultural phase
#'
#' @return A dataframe suitable for plotting with ggraph
#' @export
#'
allen_relate_stratification_culture <- function(chronomodel_mcmc,
                                                stratigraphic_phases,
                                                cultural_phase) {
  relations <- NULL
  mcmc <- ArchaeoPhases::read_chronomodel(chronomodel_mcmc)
  for(ind in stratigraphic_phases) {
    if(is.odd(ind)) {
      positions <- c(ind, ind + 1, cultural_phase)
      chains <- mcmc[,positions]
      names <- allen.check.names(colnames(chains))
      zero.vector <- allen.create.result.vector()
      result.vector <- allen.calculate.relations.2(zero.vector, chains)
      result.vector.six <- allen.coerce.six(result.vector)
      result <- allen.proportion.results(result.vector.six)
      node <- c('precedes', 'overlaps', 'contains', 'during',
                'overlapped-by', 'preceded-by')
      relation_string <- allen_code_to_string(names(result[1]))
      x <- c(0, 0, 0.5, -0.5, 0, 0)
      y <- c(2, 1.5, 1, 1, 0.5, 0)
      title <- rep(sprintf("%s %s %s", names$first, relation_string,
                           names$second), length(node))
      this.relation <- cbind.data.frame(x, y, result, node, title)
      relations <- rbind.data.frame(relations, this.relation)
    }
  }
  relations$title <- factor(relations$title, levels = sort(levels(relations$title)))
  ## allen.set <- allen.relations.set(result.vector)
  allen.concurs <- allen.relations.concur(result.vector)
  allen.result <- result.vector
  allen.set <- allen.set.vector.from.result(result.vector)
  allen.proportion <- allen.proportion.results(
    result.vector[result.vector != 0])
  allen.proportion <- sort(allen.proportion, decreasing=TRUE)
  concurrence.intersection <- allen.relations.intersection(
    allen.proportion, allen.concurrent.relation.set())
  allen.proportion.concurs <- sum(allen.proportion[concurrence.intersection])
  list(relations = relations, result = allen.result, set = allen.set,
       proportion = allen.proportion, concurrence = allen.proportion.concurs)
}

#' Prepare data for an illustrative graphic
#'
#' Create a dataframe that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators.  Able to illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints (six-value set).
#'
#' @author Thomas S. Dye
#'
#' @param subset One of "none" (default),
#' "concurrent" (two highlight concurrent relations),
#' or "six" (to highlight relations with distinct endpoints)
#'
#' @return A dataframe for input to ggraph
#'
#' @export
#'
illustrate.allen.relations <- function(subset = "none") {
  result <-   switch(subset,
                     none = allen.create.result.vector(initial.value = 1),
                     concurrent = allen.create.concurrent.vector(),
                     six = allen.create.distinct.endpoint.vector(),
                     stop(sprintf("Unknown subset, '%s'", subset)))
  node <- c('precedes', 'meets', 'overlaps', 'finished-by', 'starts',
            'contains', 'equals', 'during', 'started-by', 'finishes',
            'overlapped-by', 'met-by', 'preceded-by')
  x <- c(0, 0, 0, -1, 1, -2, 0, 2, -1, 1, 0, 0, 0)
  y <- c(8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 1, 0)
  df <- cbind.data.frame(x, y, result, node)
  df
}
