#' Relate phases
#'
#' Reads MCMC output to create a dataframe suitable
#' for plotting the Allen relations between two sets of phases
#' using the six-value logic for intervals with distinct
#' endpoints.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases_1 vector of column indices for the
#' beginning and end of one or more phases
#' @param phases_2 vector of column indices for the beginning and end
#' of one or more phases
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#'
#' @return A dataframe suitable for plotting with ggraph
#' @export
#'
allen_relate_phases <- function(mcmc,
                                phases_1,
                                phases_2,
                                app) {
    just_odd <- function(x) x[ x %% 2 == 1 ]
    relations <- NULL
    chains <- switch(app,
                     chronomodel = ArchaeoPhases::read_chronomodel(mcmc),
                     oxcal = ArchaeoPhases::read_oxcal(mcmc),
                     bcal = ArchaeoPhases::read_bcal(mcmc))
    term_1 <- just_odd(phases_1)
    term_2 <- just_odd(phases_2)
    for(ind_2 in term_2) {
        positions_2 <- c(ind_2, ind_2 + 1)
        for(ind_1 in term_1) {
            positions_1 <- c(ind_1, ind_1 + 1, positions_2)
            chains <- chains[,positions_1]
            names <- allen.check.names(colnames(chains))
            zero.vector <- allen.create.result.vector()
            result.vector <- allen.calculate.relations.2(zero.vector, chains)
            result.six <- allen.coerce.six(result.vector)
            result <- allen_proportion_results(result.six)
            node <- c('precedes', 'overlaps', 'contains', 'during',
                      'overlapped by', 'preceded by')
            max_code <- names(result.six[result.six == max(result.six)])
            relation_string <- allen_code_to_string(max_code)
            x <- c(0, 0, 0.5, -0.5, 0, 0)
            y <- c(2, 1.5, 1, 1, 0.5, 0)
            title <- rep(sprintf("%s %s %s", names$first, relation_string,
                                 names$second), length(node))
            this.relation <- cbind.data.frame(x, y, result, node, title)
            relations <- rbind.data.frame(relations, this.relation)
        }
    }
    ## relations$title <- factor(relations$title, levels = sort(levels(relations$title)))
    ## allen.concurs <- allen.relations.concur(result.vector)
    ## allen.result <- result.vector
    ## allen.set <- allen.set.vector.from.result(result.vector)
    ## allen.proportion <- allen.proportion.results(
    ##     result.vector[result.vector != 0])
    ## allen.proportion <- sort(allen.proportion, decreasing = TRUE)
    ## concurrence.intersection <- allen.relations.intersection(
    ##     allen.proportion, allen.concurrent.relation.set())
    ## allen.proportion.concurs <- sum(allen.proportion[concurrence.intersection])
    ## list(relations = relations, result = allen.result, set = allen.set,
    ##      proportion = allen.proportion, concurrence = allen.proportion.concurs)
    relations
}

#' Summarize the relation of two phases
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases a vector with four column indices representing the start
#' and end of two phases
#' @param app one of 'bcal', 'OxCal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#'
#' @return a list with the following components:
#'
allen_relation_summary <- function(mcmc, phases, app = "chronomodel") {
    chains <- switch(app,
                     chronomodel = ArchaeoPhases::read_chronomodel(mcmc,
                                                                   quiet = "partial"),
                     oxcal = ArchaeoPhases::read_oxcal(mcmc, quiet = "partial"),
                     bcal = ArchaeoPhases::read_bcal(mcmc, quiet = "partial"))
    chains <- chains[,phases]
    names <- allen.check.names(colnames(chains))
    zero.vector <- allen.create.result.vector()
    result.full <- allen.calculate.relations.2(zero.vector, chains)
    result.six <- allen.coerce.six(result.full)
    result.six.proportion <- allen_proportion_results(result.six)
    result.full.proportion <- allen_proportion_results(result.full)
    result.non.zero <- result.full.proportion[result.full.proportion != 0]
    concur.set <- allen.concurrent.relation.set()
    relation.set <- allen.relations.set(result.full)
    non.zero.concurs <- allen.relations.intersection(names(result.non.zero),
                                                     concur.set)
    concurrence_string <- allen.relations.concur(result.full)
    proportion.concurs <- sum(result.full.proportion[non.zero.concurs])
    max_code <- names(result.six[result.six == max(result.six)])
    relation_string <- allen_code_to_string(max_code)
    result_string <- sprintf("%s %s %s",
                             names$first,
                             relation_string,
                             names$second)
    list(result = result_string,
         relation_set = relation.set,
         concurrence = concurrence_string,
         full_result = result.full,
         six_value_result = result.six,
         full_proportion = round(result.full.proportion, digits = 3),
         six_value_proportion = round(result.six.proportion, digits = 3),
         concurrence_proportion = proportion.concurs,
         mcmc_file = mcmc,
         application = app)
}

#' Data for an illustrative graphic
#'
#' Create a dataframe that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators.  Able to illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints (six-value set).
#'
#' @author Thomas S. Dye
#'
#' @param subset One of "none" (default),
#' "concurrent" to highlight concurrent relations,
#' or "six" to highlight relations with distinct endpoints
#'
#' @return A dataframe for input to allen_plot_single()
#'
#' @export
#'
illustrate_allen_relations <- function(subset = "none") {
    result <-   switch(subset,
                       none = allen.create.result.vector(initial.value = 1),
                       concurrent = allen.create.concurrent.vector(),
                       six = allen.create.distinct.endpoint.vector(),
                       stop(sprintf("Unknown subset, '%s'", subset)))
    title_string <- switch(subset,
                           none = "Allen basic relations",
                           concurrent = "Allen concurrent relations",
                           six = "Allen relations with distinct endpoints",
                           stop(sprintf("unknown subset, '%s'", subset)))
    node <- c('precedes', 'meets', 'overlaps', 'finished by', 'starts',
              'contains', 'equals', 'during', 'started by', 'finishes',
              'overlapped by', 'met-by', 'preceded by')
    x <- c(0, 0, 0, -1, 1, -2, 0, 2, -1, 1, 0, 0, 0)
    y <- c(8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 1, 0)
    title <- rep(title_string, length(node))
    df <- cbind.data.frame(x, y, result, node, title)
    df
}
