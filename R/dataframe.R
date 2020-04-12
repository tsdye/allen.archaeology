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
#'
#' @importFrom ArchaeoPhases read_bcal read_oxcal read_chronomodel
#'
#' @export
#'
allen_relate_phases <- function(mcmc,
                                phases_1,
                                phases_2,
                                app = "bcal") {
    just_odd <- function(x) x[ x %% 2 == 1 ]
    relations <- NULL
    chains.df <- switch(app,
                        chronomodel = ArchaeoPhases::read_chronomodel(mcmc,
                                                                      quiet = "partial"),
                        oxcal = ArchaeoPhases::read_oxcal(mcmc, quiet = "partial"),
                        bcal = ArchaeoPhases::read_bcal(mcmc, quiet ="partial"),
                        stop(sprintf("Unknown application, '%s'", app)))
    ## chains.df <- as.data.frame(chains)
    term_1 <- just_odd(phases_1)
    term_2 <- just_odd(phases_2)
    for(ind_2 in term_2) {
        positions_2 <- c(ind_2, ind_2 + 1)
        for(ind_1 in term_1) {
            positions_1 <- c(ind_1, ind_1 + 1, positions_2)
            chains <- chains.df[,positions_1]
            names <- allen.check.names(colnames(chains))
            zero.vector <- allen.create.result.vector()
            result.vector <- allen.calculate.relations.2(zero.vector, chains)
            ## result.six <- allen.coerce.six(result.vector)
            result <- allen_proportion_results(result.vector, sort = FALSE)
            node <-  allen_basic_relation_strings()
            max_code <- names(result[result == max(result)])
            ## relation_string <- allen_code_to_string(max_code)
            relation_string <- node[max_code]
            x <- allen_lattice_x()
            y <- allen_lattice_y()
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


#' Data for an illustrative graphic:
#'
#'
#' Create a dataframe that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators: illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints (six-value set).  Also, useful for describing the
#' chronological domains of stratification, anagenesis, and cladogenesis.
#'
#' @author Thomas S. Dye
#'
#' @param relations One of:
#' \describe{
#' \item{basic}{show the 13 basic Allen relations (default);}
#' \item{concurrent}{show concurrent relations;}
#' \item{six}{show relations with distinct endpoints;}
#' \item{inherit}{show chronological information about a transmission
#'     relationship from the receiver's point of view;}
#' \item{contribute}{show chronological information from the giver's point of view during
#'     transmission;}
#' \item{stratigraphic}{show the basic stratigraphic relations established by an
#'     observation of superposition;}
#' \item{anagenetic}{show the basic relations of artifact
#'     change without branching;}
#' \item{cladogenetic}{show the basic relations of artifact change with branching;}
#' \item{contributors}{show the composite relations of two contributors to a reticulation
#'     occurrence;}
#' \item{innovations}{show the composite relations of two entities produced by
#'     innovation from a single source;}
#' \item{sequence}{show the composite relations of superposition
#'     in a stratigraphic sequence; or}
#' \item{anagenesis}{show the composite relations of transmission during
#'     anagenesis.}}
#'
#' @return A dataframe for input to allen_plot_single()
#'
#' @references
#'
#' Harris, E. \emph{Principles of Archaeological Stratigraphy}. Second edition.
#' London: Academic Press.
#'
#' Lyman, R. Lee and Michael J. O'Brien.  Seriation and cladistics: The
#' difference between anagenetic and cladogenetic evolution.  Chapter 5 in
#' \emph{Mapping Our Ancestors: Phylogenetic Approaches in Anthropology and
#' Prehistory.} New Brunswick: AldineTransaction.
#'
#' @export
#'

illustrate_allen_relations <- function(relations = "basic") {
    result <-   switch(relations,
                       basic = allen.create.result.vector(initial.value = 1),
                       concurrent = allen.create.concurrent.vector(),
                       six = allen.create.distinct.endpoint.vector(),
                       inherit = allen.string.to.vector("oFD"),
                       contribute = allen.string.to.vector("moFD"),
                       stratigraphic = allen.string.to.vector("mM"),
                       anagenetic = allen.string.to.vector("mM"),
                       cladogenetic = allen.string.to.vector("OfdoFD"),
                       contributors = allen.set.to.vector(
                           allen.composition(allen.string.to.set("moFD"),
                                             allen.string.to.set("MOfd"))),
                       innovations = allen.set.to.vector(
                           allen.composition(allen.string.to.set("Ofd"),
                                             allen.string.to.set("oFD"))),
                       sequence = allen.set.to.vector(
                           allen.composition(allen.string.to.set("m"),
                                             allen.string.to.set("m"))),
                       anagenesis = allen.set.to.vector(
                           allen.composition(allen.string.to.set("m"),
                                             allen.string.to.set("m"))),
                       incorporation.2 = allen.set.to.vector(
                           allen.composition(allen.string.to.set("m"),
                                             allen.string.to.set("Ofd"))),
                       incorporation.1 = allen.set.to.vector(
                           allen.composition(allen.string.to.set("m"),
                                             allen.string.to.set("M"))),
                       stop(sprintf("Unknown relation, '%s'", relations)))
    title_string <- switch(relations,
                           basic = "Basic Allen relations",
                           concurrent = "Basic concurrent relations",
                           six = "Basic Allen relations with distinct endpoints",
                           inherit = "Basic inheritance relations",
                           contribute = "Basic contribution relations",
                           stratigraphic = "Basic stratigraphic relations",
                           anagenetic = "Basic anagenetic relations",
                           cladogenetic = "Basic cladogenetic relations",
                           contributors = "Composite contributor relation",
                           innovations = "Composite innovation relation",
                           sequence = "Composite stratigraphic relation",
                           anagenesis = "Composite anagenetic relation",
                           incorporation.2 = "Composite contributor relation with two successors",
                           incorporation.1 = "Composite contributor relation with one successor",
                           stop(sprintf("unknown relation, '%s'", relations)))
    node <- allen_basic_relation_strings()
    x <- allen_lattice_x()
    y <- allen_lattice_y()
    title <- rep(title_string, length(node))
    df <- cbind.data.frame(x, y, result, node, title)
    df
}
#' Calculate the composite relation of two phases
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases a vector with six column indices representing the start
#' and end chains of three phases
#' @param title a plot title
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#' @param quiet One of "no" to allow messages and warnings,
#' "partial" (default) to suppress messages and allow warnings, or "yes"
#' to suppress messages and warnings.
#'
#' @return A dataframe for input to allen_plot_single()
#'
#' @importFrom ArchaeoPhases read_bcal read_oxcal read_chronomodel
#'
#' @export
#'
allen_composite_relation <- function(mcmc,
                                     phases,
                                     title = c("first", "second"),
                                     app = "bcal",
                                     quiet = "partial") {
    if(length(phases) != 6) stop("Chains for three phases are required.")
    chains <- switch(app,
                     chronomodel = read_chronomodel(mcmc,
                                                    quiet = quiet),
                     oxcal = read_oxcal(mcmc, quiet = quiet),
                     bcal = read_bcal(mcmc, quiet = quiet),
                     stop(sprintf("Unknown application, '%s'", app)))
    chains <- chains[,phases]
    names <- colnames(chains)
    zero.vector <- allen.create.result.vector()
    result.vector.1 <- allen.calculate.relations.2(zero.vector, chains[,1:4])
    result.vector.2 <- allen.calculate.relations.2(zero.vector, chains[,3:6])
    set.vector.1 <- allen.set.vector.from.result(result.vector.1)
    set.vector.2 <- allen.set.vector.from.result(result.vector.2)
    res <- allen.composition(set.vector.1, set.vector.2)
    title_res <- paste(res, collapse = "")
    title_res <- allen.order.string(title_res)
    graph_title <- sprintf("Allen relation: %s(%s)%s",
                           title[1],
                           title_res,
                           title[2])
    result <- allen.set.to.vector(res)
    node <- allen_basic_relation_strings()
    x <- allen_lattice_x()
    y <- allen_lattice_y()
    title <- rep(graph_title, length(node))
    df <- cbind.data.frame(x, y, result, node, title)
    df
}
