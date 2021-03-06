#' Illustrate basic and composite relations
#'
#' Illustrate basic and composite Allen relations for several chronological
#' model domains with a Nokel lattice. Chronological model domains include
#' stratigraphy, anagenetic artifact change, and a variety of cladogenetic
#' processes of artifact change, including what archaeologists label tradition,
#' innovation, and borrowing. The illustrations show that stratigraphy and
#' anagenetic artifact change each offer the necessary information to structure
#' a chronological model deductively. In contrast, cladogenetic processes of
#' artifact change comprise relatively weak sources of information that offer
#' little guidance for the chronological model builder.
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
#' \item{anagenetic}{show the basic relations of artifact change without
#'     branching;}
#' \item{cladogenetic}{show the basic relations of artifact change with branching;}
#' \item{contributors}{show the composite relations of two donors to a borrowing
#'     occurrence;}
#' \item{innovations}{show the composite relations of two entities produced by
#'     innovation from a single source;}
#' \item{sequence}{show the composite relations of superposition
#'     in a stratigraphic sequence;}
#' \item{anagenesis}{show the composite relations of transmission during
#'     anagenesis;}
#' \item{incorporation.2}{show the composite relations of incorporation with
#'     two successors; or}
#' \item{incorporation.1}{show the composite relations of incorporation with
#'     one successor.}
#' }
#'
#'
#' @param ... Named arguments to be passed on to \code{allen_plot()}.
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
#' @author Thomas S. Dye
#'
#' @return A layout_tbl_graph object.
#'
#' @examples
#'
#' # Plot to the R graphics device
#' # illustrate()
#'
#' # Save layout_tbl_graph object to a variable
#' # then plot to the R graphics device
#'
#' # foo <- illustrate()
#' # foo
#'
#' @export

illustrate <- function(relations = "basic", ...) {
    allen_plot(illustrate_allen_relations(relations), ...)
}
