#' Plot a Nökel lattice
#'
#' Plot a single N\\u00F6kel lattice to the display and optionally to a file.
#'
#' @param allen.set a dataframe, such as the one produced by
#' illustrate.allen.relations()
#' @param file.name path to the optional graphics file
#' @param graph.title title of the graph
#' @param pad pad the left and right margins to keep labels from disappearing
#' @param font.size font size for the labels in the plot
#' @param height height in inches of the plot saved to file
#' @param width width in inches of the plot saved to file
#'
#' @return called for its side effects
#'
#' @author Thomas S. Dye
#'
#' @export
#'
#' @importFrom ggplot2 .pt aes facet_wrap ggsave ggtitle labs vars xlim
#'
allen.nokel.lattice <- function(allen.set,
                                file.name = NULL,
                                graph.title = "N\\u00F6kel lattice",
                                pad = 0.2,
                                font.size = 11,
                                height = 7,
                                width = 7) {
  g <- ggraph::ggraph(graph = allen.set, layout = "nicely")
  min.x <- min(allen.set$x)
  max.x <- max(allen.set$x)
  g <- g + xlim(min.x - pad, max.x + pad)
  g <- g + khroma::scale_colour_iridescent()
  g <- g + ggraph::geom_node_label(mapping = aes(label = allen.set$node,
                                                 colour = allen.set$result),
                                   data = allen.set,
                                   size = font.size / .pt)
  g <- g + ggtitle(graph.title)
  g <- g + labs(colour = "Likeli-\nhood")
  if(!is.null(file.name))
    ggsave(filename = file.name, plot = g,
           device = grDevices::cairo_pdf,
           height = height,
           width = width)
  g
}

#' Make a plot with panels of Nökel lattices.
#'
#' Plot panels of Nökel lattices to the display and optionally to a file.
#'
#' @param allen.set a dataframe with plot information, such as the one
#' produced by xxx
#' @param file.name optional path to the graphic file output
#' @param pad pad the left and right margins to keep labels from disappearing
#' @param font.size font size for the labels in the plot
#' @param height height in inches of the graphic file output
#' @param width width in inches of the graphic file output
#'
#' @return called for its side effects
#'
#' @author Thomas S. Dye
#'
#' @export
#'
#' @importFrom graphics title
#' @importFrom ggplot2 .pt aes facet_wrap ggsave ggtitle labs vars xlim
#'
allen.ggplot2.graphic.full <- function(allen.set,
                                       file.name = NULL,
                                       pad = 0.2,
                                       font.size = 11,
                                       height = 7,
                                       width = 7) {
  g <- ggraph::ggraph(graph = allen.set, layout = "nicely")
  min.x <- min(allen.set$x)
  max.x <- max(allen.set$x)
  g <- g + xlim(min.x - pad, max.x + pad)
  g <- g + khroma::scale_colour_iridescent()
  g <- g + facet_wrap(vars(title))
  g <- g + ggraph::geom_node_label(mapping = aes(label = allen.set$node,
                                                 colour = allen.set$result),
                                   data = allen.set,
                                   size = font.size / .pt)
  g <- g + labs(colour = "Likeli-\nhood")
  if(!is.null(file.name))
      ggsave(filename = file.name,
             plot = g,
             device = grDevices::cairo_pdf,
             height = height,
             width = width)
  g
}
