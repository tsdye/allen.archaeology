#' Plot a Nökel lattice
#'
#' Plot a single N\\u00F6kel lattice to the display and optionally to a file.
#'
#' @param allen_set a dataframe, such as the one produced by
#' illustrate.allen.relations()
#' @param file_name path to the optional graphics file
#' @param graph_title title of the graph
#' @param pad pad the left and right margins to keep labels from disappearing
#' @param font_size font size for the labels in the plot
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
allen_nokel_lattice <- function(allen_set,
                                file_name = NULL,
                                graph_title = "N\u00F6kel lattice",
                                ## graph_title = "Nökel lattice",
                                pad = 0.2,
                                font_size = 11,
                                height = 7,
                                width = 7) {
  g <- ggraph::ggraph(graph = allen_set, layout = "nicely")
  min_x <- min(allen_set$x)
  max_x <- max(allen_set$x)
  g <- g + xlim(min_x - pad, max_x + pad)
  g <- g + khroma::scale_colour_iridescent()
  g <- g + ggraph::geom_node_label(mapping = aes(label = allen_set$node,
                                                 colour = allen_set$result),
                                   data = allen_set,
                                   size = font_size / .pt)
  g <- g + ggtitle(graph_title)
  g <- g + labs(colour = "Likeli-\nhood")
  if(!is.null(file_name))
      ggsave(filename = file_name,
             plot = g,
             device = grDevices::cairo_pdf,
             height = height,
             width = width)
  g
}

#' Make a plot with panels of N\\u00F6kel lattices.
#'
#' Plot panels of N\\u00F6kel lattices to the display and optionally to a file.
#'
#' @param allen_set a dataframe with plot information, such as the one
#' produced by xxx
#' @param file_name optional path to the graphic file output
#' @param pad pad the margins to keep labels from disappearing
#' @param font_size font size for the labels in the plot
#' @param columns number of columns in the graphic
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
allen_plot_multiple <- function(allen_set,
                                file_name = NULL,
                                pad = 0.2,
                                font_size = 11,
                                columns = 3,
                                height = 7,
                                width = 7) {
    g <- ggraph::ggraph(graph = allen_set, layout = "nicely")
    min_x <- min(allen_set$x)
    max_x <- max(allen_set$x)
    g <- g + xlim(min_x - pad, max_x + pad)
    min_y <- min(allen_set$y)
    max_y <- max(allen_set$y)
    g <- g + ylim(min_y - pad, max_y + pad)
    g <- g + khroma::scale_colour_iridescent()
    g <- g + facet_wrap(vars(title), ncol = columns)
    g <- g + ggraph::geom_node_label(mapping = aes(label = allen_set$node,
                                                   colour = allen_set$result),
                                     data = allen_set,
                                     size = font_size / .pt)
    g <- g + labs(colour = "Likeli-\nhood")
    if(!is.null(file_name))
        ggsave(filename = file_name,
               plot = g,
               device = grDevices::cairo_pdf,
               height = height,
               width = width)
    g
}

#' Make a single plot of a N\\u00F6kel lattice.
#'
#' Plots a N\\u00F6kel lattice to the display and optionally to a file.
#'
#' @param allen_set a dataframe with plot information, such as the one
#' produced by illustrate_allen_relations()
#' @param file_name optional path to the graphic file output
#' @param pad padding in inches to the margins to keep
#' labels from disappearing off the edge of the graphic
#' @param font_size font size for the labels in the plot
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
#' @importFrom ggplot2 .pt aes facet_wrap ggsave ggtitle labs vars xlim ylim
#'
allen_plot_single <- function(allen_set,
                              file_name = NULL,
                              pad = 0.2,
                              font_size = 11,
                              height = 7,
                              width = 7) {
    g <- ggraph::ggraph(graph = allen_set, layout = "nicely")
    min_x <- min(allen_set$x)
    max_x <- max(allen_set$x)
    g <- g + xlim(min_x - pad, max_x + pad)
    min_y <- min(allen_set$y)
    max_y <- max(allen_set$y)
    g <- g + ylim(min_y - pad, max_y + pad)
    g <- g + ggtitle(allen_set$title)
    g <- g + khroma::scale_colour_iridescent()
    g <- g + ggraph::geom_node_label(mapping = aes(label = node,
                                                   colour = result),
                                     data = allen_set,
                                     size = font_size / .pt)
    g <- g + labs(colour = "Likeli-\nhood")
    if(!is.null(file_name))
        ggsave(filename = file_name,
               plot = g,
               device = grDevices::cairo_pdf,
               height = height,
               width = width)
    g
}
