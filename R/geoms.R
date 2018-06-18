#' Timeline plots
#'
#' A stat object that used for data preprocessing in \code{geom_timeline} and
#'   \code{geom_timeline_label}.
#'
#' @import ggplot2
#'
#' @export
#'
StatTimeline <- ggproto("StatTimeline", Stat,
                        required_aes = c("x", "xmin", "xmax"),

                        compute_group = function(data, scales, nmax = nrow(data)){

                          data <- data[data$x >= unique(data$xmin) & data$x <= unique(data$xmax), ]
                          data <- data[order(data$size, decreasing = TRUE), ]

                          if (nmax > nrow(data)) nmax <- nrow(data)
                           data[1:nmax, ]
                        }

)

#' Timeline plots
#'
#' A stat function that pre-processes data for \code{geom_timeline} and
#'  \code{geom_timeline_label}.
#'
#' @import ggplot2
#'
#' @export
#'
stat_timeline <- function(mapping = NULL, data = NULL, geom = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatTimeline,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, nmax = nmax, ...)
  )
}

#'Timeline plots
#'
#' A geom object used with \code{geom_timeline}.
#'
#' @import ggplot2
#'
#' @export
#'
GeomTimeline <- ggproto("GeomTimeline", Geom,
                           required_aes = c("x", "xmin", "xmax"),

                           non_missing_aes = c("size", "shape", "colour"),

                           default_aes = aes(
                            y = 0.5, shape = 19, colour = "black", size = 4, fill = NA,
                            alpha = NA, stroke = 0.5
                          ),


                           draw_key = draw_key_point,

                           draw_group = function(data, panel_scales, coord) {
                             coords <- coord$transform(data, panel_scales)

                             grid::gTree(children = grid::gList(
                             grid::segmentsGrob(
                              x0 = min(coords$x),
                              y0 = min(coords$y),
                              x1 = max(coords$x),
                              y1 = max(coords$y),
                              gp = grid::gpar(col = 'grey', alpha = 0.5, lwd = 3.)),
                             grid::pointsGrob(
                               coords$x,
                               coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(
                                 col = alpha(coords$colour, coords$alpha),
                                 fill = alpha(coords$fill, coords$alpha),
                                 fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                 lwd = coords$stroke * .stroke / 2))
                             ))
                           }
)

#' Timeline plots
#'
#' \code{geom_timeline} is used to visualize earthquake occurence, intensity,
#'   the number of deaths caused, and other parameters.
#'
#' @param xmin a date. The lower bound of the date interval used for subsetting the NOAA data.
#' @param xmax a date. The upper bound of the date interval used for subsetting the NOAA data.
#' @param nmax an integer. Top earthquakes with respect to the magnitude. These earthquakes are
#'   labelled on the plot.
#' @param label a character vector. A column in the NOAA dataset used for labelling the data points.
#'
#' @examples
#' \donttest{
#' data("eq_data")
#' ggplot(data = eq_data %>% filter(COUNTRY == "ITALY"),
#'        aes(x = DATE, size = EQ_PRIMARY, color = TOTAL_DEATHS,
#'            xmin = as.Date('1950-01-01'),
#'            xmax = as.Date('2015-01-01'))) +
#'  geom_timeline() +
#'  theme_eq
#' }
#' \donttest{
#' data("eq_data")
#' ggplot(data = eq_data %>% filter(COUNTRY %in% c('COLOMBIA', 'MEXICO', 'USA')),
#'        aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY,
#'            color = TOTAL_DEATHS, xmin = as.Date('1970-01-01'),
#'            xmax = as.Date('2015-01-01'))) +
#'   geom_timeline_label(aes(label = as.character(DATE)), nmax = 2) +
#'   geom_timeline() +
#'   theme_eq
#' }
#' @import ggplot2
#'
#' @export
#'
geom_timeline <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    stat = StatTimeline,
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

#' Timeline plots (labels)
#'
#' A geom object used with \code{geom_timeline_label}.
#'
#' @import ggplot2
#'
#' @export
#
GeomTimelineLabel <- ggproto("GeomTimeline", Geom,
                                 required_aes = c("x", "xmin", "xmax", "label"),


                                 default_aes = aes(y = 0.5),


                                 draw_key = draw_key_point,

                                 draw_group = function(data, panel_scales, coord) {
                                   coords <- coord$transform(data, panel_scales)

                                   grid::gTree(children = grid::gList(
                                   grid::segmentsGrob(
                                     x0 = coords$x,
                                     y0 = coords$y,
                                     x1 = coords$x,
                                     y1 = coords$y + 0.05,
                                     gp = grid::gpar(col = 'grey', alpha = 0.5, lwd = 2.)),
                                   grid::textGrob(
                                     label = coords$label,
                                     x = coords$x,
                                     y = coords$y + 0.1,
                                     just = 'top',
                                     rot = 45,
                                     gp = grid::gpar(
                                       col = 'black',
                                       fontsize = 14
                                     )
                                    )))
                                   }
)

#' Timeline plots (labels)
#'
#' \code{geom_timeline_label} generates labels for earthquakes on a timeline plot. Used along with \code{geom_timeline}.
#'
#' @param nmax an integer. Top earthquakes with respect to the magnitude. These earthquakes are
#'   labelled on the plot.
#' @param label a character vector. A column in the NOAA dataset used for labelling the data points.
#'
#' @examples
#' \donttest{
#' data("eq_data")
#' ggplot(data = eq_data %>% filter(COUNTRY %in% c('COLOMBIA', 'MEXICO', 'USA')),
#'        aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY,
#'            color = TOTAL_DEATHS, xmin = as.Date('1970-01-01'),
#'            xmax = as.Date('2015-01-01'))) +
#'   geom_timeline_label(aes(label = as.character(DATE)), nmax = 2) +
#'   geom_timeline() +
#'   theme_eq
#' }
#' @import ggplot2
#'
#' @export
#
geom_timeline_label <- function(mapping = NULL, data = NULL,
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatTimeline,
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

#' A custom theme for timeline plots
#'
#' @import ggplot2
#'
#' @export
#'
theme_eq <- theme_bw() + theme(panel.grid = element_blank(),
                               axis.title.y = element_blank(),
                               panel.border = element_blank(),
                               axis.line.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.line.x = element_line(colour = 'black'),
                               legend.position = 'bottom')


