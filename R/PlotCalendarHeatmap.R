#' MinMaxNorm
#'
#' Internal function for scaling user provided size values to be between 0 and 1 (inclusive).
#'
#' @param vector numeric, vector of values to be scaled
#'
#' @return numeric vector equal in length to the vector provided, with all
#' values scaled to be between 0 and 1 (inclusive).
#'
.MinMaxNorm <- function(vector){

  (vector - min(vector)) / (max(vector) - min(vector))

}

#' Get Path
#'
#' Finds the x and y values needed to construct a path to separate adjacent
#' months in the plot.
#'
#' @param plot.data data.table, containing the data that will be used to
#' construct the calendar plot
#'
#' @return data.table containing the path between the different months
#'
.GetPath <- function(plot.data){

  path <- plot.data[,
                    .(min.wday = min(wday.num)),
                    keyby = .(year.fctr, year.num, month.num, week.yr.num)
                    ][,
                      max.week := max(week.yr.num),
                      by = .(year.fctr, year.num, month.num)
                      ][week.yr.num == max.week,
                        .(year.fctr, year.num, month.num, min.wday, week.yr.num)
                        ][,
                          .(x = c(rep(week.yr.num, 2), rep(week.yr.num - 1, 2 ) ),
                            y = c(0L, rep(min.wday, 2), -7L) ),
                          by = .(year.fctr, year.num, month.num)
                          ][,
                            max.month := max(month.num),
                            by = .(year.fctr, year.num)
                            ][month.num != max.month,
                              .(year.fctr, year.num, month.num, x, y)]

  path[, keep := TRUE]

  path[y == -7,
       keep := x == max(x),
       by = .(year.fctr, year.num, month.num)]

  path[keep == TRUE, .(year.fctr, year.num, month.num, x, y, year.mon = 100 * year.num + month.num)]

}

#' Plot Calendar Heatmap
#'
#' @param data data.frame, containing the data to be used to create the plot
#' @param date.column character, the name of the column in `data` that contains the dates
#' @param fill character, the name of the column in `data` that will be used to determing the colors in the heatmap
#' @param bg.col character, the background color for the plot
#' @param size character or numeric, either the column to be used to determine the size of the squares, or a number that defines the size to use for all squares. Optional, and at present not recommended.
#' @param missing.is.zero logical, should dates for which no data is presented be assumed to be zero?
#' @param day.border.col character, the color to be used for the boundaries between days
#' @param month.border.col character, the color to be used for the boundaries between months
#'
#' @return ggplot object containing a calendar heatmap
#' @export
#'
#' @import data.table
#'
PlotCalendarHeatmap <- function(data, date.column, fill, bg.col, size, missing.is.zero = TRUE, day.border.col, month.border.col){

  if(missing(month.border.col)) month.border.col <- 'white'

  if(missing(day.border.col)) day.border.col <- 'black'

  if(missing(bg.col)) bg.col <- 'white'

  data <- as.data.frame(data)

  month.breaks <-
    data.table(breaks = c(  2.5,   7.0,  11.0,  15.5,  20.0,  24.0,
                            28.5,  33.0,  37.5,  41.5,  46.0,  50.5),
               labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec') )

  wday.breaks <- data.table(breaks = c( -1.5,  -3.5,  -5.5),
                            labels = c('Mon', 'Wed', 'Fri') )

  year.ends <-
    list('start' = lubridate::floor_date(min(data[,date.column]), unit = 'year'),
         'end' = lubridate::ceiling_date(max(data[,date.column]), unit = 'year') - 1L )

  plot.data <-
    data.table(
      date = seq(from = year.ends$start,
                 to = year.ends$end,
                 by = 'day') )

  plot.data[,  wday.num := -lubridate::wday(date) ]
  plot.data[, month.num := lubridate::month(date) ]
  plot.data[,  year.num := lubridate::year(date) ]

  plot.data[, year.fctr := factor(year.num, levels = seq(from = max(year.num),
                                                         to = min(year.num)) )]

  plot.data[,  week.yr.num := data.table::frank(
    lubridate::floor_date(date,
                          unit = 'week'),
    ties.method = 'dense'),
    by = .(year.num) ]

  plot.data <-
    merge(plot.data,
          data,
          by.x = 'date',
          by.y = date.column,
          all.x = TRUE)

  if( missing.is.zero == TRUE ){

    i.na <- plot.data[is.na(get(fill)), which = TRUE]
    set(plot.data, i = i.na, j = fill, value = 0)
    rm(i.na)

  }

  if( missing(size) ){

    size <- 1

  } else if(is.character(size)){

    temp.var <- .MinMaxNorm(plot.data[, ..size])

    plot.data[, size := sqrt(temp.var) ]

  } else if( is.numeric(size) ){

    plot.data[, size := ..size ]

  }

  month.border.path <- .GetPath(plot.data)

  g <- ggplot(plot.data)

  g <- g +
    geom_tile(aes(x = week.yr.num - 0.5,
                  y = wday.num + 0.5,
                  width = size,
                  height = size,
                  fill = get(fill)),
              col = day.border.col,
              size = 0.5)

#   if( !missing(day.border.col) ){
#
#     g <- g +
#       geom_segment(
#         aes(x = x,
#             xend = x,
#             y = y,
#             yend = yend),
#         data = plot.data[,.(y = min(wday.num) - 1,
#                             yend = max(wday.num)),
#                          by = .(x = week.yr.num, year.fctr)],
#                  col = day.border.col,
#                  size = 0.5) +
#       geom_segment(
#         aes(   y = y,
#                yend = y,
#                x = x,
#                xend = xend),
#         data = plot.data[,.(x = min(week.yr.num) - 1,
#                             xend = max(week.yr.num) ),
#                          by = .(y = wday.num, year.fctr)],
#         col = day.border.col,
#         size = 0.5)
#
#   }

  g <- g +
    geom_path(data = month.border.path,
              aes(x = x,
                  y = y,
                  group = month.num),
              col = month.border.col,
              size = 1)


  g <- g +
    coord_equal(ratio = 1,
                xlim = c(plot.data[,min(week.yr.num)] - 1.1, plot.data[,max(week.yr.num)] + 0.1),
                ylim = c(plot.data[,min(wday.num)] - 0.1, plot.data[,max(wday.num)] + 1.1),
                expand = FALSE) +
    facet_grid(year.fctr ~ .,
               switch = 'y') +
    scale_y_continuous(name = NULL,
                       breaks = wday.breaks$breaks,
                       labels = wday.breaks$labels) +
    scale_x_continuous(name = NULL,
                       breaks = month.breaks$breaks,
                       labels = month.breaks$labels) +
    scale_fill_viridis_c(
      limits = c(0, plot.data[,max(get(fill))]*1.05),
      option = 'magma',
        name = fill,
       guide = guide_colorbar(title = fill,
                               nbin = 80,
                          barheight = grid::unit(    x = 0.25,
                                                 units = 'npc'),
                              draw.ulim = TRUE,
                              draw.llim = TRUE,
                              frame.colour = 'grey50',
                              ticks.colour = 'grey50')) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = bg.col),
          panel.border = element_rect(fill = NA, color = bg.col),
          panel.grid = element_line(color = NA),
          legend.position = 'right',
          legend.direction = 'vertical',
          strip.placement = 'outside',
          text = element_text(color = 'black'),
          axis.text = element_text(color = 'black'))

  g

}
