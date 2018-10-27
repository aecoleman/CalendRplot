#' PlotCalendarHeatmap
#'
#' @param data data.frame, which must have a column named \code{date} containing
#' dates, and at least one other column.
#'
#' @param bg.col character, the background color, which is used for the panel
#' background, panel border, and the lines that divide the months.
#'
#' @return
#' @export
#'
#' @examples
PlotCalendarHeatmap <- function(data, bg.col){

  if( missing(bg.col) ){
    bg.col <- 'white'
  }

  #TODO: Add ability to specify which columns to use for aesthetics:
  # - Fill
  # - Color (Maybe?)
  # - Size
  # - Alpha

  #TODO: Add option for user to specify that they have included columns meant
  # for grouping and/or faceting

  # TODO: Add considerations for if the user has included their own groupings

  # Get the first day of the first month for which data is supplied
  start.date <-
    as.Date( lubridate::floor_date( min(data$date), unit = 'month' ) )

  # Get the last day of the last month for which data is supplied, by finding
  # the first day of the first month after the provided dates and then
  # subtracting one day
  end.date <-
    as.Date( lubridate::ceiling_date( max(data$date), unit = 'month' ) - 1)

  # Make data.frame with all days, to be joined with user supplied data
  plot.data <- data.frame( 'date' = seq.Date( from = start.date,
                                              to = end.date,
                                              by = 'day' ) )

  # Merge list of all days with the user supplied data
  plot.data <- merge( plot.data,
                      data,
                      by = 'date',
                      all = TRUE)

  # Create year column, for grouping
  plot.data$year <- lubridate::year(plot.data$date)

  # Create month column, for grouping
  plot.data$month <- lubridate::month(plot.data$date)

  # Create week column, determines which column the block representing a
  # particular date will be placed in
  plot.data$week <- lubridate::floor_date(plot.data$date,
                                     unit = 'week')

  # Create wday column, determines which row the block representing a particular
  # date will be placed in. We need to reverse the factor order so that time
  # flows from top to bottom and from left to right. Without reversing the
  # factors, we would have time flowing from bottom to top and left to right.
  plot.data$wday <- forcats::fct_rev(lubridate::wday(plot.data$date, label = TRUE))

  # Get the facets/groups into which the data is to be split
  facet.groups <- unique(plot.data[,c('year','month')])

  # For each group, call the .GetBoundaries function to determine the boundaries
  # for that group, then turn the resulting list of data.frames into a single
  # data.frame which can be passed to ggplot
  paths <-
    do.call('rbind',
            apply(facet.groups,
                     MARGIN = 1,
                     FUN = function(x){
                       data.frame('group' = paste0(x, collapse = '_'),
                      .GetBoundaries(
                        plot.data[plot.data$year == x['year']
                                  & plot.data$month == x['month'], ]$date),
                      stringsAsFactors = FALSE) } ) )

  # Initialize ggplot object
  out <- ggplot(data = plot.data)

  # Draw blocks for each day
  out <- out +
    geom_tile(mapping = aes(x = week,
                            y = wday,
                         fill = value))

  # Draw dividing lines between months
  out <- out +
    geom_path(aes(x = x,
                  y = y,
              group = group),
              data = paths,
              col = bg.col,
              size = 1,
              linejoin = 'mitre',
              lineend = 'butt')

  # Set scales for fill, x, and y
  out <- out +
    scale_fill_viridis_c(option = 'magma') +
    scale_x_date(  name = '',
                 breaks = 'month',
            date_labels = '%b') +
    scale_y_discrete(name = '',
                   breaks = c('Mon', 'Wed', 'Fri'))

  # Set coordinate ratio, ratio of 7 creates square blocks
  out <- out +
    coord_equal(ratio = 7, expand = FALSE)

  # Set theme
  out <- out +
    theme_minimal() +
    theme(panel.background = element_rect(fill = bg.col),
          panel.border = element_rect(fill = NA, color = bg.col),
          panel.grid = element_line(color = NA),
          legend.position = 'bottom',
          legend.direction = 'horizontal')

  return(out)

}


#' GetBoundaries
#'
#' This function takes a set of dates and returns the set of coordinates that
#' can be used to draw a polygon around the specified set of dates when arranged
#' in a grid as in the \code{CalendarHeatmap} function.
#'
#' @param dates Date, the dates that belong to the interval. Can also pass just
#' the first and last date.
#' @param first logical, is this the first of a set of intervals? When TRUE, the
#' function will return a path that encloses the dates. When \code{FALSE}, the function
#' will return a path that encloses the dates on 3 sides only. This is useful
#' because it prevents overplotting when one interval's right boundary is
#' another interval's left boundary.
#'
#' @return data.frame, containing x and y values to be used in a call to the
#' \code{ggplot::geom_path} function
#' @export
#'
.GetBoundaries <- function(dates, first = FALSE){

  df <- data.frame( 'date' = seq.Date( from = min(dates),
                                       to = max(dates),
                                       by = 'day' ) )

  df$week <- lubridate::floor_date(df$date, unit = 'week')

  df$wday <- forcats::fct_rev(lubridate::wday(dates, label = TRUE))

  weekdays <- forcats::fct_rev(lubridate::wday(seq.Date(from = Sys.Date(),
                                                        to = Sys.Date() + 6,
                                                        by = 'day'),
                                                        label = TRUE) )

  minWK <- min(df$week)

  maxWD.minWK <- as.numeric(max(df$wday[df$week == minWK])) + 0.5

  maxWK <- max(df$week)

  minWD.maxWK <- as.numeric(min(df$wday[df$week == maxWK])) - 0.5

  minX <- minWK - 3.5
  maxX <- maxWK + 3.5

  minY <- as.numeric(min(weekdays)) - 0.5
  maxY <- as.numeric(max(weekdays)) + 0.5

  # Creates top, right side, and bottom of boundary
  path <- data.frame( 'x' = c( minX,
                               rep(maxX, 2),
                               rep(maxX - 7, 2),
                               minX ),
                      'y' = c( rep( maxY, 2),
                               rep(minWD.maxWK, 2),
                               rep( minY, 2) ) )


  if( first == TRUE ){

    # Adds left side of boundary to path if argument 'first' is TRUE
    path <- rbind(
      data.frame( 'x' = c( rep(minX, 2),
                               minX + 7 ),
                  'y' = c( minY,
                           rep(maxWD.minWK, 2) ) ),
      path )

  }

  return( path )

}
