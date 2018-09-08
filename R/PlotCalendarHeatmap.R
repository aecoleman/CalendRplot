
PlotCalendarHeatmap <- function(data){

  data$year <- lubridate::year(data$date)

  data$month <- lubridate::month(data$date)

  data$week <- lubridate::floor_date(data$date,
                                     unit = 'week')

  data$wday <- forcats::fct_rev(lubridate::wday(data$date, label = TRUE))

  # Constructing Boundary Paths for Time Intervals ----

  start.date <- as.Date( sprintf('%04d-01-01', min(data$year) ) )

  end.date <- as.Date( sprintf('%04d-01-01', max(data$year) ) )

  out <-
    ggplot(out.fy) +
    geom_tile(aes(x = ORIG_CONTRACT_WEEK,
                  y = WDAY,
                  fill = N,
                  alpha = as.numeric(DAY_STATUS_TYP == 'A'))) +
    geom_tile(aes(x = ORIG_CONTRACT_WEEK,
                  y = WDAY,
                  alpha = as.numeric(DAY_STATUS_TYP == 'Y')),
              fill = 'grey85') +
    geom_tile(aes(x = ORIG_CONTRACT_WEEK,
                  y = WDAY,
                  alpha = as.numeric(DAY_STATUS_TYP == 'X')),
              fill = 'grey85') +
    geom_tile(aes(x = ORIG_CONTRACT_WEEK,
                  y = WDAY,
                  alpha = as.numeric(DAY_STATUS_TYP == 'B')),
              fill = 'grey85') +
    geom_tile(aes(x = ORIG_CONTRACT_WEEK,
                  y = WDAY,
                  alpha = as.numeric(DAY_STATUS_TYP == 'D')),
              fill = 'grey85') +
    geom_path(aes(x = as.Date(X, origin = '1970-01-01'),
                  y = Y,
                  group = group.id),
              data = timeperiodbndry.path,
              col = 'grey25',
              size = 1,
              linejoin = 'mitre',
              lineend = 'butt') +
    scale_fill_viridis_c(name = 'Gross Contracts') +
    scale_x_date(name = 'Time', breaks = 'month', date_labels = '%b') +
    scale_alpha_continuous(guide = 'none', range = c(0, 1)) +
    coord_equal(ratio = 7, expand = FALSE) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(fill = NA, color = 'white'),
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

  dates <-

  df <- data.frame( 'date' = seq.Date( from = min(dates),
                                       to = max(dates),
                                       by = 'day' ),
                    'week' = lubridate::floor_date(dates, unit = 'week'),
                    'wday' = forcats::fct_rev(lubridate::wday(dates,
                                                              label = TRUE)))

  weekdays <- forcats::fct_rev(lubridate::wday(seq.Date(from = Sys.Date(),
                                                        to = Sys.Date() + 6,
                                                        by = 'day'),
                                                        label = TRUE) )

  minWK <- min(df$week)

  maxWD.minWK <- max(df$wday[df$week == minWK])

  maxWK <- max(df$week)

  minWD.maxWK <- min(df$wday[df$week == maxWK])

  # Creates top, right side, and bottom of boundary
  path <- data.frame( 'x' = c( minWK + 7,
                               rep(maxWK, 2),
                               rep(maxWK - 7, 2),
                               minWK ),
                      'y' = c( rep( max(weekdays), 2),
                               rep(minWD.maxWK, 2),
                               rep( min(weekdays), 2) ) )


  if( first == TRUE ){

    # Adds left side of boundary to path if argument 'first' is TRUE
    path <- rbind(
      data.frame( 'x' = c( rep(minWK, 2),
                               minWK + 7 ),
                  'y' = c(min(weekdays),
                          rep(maxWD.minWK, 2) ) ),
      path )

  }

  return( path )

}
