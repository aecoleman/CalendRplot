
PlotCalendarHeatmap <- function(data){

  data$year <- lubridate::year(data$date)

  data$month <- lubridate::month(data$date)

  data$week <- lubridate::floor_date(data$date,
                                     unit = 'week')

  data$wday <- forcats::fct_rev(lubridate::wday(data$date, label = TRUE))

  # Constructing Boundary Paths for Time Intervals ----

  start.date <- as.Date( sprintf('%04d-01-01', min(data$year) ) )

  end.date <- as.Date( sprintf('%04d-01-01', max(data$year) ) )

  timeperiodbndry <-
    out.fy[, .(X_B = as.numeric(min(ORIG_CONTRACT_WEEK)) - 3.5,
               X_E = as.numeric(max(ORIG_CONTRACT_WEEK)) + 3.5 ),
           keyby = .(RSM_FY, RSM_QTR, RSM_MONTH, WDAY)
           ][, group.id := sprintf('Y%04.0fQ%01.0fM%02.0f', RSM_FY, RSM_QTR, RSM_MONTH)]

  timeperiodbndry.path <-
    rbindlist(
      list(timeperiodbndry[,.(Y = max(as.numeric(WDAY)) + 0.5,
                              type = 'B'),
                           by = .(group.id, RSM_FY, RSM_QTR, RSM_MONTH, X = X_B)],
           timeperiodbndry[,.(Y = min(as.numeric(WDAY)) - 0.5,
                              type = 'B'),
                           by = .(group.id, RSM_FY, RSM_QTR, RSM_MONTH, X = X_B)],
           timeperiodbndry[,.(Y = max(as.numeric(WDAY)) + 0.5,
                              type = 'E'),
                           by = .(group.id, RSM_FY, RSM_QTR, RSM_MONTH, X = X_E)],
           timeperiodbndry[,.(Y = min(as.numeric(WDAY)) - 0.5,
                              type = 'E'),
                           by = .(group.id, RSM_FY, RSM_QTR, RSM_MONTH, X = X_E)]) )

  timeperiodbndry.path[type == 'B',
                       Rank := frankv(.SD, cols = c('X','Y'), order = 1),
                       by = .(group.id),
                       .SDcols = c('X', 'Y')]

  maxRank <- timeperiodbndry.path[, max(Rank, na.rm = TRUE)]

  timeperiodbndry.path[type == 'E',
                       Rank := maxRank + frankv(.SD, cols = c('X','Y'), order = -1),
                       by = .(group.id),
                       .SDcols = c('X', 'Y')]

  maxRank <- timeperiodbndry.path[, max(Rank, na.rm = TRUE)]

  setkeyv(timeperiodbndry.path, cols = c('group.id', 'type', 'Rank'))

  timeperiodbndry.path <-
    rbindlist(
      list(timeperiodbndry.path,
           timeperiodbndry.path[Rank == 1,
                                .(group.id, RSM_FY, RSM_QTR, RSM_MONTH,
                                  X, Y, type, Rank = maxRank + 1)] ) )

  setkeyv(timeperiodbndry.path, cols = c('group.id', 'Rank'))

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


.GetBoundaries <- function(dates){

  df <- data.frame( 'date' = dates,
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

  path <- data.frame( 'x' = c( rep(minWK, 2),
                               rep(minWK + 7, 2),
                               rep(maxWK, 2),
                               rep(maxWK - 7, 2),
                               minWK ),
                      'y' = c(min(weekdays),
                              rep(maxWD.minWK, 2),
                              rep( max(weekdays), 2),
                              rep(minWD.maxWK, 2),
                              rep( min(weekdays), 2) ) )

  return( path )

}
