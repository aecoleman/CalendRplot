library(data.table)
library(lubridate)

raw <-
  fread('C:/Users/andre/Documents/R/R Scripts/git/MaiAccount/r_2015Expenses.txt')

raw[, `:=`(START_EXPENSE_DATE = as.Date(START_EXPENSE_DATE, format = '%m/%d/%Y'),
              END_EXPENSE_DATE = as.Date(END_EXPENSE_DATE, format = '%m/%d/%Y') )]

raw[, I := .I]


preformat <-
  raw[, .( value = AMOUNT / (1L + as.integer(difftime(END_EXPENSE_DATE, START_EXPENSE_DATE, units = 'day'))),
             date = seq.Date(from = START_EXPENSE_DATE, to = END_EXPENSE_DATE, by = 'day') ),
       by = .(I)]

formatted <- preformat[, .(value = sum(value), n = .N), keyby = .(date)]

data <- formatted[date >= as.Date('2015-01-01') & date < as.Date('2016-01-01')]
