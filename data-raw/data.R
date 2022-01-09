## code to prepare datasets tickers: ^GDAXI, ^GSPC, ^N225, ^FTSE, ^HSI. ^DJI
HSI <- BatchGetSymbols::BatchGetSymbols("^DJI",
  first.date = "2000-01-01", last.date = "2021-12-31",
  freq.data = "daily"
)$df.tickers
usethis::use_data(HSI, overwrite = TRUE)
