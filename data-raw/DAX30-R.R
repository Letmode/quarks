## code to prepare `DAX30.R` dataset goes here
DJI <- BatchGetSymbols::BatchGetSymbols("^DJI",
  first.date = "2000-01-01", last.date = "2020-12-31",
  freq.data = "daily"
)$df.tickers
usethis::use_data(DJI, overwrite = TRUE)
