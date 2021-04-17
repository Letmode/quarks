## code to prepare `DAX30.R` dataset goes here
DAX30 <- BatchGetSymbols::BatchGetSymbols("^GDAXI",
  first.date = "2000-01-01", last.date = "2021-03-31",
  freq.data = "daily"
)$df.tickers
usethis::use_data(DAX30, overwrite = TRUE)
