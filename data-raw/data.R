## code to prepare datasets tickers: ^GDAXI, ^GSPC, ^N225, ^FTSE, ^HSI. ^DJI


SP500 <- yfR::yf_get("^GSPC",
  first_date = "2000-01-01",
  last_date = "2023-12-31",
  freq_data = "daily"
)#$df.tickers
usethis::use_data(SP500, overwrite = TRUE)

HSI <- yfR::yf_get("^HSI",
                     first_date = "2000-01-01",
                     last_date = "2023-12-31",
                     freq_data = "daily"
)#$df.tickers
usethis::use_data(HSI, overwrite = TRUE)

DAX <- yfR::yf_get("^GDAXI",
                     first_date = "2000-01-01",
                     last_date = "2023-12-31",
                     freq_data = "daily"
)#$df.tickers
usethis::use_data(DAX, overwrite = TRUE)

NIK225 <- yfR::yf_get("^N225",
                     first_date = "2000-01-01",
                     last_date = "2023-12-31",
                     freq_data = "daily"
)#$df.tickers
usethis::use_data(NIK225, overwrite = TRUE)

DJI <- yfR::yf_get("^DJI",
                     first_date = "2000-01-01",
                     last_date = "2023-12-31",
                     freq_data = "daily"
)#$df.tickers
usethis::use_data(DJI, overwrite = TRUE)

FTSE100 <- yfR::yf_get("^FTSE",
                     first_date = "2000-01-01",
                     last_date = "2023-12-31",
                     freq_data = "daily"
)#$df.tickers
usethis::use_data(FTSE100, overwrite = TRUE)
