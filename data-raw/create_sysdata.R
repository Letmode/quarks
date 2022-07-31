setwd("~/Arbeit/Packages/quarks/data-raw")
source("smooth.help.R")
ticker_indices <- read.delim("https://gist.githubusercontent.com/Letmode/2188dc76044fed60847b6a0c15be863a/raw/b7ed35aebdea98bb8eca96f8113f542007ab8738/Ticker_Indices.txt",
                             quote = "", fill = FALSE)
ticker_stocks <- read.delim("https://gist.githubusercontent.com/Letmode/2188dc76044fed60847b6a0c15be863a/raw/5c4d0575f30e7744310a2c973608109398b93be2/Ticker_Stocks.txt",
                            quote = "", fill = FALSE)
usethis::use_data(smooth.help, ticker_indices, ticker_stocks, internal = TRUE, overwrite = TRUE)
