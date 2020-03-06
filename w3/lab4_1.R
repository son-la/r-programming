library(quantmod)

start <- "2013-01-01"
end <- "2020-03-06"

# Download F, MSFT, CCF data from Yahoo Finance

ford <- getSymbols("F", from=start, to=end, env=NULL)
ms <- getSymbols("MSFT", from=start, to=end, env=NULL)
ccf <- getSymbols("CCF", from=start, to=end, env=NULL)

# Download daily FF3 from French data library
download.file(url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_factors_daily_TXT.zip", destfile="tmp.zip" )
unziped_file <- unz("tmp.zip", filename="F-F_Research_Data_Factors_daily.txt")
ffdf <- readLines(unziped_file)
unlink(unziped_file)

subset <- ffdf[8:length(ffdf)-2]

data_set <- lapply(subset, FUN=function(x) {return(as.numeric(unlist(strsplit(x, "\\s+"))))})
data_set <- t(sapply(data_set, FUN=function(x){return(x)}))
colnames(data_set) <- c("date", "mmrf", "smb", "hml", "rf")
data_set <- data.frame(data_set)
data_set$date <- as.Date(as.character(data_set$date), format="%Y%m%d")


# Compute stock return
ford_ret <- 100*periodReturn(ford, period="daily")
ford_df <- data.frame(date=index(ford_ret), ford=as.vector(ford_ret$daily.returns))
data_set <- merge(data_set, ford_df)

ms_ret <- 100*periodReturn(ms, period="daily")
ms_df <- data.frame(date=index(ms_ret), ms=as.vector(ms_ret$daily.returns))
data_set <- merge(data_set, ms_df)

ccf_ret <- 100*periodReturn(ccf, period="daily")
ccf_df <- data.frame(date=index(ccf_ret), ccf=as.vector(ccf_ret$daily.returns))
data_set <- merge(data_set, ccf_df)

# Statistics
round(apply(data_set[, -1], MARGIN = 2, FUN=summary), digits=3)
round(apply(data_set[, -1], MARGIN = 2, FUN=sd), digits=3)

round(cor(data_set[, -1]), digits=3)

# Estiate FF3 model for each stock
summary(lm(ford ~ mmrf + smb + hml, data=data_set))
summary(lm(ms ~ mmrf + smb + hml, data=data_set))
summary(lm(ccf ~ mmrf + smb + hml, data=data_set))