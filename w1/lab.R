####################################################
## R programming                                   #
## Lab 1.1: Examining stock returns                #
####################################################
## update packages, if using Rstudio, you can use menu -> Tools -> Update packages
## otherwise use the following command
## update.packages(repos = "https://cran.uib.no", ask = FALSE) # update package
##
## for the purpose, install quantmod package, again with Rstudio
## installation can be executed via Tools menu
## install.packages("quantmod", repos = "https://cran.uib.no")
library(quantmod) # make quantmod available
library(help = quantmod) # some info about quantmod
help(getSymbols) #  help info about getSymbols
## get SP500 from Yahoo website
sp <- getSymbols("^GSPC", from = "2000-01-01", to = "2018-12-31", env = NULL)
head(sp) # first few lines
str(sp) # check out the structure of the sp object
## thus sp is an xts object, see help(xts) for some description
help(xts)
chartSeries(x = sp, log.scale = TRUE, theme = chartTheme("white")) # basic chart
addROC() # add return series graph
help(chartSeries)
##
## return satatistics, etc
##
## 1. average sample period daily, weekly, monthly, quarterly, and annual returns
## 2. volatility (annualized)
## 3. produce historgram of the daily log returns and impose normal curve on top of it 
##    using sample mean and standard deviation in the normal curve parameters
## 4. average Monday, Tuesday, ..., Friday retuns and standard deviations
## 5. average daily returns by year and by months (average within month return by year)
## 6. return standard deviations by year and by months (within month volatility by year)
## 7. average Jan, Feb, ..., Dec returns and standard deviations
## 8. plot these returns and standard deviations over months
## 9. generate box-plots over months Jan, ..., Dec
##
##
p <- sp$GSPC.Adjusted # extract price index (the type is again an xts object)
str(p) # structure of p 
head(p) # first six values
help(periodReturn)
##
## 1. average returns
##
round(100*mean(dailyReturn(sp)), digits = 3) # sample period mean daily returns
round(100*mean(weeklyReturn(sp)), digits = 3) # sample period mean weekly returns
round(100*mean(monthlyReturn(sp)), digits = 3) # sample period mean monthly returns
round(100*mean(quarterlyReturn(sp)), digits = 3) # sample period mean monthly returns
round(100*mean(annualReturn(sp)), digits = 3) # sample period mean monthly returns
##
## 2. volatiltiy (annualized), volatility is typically based on log-returns
##
round(sqrt(252) * 100 * sd(dailyReturn(sp, type = "log")), digits = 1)
##
## 3. daily log return histogram and normal curve
##
help(hist) # info about hist() function
hist(100 * dailyReturn(sp, type = "log"), breaks = "Scott", xlab = "Return (%)",
     main = "Histogram of S&P 500 Daily Reurns", col = "light blue", prob = TRUE)
(mu <- 100 * mean(dailyReturn(sp, type = "log"))) # mean logreturn
(sigma = 100 * sd(dailyReturn(sp, type = "log")))
curve(expr = dnorm(x, mean = mu, sd = sigma), from = -6, to = 6, col = "red", lwd = 2, add = TRUE)
##
## 4. average day-of-week returns and standard deviations (volatility)
##
head(weekdays(index(sp)))
## utilizing tapply() with weekdays() results to alphapet order of days
## (see help(tapply) help(weekdays)
round(100*tapply(X = dailyReturn(sp), INDEX = weekdays(index(sp)), mean), digits = 3) # not good
## utilizing POSIXlt and POSIXlt classes prouced by R functions as.POSIXct() and as.POSIXlt(),
## see e.g. help(as.POSIXlt)
## you can think POSIXct with suffix 'ct' as 'continuous' time
## (represented in seconds starting form Jan 1, 1970)
as.double(as.POSIXct(Sys.time())) # seconds from Jan 1, 1970
## POSIXlt with suffix 'lt'  can be thought as list time (various categorical descriptions of the time,
## including day of the week, month, etc).
## help(as.POSIXlt)
unlist(as.POSIXlt(Sys.time())) # time categorial description of the POSIXlt object
as.POSIXlt(Sys.time())$wday # day of week (0 = Sunday)
as.POSIXlt(Sys.time())$year
## help(strptime) # see also help for strptime() function with is a wrapper of as.POSIX functions
## average day-of-week returns
round(100*tapply(X = dailyReturn(sp), INDEX = as.POSIXlt(index(sp))$wday, mean), digits = 3)

round(100*sqrt(252)*tapply(X = dailyReturn(sp), INDEX = as.POSIXlt(index(sp))$wday, sd), digits = 1)
## weekdays explicitly shown
dowr <- 100*tapply(X = dailyReturn(sp), INDEX = as.POSIXlt(index(sp))$wday, mean) # day-of-week returns
names(dowr) <- c("Mon", "Tue", "Wed", "Thu", "Fri")
## as an extra, plot box plots (see https://en.wikipedia.org/wiki/Box_plot).
plot(x = as.factor(as.POSIXlt(index(sp))$wday), y = as.vector(100*dailyReturn(sp)),
     xlab = "Week Day (1 = Monday)", ylab = "Daily Return (%)", col = "light blue")
##
## 5. average daily returns by year and by each month
##
round(
    100 * tapply(X = dailyReturn(sp), INDEX = list(1900 + as.POSIXlt(index(sp))$year,
                                             as.POSIXlt(index(sp))$mon + 1), FUN = mean),
    digits = 2) # monthly return averages by year
##
## 6. return volatilities by year and by month
##
round(
    100 * sqrt(252) * tapply(X = dailyReturn(sp, type = "log"),
                             INDEX = list(1900 + as.POSIXlt(index(sp))$year,
                                          as.POSIXlt(index(sp))$mon + 1), FUN = sd),
    digits = 1) # monthly return volatilities by year
##
##
## 7. average Jan, Feb, ..., Dec returns and standard deviations
##
r <- 100 * monthlyReturn(sp) # monthly returns as an xts object
mret <- tapply(X = r, INDEX = as.POSIXlt(index(r))$mon + 1, FUN = mean)
round(mret,  digits = 2) # average Jan, Feb, ..., Dec returns
## average monthly return volatitlities for each month (Jan, Feb, ..., Dec)
mvolat <- apply(X = 100 * sqrt(252) * tapply(X = dailyReturn(sp, type = "log"),
                                             INDEX = list(1900 + as.POSIXlt(index(sp))$year,
                                                          as.POSIXlt(index(sp))$mon + 1), FUN = sd),
                MARGIN = 2, FUN = mean)

round(mvolat, digits = 1) # average monthly return volatilities
##
## plots of average monthly returns and average monthly volatilities
##
monthlabs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
par(mfrow = c(2, 1)) # split window to two sub-windows 
plot(x = 1:12, # months 
     y = mret, # monthly means
     type = "b", # points and  lines (i.e., both, see help(plot.default))
     xlab = "Month", ylab = "Average Return (%)", # axis labels
     main = paste("Monthly Average Returns\nSample Period ", # note: \n implie new line
                  format(index(sp)[1], format = "%d.%m.%Y"), " - ", # see help(format)
                  format(index(sp)[nrow(sp)], format = "%d.%m.%Y"), "]", sep = ""),
     xaxt = "n" # supress xaxis
     ) # plot
axis(1, at = 1:12, labels = monthlabs) # generate x-axis with month labels

plot(x = 1:12, # months 
     y = mvolat, # monthly means
     type = "b", # points and lines [i.e., both]
     xlab = "Month", ylab = "Average Volatility (%, p.a)", # axis labels
     main = paste("Monthly Average Returns\nSample Period ", # note: \n implie new line
                  format(index(sp)[1], format = "%d.%m.%Y"), " - ", # see help(format)
                  format(index(sp)[nrow(sp)], format = "%d.%m.%Y"), "]", sep = ""),
     xaxt = "n"
     ) # plot
axis(1, at = 1:12, labels = monthlabs)

##
## 9. generate box-plots over months Jan, ..., Dec
##
months <- factor(as.POSIXlt(index(sp))$mon + 1)
levels(months) <- monthlabs
plot(x = months, y = as.vector(100*dailyReturn(sp)), col = "light blue") # here as.vector is needed for coersion
