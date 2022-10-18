library(ocp)
library(lubridate)

build_df <- function(df1,df2) {
  frax_df <- read.csv(df1)
  colnames(frax_df)[2] <- 'Frax'
  frax_price <- read.csv(df2)
  frax_price$frax_date <-as.Date(as.POSIXct(frax_price$Date/1000,origin = "1970-01-01"))
  frax_df$frax_date <- as.Date(as_datetime(frax_df$Timestamp)) 
  frax_comb <- merge(frax_price,frax_df,by.x='frax_date',by.y='frax_date',how=inner)
  return(frax_comb)
}

frax_comb <- build_df('fantom_tvl - Sheet2.csv','fantom_price (1).csv')

#Score the changepoint detection
uvg <- frax_comb$Frax
ocpd1<- onlineCPD(log(uvg), getR=TRUE, optionalOutputs = TRUE, 
                  maxRlength = 100, minRlength = 2, truncRlim = 10^(-2),
                  hazard_func=function(x, lambda){const_hazard(x, lambda=100)})
cpdf<- data.frame(method=names(ocpd1$changepoint_lists))
cpdf$changepoints<- unlist(ocpd1$changepoint_lists, recursive = FALSE)
truecps <- cpdf$changepoints$maxCPs

# view the data
plot(uvg, main = "Simulated Univariate Gaussian Data with Changepoints", 
     ylab = "data values", xlab="time point", type= "l", col = "black", cex=0.5)
# show the changepoints on the graph
for(cp in truecps){
  abline(v=cp, col = "green", lwd= 2)
}

frax_comb$frax_scale <- scale(frax_comb$Frax)
frax_comb$price_scale <- scale(frax_comb$Price)

plot(frax_comb$frax_scale,type='l')
lines(frax_comb$price_scale,type='l',col='red')
for(cp in truecps){
  abline(v=cp, col = "green", lwd= 2)
}



plot(ocpd1, cplistID = 3)




#library(prophet)
#frax_df$frax_date <-as.Date(as_datetime(frax_df$Timestamp))
#df <- frax_df[,c('frax_date','Frax')]
#colnames(df) <- c('ds','y')
