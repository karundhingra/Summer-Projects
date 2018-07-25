# Read in your data from excell, saved as a dos text file
D<- read.table("C:/Users/karun/Desktop/dow_jones_index.data",sep=",",header=T)
dimnames(D)
open = as.numeric(gsub("\\$", "", D$open))
open[is.na(open)] = mean(open)
open_ts = ts(open, frequency = 4)

close = as.numeric(gsub("\\$", "", D$close))
close[is.na(close)] = mean(close)
close_ts = ts(close, frequency = 4)

D$volume[is.na(D$volume)] = mean(D$volume)
volume_ts = ts(D$volume, frequency = 4)

D$percent_change_next_weeks_price[is.na(D$percent_change_next_weeks_price)] = mean(D$percent_change_price)

plot.ts(D$percent_change_next_weeks_price)
ts_pcnwp = ts(D$percent_change_next_weeks_price, frequency = 2)
plot(ts_pcnwp)

pcnwp = HoltWinters(DJ_monthly$percent_change_next_weeks_price)
#decompose_pcnwp = decompose(ts_pcnwp, "additive")
#detren_pcnwp = ts_pcnwp - decompose_pcnwp$trend

#plot(detren_pcnwp)
HoltWinters(ts_pcnwp, gamma=FALSE, l.start=608, b.start=9)
pcnwpforecasts2 <- forecast.HoltWinters(pcnwp, h=10)

fit <- stl(ts_pcnwp, s.window = "period")
plot(fit)

Box.test(pcnwp_diff1,lag = 15, type = "Ljung-Box")



B = c(0,1)
open_1 = filter(open_ts,B,sides=1)
close_1 = filter(close_ts,B,sides=1)
vol_1 = filter(volume_ts,B,sides=1)


X1 = cbind(open_1,close_1,vol_1)

m1 = arima(ts_pcnwp[1:187],xreg = X1, order = c(2,0,2))
res = residuals(m1)

plot(res)
acf(res)

Box.test(res,lag=10)

spectrum(res,log='no')

