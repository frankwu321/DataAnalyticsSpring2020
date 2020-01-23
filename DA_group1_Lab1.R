setwd("/Users/wukelun/Downloads")
EPI_data = read.csv('2010EPI_data.csv', skip = 1)

##Tips (in R)
attach(EPI_data)
fix(EPI_data)
tf = is.na(EPI)
E = EPI[!tf]

##Exercise 1: exploring the distribution
summary(EPI) #stats
fivenum(EPI, na.rm = T)
stem(EPI) #stem and leaf plot
EPI_hist = hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = T)  #seq(from, to, by) prob is inverse of freq
#help(hist) 
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw='SJ'))
rug(EPI)

##Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty = 's')
#help(par)
qqnorm(EPI); qqline(EPI) #normal distribution test: if vector is normal distributed the qqplot is a straight line.
x = seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

##Exercise 1: exploring the DALY
summary(DALY)
fivenum(DALY, na.rm = T)
stem(DALY)
hist(DALY)
hist(DALY, seq(0,100,5), probability = T)
lines(density(DALY, na.rm = T, bw = 1))
lines(density(DALY, na.rm = T, bw = 'SJ'))

## fitting a distribution for DALY







