

suppressMessages(library(GARPFRM)) 
options(digits=3) 
data(bonds)



cashFlow=rbind(c(100+(1+1/4)/2,0,0),c((4+7/8)/2,100+(4+7/8)/2,0),c((4+1/2)/2,(4+1/2)/2,100+(4+1/2)/2)) 
cashFlow
price = matrix(c(100.550, 104.513, 105.856), ncol=1) 
price
DF = discountFactor(price, cashFlow)
DF



cashFlow = rbind(c(100, 0, 0, 0), c(2 + 7/8, 102 + 7/8, 0, 0), 
			c(3 + 3/4, 3 + 3/4, 103 + 3/4, 0), 
			c(3 + 3/4, 3 + 3/4, 3 + 3/4, 103 + 3/4)) 
cashFlow
price = matrix(c(96.8, 99.56, 100.86, 101.22), ncol=1)
price
DF = discountFactor(price , cashFlow) 
DF
# As to confirm solution check that price is replicable 
(cashFlow %*% price) /100 



# Application: Idiosyncratic Pricing of US Treasury Notes and Bonds 
t0 = as.Date("2013-08-15") 
t1 = as.Date("2014-02-15") 
tn = as.Date("2013-10-04") 
currentDate = tn 
# Apply a coupon rate of 4.75% bond and create a bond object 
bond = bondSpec(face=100, m=2, couponRate = 0.0475) 
y1 = 0.00961 
bondFullPrice(bond, y1, 8, t0, t1, tn)$clean 
bondFullPrice(bond, y1, 8, t0, t1, tn)$dirty 
bondFullPrice(bond, y1, 8, t0, t1, tn)$accruedInterest



# Data format
head(dat) 
ccRate = compoundingRate(dat, initialDate=as.Date("2000-05-15"), m=4, face=100)
years = ccRate$years 
rate = ccRate$ccRate 
# Plot of continuously compounded spot rates 
plot(x=years, y=rate, type="l", ylab="Rate", xlab="Time to Maturity", main="Term Structure: Spot Rates")



# Esimate Spot, and Forward Rates 
DF = c(0.996489, 0.991306, 0.984484, 0.975616, 0.964519) 
time = seq(from=0.5, to=2.5, by=0.5) 
rates = spotForwardRates(time,DF) 
rates



# 1) Choose a 2 year bond with semiannual payments to match number of bond prices and CFs 
time = seq(from=0.5, to=2, by=0.5) 
# 2) Define a bond object to be used throughout the analysis, where m is the compound frequency 
bond = bondSpec(time, face=100, m=2, couponRate = 0.0475) 
bond
# 3) Estimate price
price = bondPrice(bond,DF) 
price
# 4) Estimate yield-to-maturity (YTM) 
### Yield-to-maturity is often quoted when describing a security in terms of rates rather than price. 
YTM = bondYTM(bond,DF)
YTM



cashFlow = rbind(c(100, 0, 0, 0), c(2 + 7/8, 102 + 7/8, 0, 0), 
			c(3 + 3/4, 3 + 3/4, 103 + 3/4, 0), 
			c(3 + 3/4, 3 + 3/4, 3 + 3/4, 103 + 3/4)) 
price = matrix(c(96.8, 99.56, 100.86, 101.22), ncol=1)
DF = discountFactor(price , cashFlow) 



# Duration Estimation
mDuration = bondDuration(bond,DF) 
mDuration
# Convexity Estimation 
convexity = bondConvexity(bond,DF)
convexity



## Example with a longer compounding time sequence: 
## Yields of bond with varying coupons over  Estimation and Plot 
## Utilizing a discount factor trable rewrite DF 10 years semiannually 
DF = rbind( 0.9615, 0.94305, 0.9246, 0.90591, 0.889, 0.87019, 0.8548, 0.8358825, 0.8219, 0.80294, 0.7903, 0.7713, 0.7599, 0.74092, 0.7307, 0.7117325, 0.7026, 0.70059, 0.6756) 
time = seq(0.5,10,0.5) 
# Estimate bond specs using a 4% coupon rate 
bond = bondSpec(time, face=100, m=2, couponRate = 0.04) 
bond
bondYTM(bond,DF)
# Illustrating elasticity of YTM: measure 1% and 10% increase in yield on duration 
DF=rbind(0.95434,0.9434,0.917232,0.89,0.85678,0.8396,0.81242,0.7921,0.7693,0.7473,0.7298,0.7050) 
# Choose a 2 year bond with semiannual payments to match number of bond prices and CFs 
time = seq(from=0.5, to=6, by=0.5) 
# First define a bond object to be used throughout the analysis, where m is the compound frequency 
bond = bondSpec(time, face=100, m=2, couponRate = 0.0475) 
# Duration measures the effect of a small parallel shift in the yield curve bondDuration(bond,DF) 
bondDuration(bond,DF,0.01) 
bondDuration(bond,DF,0.1)



## Valuation and Risk Model Section- Yield Curve Shapes 
# Vasicek Modeling to illustrate different yield curve calibrations 
# Initialize Model Parameters
theta = 0.10 
k = 0.8 
sigma = 0.08 
# Setup seven yield curves to estimate 
r = seq(0, 0.15, 0.025) 
length(r) 
maturity = 10 
# Illustration #1 for standard theta and initial r estimate yield path 
yieldCurves = yieldCurveVasicek(r, k, theta, sigma, maturity) 
# Plot using matplot-plot the columns of one matrix against the columns of another 
maturity = seq(1,maturity,1) 
matplot(maturity, yieldCurves, type="l", lty=1, main="Yield Curves") 
# choose h = theta for y horizontal line 
abline(h = theta, col="red", lty=2)



# Illustration #2 for high theta and low initial r estimate yield path 
theta = 0.45 
maturity = 10 
yieldCurves = yieldCurveVasicek(r, k, theta, sigma, maturity) 
# Plot using matplot-plot the columns of one matrix against the columns of another 
maturity = seq(1,maturity,1) 
matplot(maturity, yieldCurves, type="l", lty=1, main="Yield Curves") 
# Choose h = theta for y horizontal line 
abline(h = theta, col="red", lty=2)


