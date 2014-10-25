# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
#server.R for fixedIncome part of GARPFRM package

#Loading the required library files
library(xlsx)
library(shiny)
library(xts)

source("discountFactorArbitrage.R")
source("riskMetricsAndHedges.R")


#Modified the function bondFullPrice already present in bondFullPrice
bondFullPrice<-function(bond, yield, cashFlowPd, t0, t1, currentDate){
  compoundPd = bond$m
  face = bond$face
  couponRate = bond$couponRate
  d1 = as.numeric(t1-currentDate)
  d2 = as.numeric(t1-t0)
  tmp = 0  
  if (cashFlowPd > 1)
  {
    for(k in 1:(cashFlowPd-1)){
      tmp = tmp + ((couponRate / compoundPd * face) / ((1 + yield/compoundPd)^k))
    }
    
  }
  # Calculate dirty price based on partial periods formula
  dirtyP = (1 / ((1 + yield / compoundPd)^(d1/d2))) * (couponRate / compoundPd * face + tmp + face / ((1 + yield/compoundPd)^(cashFlowPd-1)))
  # Calculate accruedInterest
  aiDays = as.numeric(currentDate-t0)
  couponDays = as.numeric(t1-t0)
  ai = couponRate / compoundPd * face * aiDays / couponDays
  cleanP = dirtyP - ai
  return(list(dirty=dirtyP, clean=cleanP, accruedInterest=ai))
}

#Reading Inbuilt Data Files
bond.data <- readRDS("data/bond_Data.rds")
tnotes <-readRDS("data/tnotes.rds")

shinyServer(function(input, output) {  
  #Calculation the Discount Curve  
  output$discount.factor<-renderPrint({
    
    # Take a dependency on input$goButton
    input$discount.factor_button
    
    # Use isolate() to avoid dependency on other inputs    
    isolate({
      #Reading Values once the Run button is clicked
      bondprice<-as.numeric(unlist(strsplit(input$bondprice,",")))
      cr<-as.numeric(unlist(strsplit(input$cr,",")))
      ttm<-as.numeric(unlist(strsplit(input$ttm,",")))
      
      #Checking if all the inputs are numeric
      if(all(is.na(c(cr,bondprice,ttm))==FALSE)==FALSE) 
        return("All the inputs must be numeric")
            
      #Checking if the coupon rate is less than 100%
      if(!all(cr<100))
        return("Please enter a coupon rate less than 100%")
      
      interval<-max(ttm)/length(ttm)  
      #Checking if the bonds have equally spaced maturity
      if(!all(diff(ttm) == interval))
        return("Bonds must have equally spaced maturity dates")
      
      #Checking if the Arguments entered are all of equal length
      if(length(cr)!=length(bondprice)){
        return("Arguments must be of equal length") } else if(length(cr)!=length(ttm)){
        return("Arguments must be of equal length")}
      
      cr <-cr/100
      #Generating the cash flow matrix
      cashflow<-matrix(0,nrow=length(ttm),ncol=length(ttm))
      for(i in 1:nrow(cashflow))
      {
        for(j in 1:ncol(cashflow))
        {
          if(ttm[i]-j*interval==0)
          {
            cashflow[i,j]= 100 * (1+ cr[i]/2)
            break  
          }
          else
            cashflow[i,j]=100*cr[i]/2
        }
      }
      
      bondprice<-matrix(bondprice,ncol=1)
      #Getting the discount curve using Function defined in GARPFRM
      DF<-discountFactor(bondprice,cashflow)
          
      #Getting the spot rates curve using Function defined in GARPFRM
      spotrates <- spotForwardRates(ttm,DF) 
      
      #Returning the output as List
      list(BondPrice=round(bondprice,input$digits),CashFlow=round(cashflow,input$digits),
                 DiscountFactor=round(DF,input$digits),"Spot Rates"=round(spotrates,input$digits))     
    })
  } 
 )

 #Calculation the full bond price  
 output$bond.price<-renderPrint({
   
   # Take a dependency on input$goButton
   input$bond.price_button
   
   # Use isolate() to avoid dependency on other inputs    
   isolate({
     
     #Reading input values
     #next coupon date
     t1<-input$t1
     #previous coupon date
     t0<-input$t0
     #Maturity Date
     tm<-input$tm
     #Current Date
     tn<-input$tn   
     #Coupon Rate
     cr<-input$bcr
     #Yield
     y<-input$yield
          
     #Checking if Yield and Coupon rate is greated than 100%
     if(!all(c(cr,y1)<100))
       return("Please enter a percentage less than 100")     
     
     cr<-cr/100
     y<-y/100
     
     #Calculating the cash flow periods
     n <- as.numeric(round((tm-t0)/365*2))
     
     bond = bondSpec(face=100, m=2, couponRate = cr) 
     
     #Getting the Full Bond Price using function defined in GARPFRM
     bp.tn<-bondFullPrice(bond, y, n, t0, t1, tn)     
    list(dirty=round(bp.tn$dirty,input$digits),clean=round(bp.tn$clean,input$digits),
         accruedInterest=round(bp.tn$accruedInterest,input$digits)) 
    
   })
      
 } 
 )

 output$bp.plot<-renderPlot({
   
   # Take a dependency on input$goButton
   input$bondpricebutton
   
   # Use isolate() to avoid dependency on other inputs    
   isolate({
     
     #Reading input values
     #next coupon date
     t1<-input$t1
     #previous coupon date
     t0<-input$t0
     #Maturity Date
     tm<-input$tm
     #Current Date
     tn<-input$tn     
     cr<-input$bcr
     y1<-input$yield
     
          
     #Checking if Yield and Coupon rate is greated than 100%
     if(!all(c(cr,y1)<100))
       return("Please enter a percentage less than 100")     
     
     cr<-cr/100
     y1<-y1/100
     
     #Calculating the cash flow periods
     n <- as.numeric(round((tm-t0)/365*2))
     
     bond = bondSpec(face=100, m=2, couponRate = cr)   
     t1.add<- seq(t1,length=2, by = "6 months")[2]
     #Getting the Full Bond Price using function defined in GARPFRM
     bp.tn<-bondFullPrice(bond, y1, n, t0, t1, tn)
     bp.t0<-bondFullPrice(bond, y1, n, t0, t1, t0)
     bp.t1<-bondFullPrice(bond, y1, n, t0, t1, t1)
     bp.t1.clean<-bondFullPrice(bond, y1, n-1, t1, t1.add, t1)
     bp.t1.new<-bondFullPrice(bond, y1, n-1, t1, t1.add, t1.add)
     price<-cbind(bp.t0,bp.tn,bp.t1,bp.t1.clean,bp.t1.new)
     dirtyp <- price[1,]
     cleanp <- price[2,]
     
     if ( t1.add > tm)
       {date <- c(t0,tn,t1)} else 
       {date<-c(t0,tn,t1,t1,t1.add)}     
     ymin<- min(as.numeric(dirtyp[1:length(date)]),as.numeric(cleanp[1:length(date)]))
     ymax<- max(as.numeric(dirtyp[1:length(date)]),as.numeric(cleanp[1:length(date)]))
     plot(x=date,y=dirtyp[1:length(date)],type="b",xaxt="n", xlab='Settlement Date', ylab="Price"
          ,ylim = c(ymin, ymax), col= 3, lty = 1, main = "Plot Showing Variation in Price with Constant Discount Curve")
     axis(side=1, at=date, labels=format(as.Date(date), '%Y-%m-%d'))     
     lines(as.Date(date),cleanp[1:length(date)],type="l",lty=2, col = 4)
     legend("bottomleft",c("Dirty Price", "Flat Price"),lty=c(1,2),col=c(3,4), bty="n")
     
   })
   
 } 
 )
 
 
 #Calculation of Bond Parameters
 output$p<-renderPrint({
   
   # Take a dependency on input$goButton
   input$presentvaluebutton
   
   # Use isolate() to avoid dependency on other inputs    
   isolate({
    #Reading input values
    t <- input$t
    df<-as.numeric(unlist(strsplit(input$df,",")))
    cr<-input$pcr
    
    #Creating a time sequnce for cash flows until maturity
    time<-seq(from=0.5,to=t,by=0.5)
    
    #Checking if only numeric values are entered for discount curve
    if(all(is.na(df)==FALSE)==FALSE) 
      return("All the inputs must be numeric")
    
    #Checking if the coupon rate is less than 100%
    if(cr>100)
      return("Coupon Rate must be less than 100%")
    
    #Checking if the length of discount curve is greater than sequence of cashflows
    if(length(df)<length(time))
      return("Discount Curve Should be longer than time to maturiy")
    
    df<-df[1:length(time)]
    cr<-cr/100
    
    #Calculating the bondparametneres  
    bond = bondSpec(time,face=100, m=2, couponRate = cr)
    price = bondPrice(bond,df)
    ytm = bondYTM(bond,df)
    duration=bondDuration(bond,df)
    convexity=bondConvexity(bond,df)
    mduration = duration/(1+ytm/2)
    
    #Giving output as a list
    list(BondPrice=round(price,input$digits),YTM=round(ytm,input$digits),
         MacaulayDuration=round(duration,input$digits),ModifiedDuration=round(mduration,input$digits)
         ,BondConvexity=round(convexity,input$digits))
   })
   
 } 
 )
 
 #################################
 
 #Function to get the data for spot rates and discount curve rates
getDFSpotrate<-reactive(
{
  input$srbutton
  isolate({ 
#  inFile <- input$file1
  if(input$userip == 'Upload a Dataset')
  {
    
    if(input$filetype == "Excel"){
      format<-unlist(strsplit(input$file.excel$name,'[.]'))[2]
      if(format == 'xlsx' || format == 'xlsm'){
        dat<- read.xlsx(file=input$file.excel$datapath, sheetName=input$shname, 
               header=input$header, as.is=TRUE)
        
      } else {
        return ("Incorrect File Format. Please Upload an Excel File.")
      }
    } else if (input$filetype=="CSV"){
      format<-unlist(strsplit(input$file.csv$name,'[.]'))[2]
      if(format == 'csv'){
        dat <- read.table(file=input$file.csv$datapath, sep=",", header=input$header, as.is=TRUE)      
      } else {
        return ("Incorrect File Format. Please Upload a CSV File.")
      }
    } else if (input$filetype == "Text"){
        format<-unlist(strsplit(input$file.txt$name,'[.]'))[2]
        if(format == 'txt'){
          dat <- read.table(file=input$file.txt$datapath, sep=input$sep, header=input$header, as.is=TRUE)      
        } else {
          return ("Incorrect File Format. Please Upload a Text File.")
        }      
    } else if (input$filetype=="RData"){
      format<-unlist(strsplit(input$file.rdata$name,'[.]'))[2]
      if(format == 'RData'){
        load(input$file.rdata$datapath,newEnv <- new.env())
        dat<-get(unlist(ls(newEnv)),envir=newEnv)
      } else {
        return ("Incorrect File Format. Please Upload a RData File.")
      }      
    }
        
    dat.names<- colnames(dat)
    
    if(!is.element("IssueDate",dat.names) || !is.element("MaturityDate",dat.names)
       || !is.element("Coupon",dat.names) || !is.element("Ask",dat.names) 
       || !is.element("Bid",dat.names) )
      return ("Please Check the header names In the file.")
  
    if(any(is.na(as.Date(as.character(dat[,"MaturityDate"])))))
       return ("Maturity Date Column is not properly formatted")  

    dat[,"MaturityDate"]<-as.Date(dat[,"MaturityDate"])
    
    step.size = as.numeric(round(diff(dat[,"MaturityDate"])/365,1))[1]
    
    if (!all(as.numeric(round(diff(dat[,"MaturityDate"])/365,1))==step.size ))
      return ("Maturity Dates must be equall spaced")
  
    } else{
      switch(input$dataset,"T-Notes & Bonds" =  dat<-bond.data,
             "T-Notes" = dat<-tnotes)
      }  
  
  n = nrow(dat)
  CF = matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n)
  {                                  
    CF[i, 1:i] = dat[i,"Coupon"]/2
  } 
  
  diag(CF) = rep(100, n) + diag(CF)
  DF<-discountFactor(as.matrix((dat[,"Bid"]+dat["Ask"])/2),CF)
  DF <- c(1,DF)
  step.size = as.numeric(round(diff(dat[,"MaturityDate"])/365,1))[1]
  time = seq(from=0,by=step.size,length=n+1)   
  rates = spotForwardRates(time,DF) 
  #Giving output as a list
  data<-list(dat=dat,DF=DF,rates=rates,time=time)  
})
})
 
 #Calculation of spot rates and forward rates
 output$sr<-renderPrint({
   
   # Take a dependency on input$srbutton
   input$srbutton
   
   # Use isolate() to avoid dependency on other inputs    
   isolate({     
    data<-getDFSpotrate()
    if(is.list(data))
      list(DsicountFactor= round(as.vector(data$DF),input$digits)
           ,SpotRates=round(data$rates[,1],input$digits),ForwardRates=round(data$rates[,2],input$digits))
    else
      data
   })
 } 
 )
 
output$srplot<-renderPlot({
  
  # Take a dependency on input$srbutton
  input$srbutton
  
  # Use isolate() to avoid dependency on other inputs    
  isolate({     
    
    data<-getDFSpotrate()
  
    if(is.list(data))
    {
      par(mfrow=c(1,2))
      plot(data$time,data$rates[,1],type = "b",xlab = "Maturity (In Years)",ylab="Rate", main = "Spot Rates")
      plot(data$time,data$DF,type = "b",xlab = "Maturity (In Years)",ylab="Rate", main = "Discount Factors")
      par(mfrow=c(1,1))
    }
  })
} 
)
})#shinyserver



