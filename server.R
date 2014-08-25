# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
#server.R for fixedIncome part of GARPFRM package

library(shiny)
library(GARPFRM)
source("discountFactorArbitrage.R")
source("riskMetricsAndHedges.R")
data(crsp_weekly)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  #Calculation the Discount Curve  
  output$df<-renderPrint({
    
    # Take a dependency on input$goButton
    input$discountfactorbutton
    
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
        return("Bonds must have equally spaced maturity")
      
      #Checking if the Arguments entered are all of equal length
      if(length(cr)!=length(bondprice))
        return("Arguments must be of equal length")
      else if(length(cr)!=length(ttm))
        return("Arguments must be of equal length")
      

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
      
      #Returning the output as List
      list(BondPrice=bondprice,CashFlow=cashflow,DiscountFactor=DF)      
    })
  } 
 )

 #Calculation the full bond price  
 output$bp<-renderPrint({
   
   # Take a dependency on input$goButton
   input$bondpricebutton
   
   # Use isolate() to avoid dependency on other inputs    
   isolate({
     
     #Reading input values
     t1<-input$t1
     t0<-input$t0
     tm<-input$tm
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
     
     #Getting the Full Bond Price using function defined in GARPFRM
     bondFullPrice(bond, y1, n, t0, t1, tn)
     
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
    list(BondPrice=price,YTM=ytm,MacaulayDuration=duration,
         ModifiedDuration=mduration,BondConvexity=convexity)
   })
   
 } 
 )
 
 
})#shinyserver