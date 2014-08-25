
# FixedIncome App

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  headerPanel("Fixed Income Overview"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view. The helpText function is also used to 
  # include clarifying text. Most notably, the inclusion of a 
  # submitButton defers the rendering of output until the user 
  # explicitly clicks the button (rather than doing it immediately
  # when inputs change). This is useful if the computations required
  # to render output are inordinately time-consuming.
  
  
  sidebarPanel(
    
    #Displaying the CFRM and  GARP logo
    p(a(img(src="cfrm-logo.png", height = 40, width = 304), 
        target="_blank", href="http://depts.washington.edu/compfin/")),
    p(a(img(src="garp_logo_sm.gif", height = 40, width = 304), 
        target="_blank", href="http://www.garp.org")),
    
    #Conditionalpanel function to change sidebarpanel according to tabs
    
    #UI for getting the Discount Factor
    conditionalPanel(condition="input.conditionedPanels==1",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("discountfactorbutton", "Run!"),
                     tags$hr(),
                     
                     h5("Bond Prices"),
                     textInput("bondprice", "Enter the bond prices separated by comma (in $):" , 
                               value = "100.550,104.513,105.856"),
                     tags$hr(),
                     
                     h5("Coupon Rates"),
                     textInput("cr", "Enter the Coupon Rates for the bonds separated by comma (in %):" , 
                               value = "1.25,4.875,4.5"),
                     
                     tags$hr(),
                     h5("Time To Maturity"),
                     textInput("ttm", "Enter the time to maturities for the bonds separated by comma (in years):" , 
                               value = "0.5,1,1.5")
                     
                     
    ),
    
    #UI for getting the Bond Price    
    conditionalPanel(condition="input.conditionedPanels==2",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("bondpricebutton", "Run!"),
                     tags$hr(),
                     
                     h5("Current Date"),
                     dateInput("tn", "Enter Current Date:" , 
                               value = "2013-10-04"),
                     tags$hr(),
                     
                     h5("Previous Coupon Date"),
                     dateInput("t0", "Enter Previous Coupon Date:" , 
                               value = "2013-08-15"),
                     
                     tags$hr(),
                     h5("Next Coupon Date"),
                     dateInput("t1", "Enter Next Coupon Date:" , 
                               value = "2014-02-15"),
                     tags$hr(),
                     h5("Maturity Date"),
                     dateInput("tm", "Enter Maturity Date:" , 
                               value = "2017-08-15"),
                     tags$hr(),
                     h5("Coupon Rate"),
                     numericInput("bcr", "Enter Coupon Rate:",4.75,step=0.1),                     
                     
                     tags$hr(),
                     h5("Yield"),
                     numericInput("yield", "Enter Yield:" , 
                                  value = 0.961,step=0.1)                     
                     
    ),
    #UI for getting the Bond Parameters
    conditionalPanel(condition="input.conditionedPanels==3",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("presentvaluebutton", "Run!"),
                     tags$hr(),
                     
                     h5("Enter Coupon rate"),
                     numericInput("pcr", "Enter Coupon Rate:" , 
                                  value = 4.75),                     
                     tags$hr(),
                     h5("Discount Curve"),
                     textInput("df", "Enter Discount Factors separated by comma:" , 
                               value = "0.9680000, 0.9407242, 0.9031545, 0.8739803"),
                     tags$hr(),
                     
                     h5("Years to Maturity"),
                     numericInput("t", "Enter Years to Maturity:" , 
                               value = 2)
                       
                     
    ) 
    
    
  ),
  
  #UI for displaying outputs
  mainPanel(
    tabsetPanel(
      tabPanel("Discount Factor", 
               verbatimTextOutput("df"),
               value=1), 
      tabPanel("Bond Pricing", 
               verbatimTextOutput("bp"),
               value=2),
      tabPanel("Bond Parameters", 
               verbatimTextOutput("p"),
               value=3),      
       id = "conditionedPanels"
    )
  )
))