library(shiny)
library(ggplot2)
library(plotly)

Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
Lx = Lxs[-(1:3)]

Wholelife_annuity = function(age,ir){
    lx = Lx[-(1:(age-20))]; if (age==20) lx = Lx
    V = 1/(1+ir)
    epv = sum( (V^(0:(120-age)))*lx )/Lx[age-20+1]
    return(epv)}

Wholelife_assurance = function(age,ir){
    DD = ir/(1+ir)
    epv = 1 - DD*Wholelife_annuity(age,ir)
    return(epv)}

library(shiny)
library(shinydashboard)



# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Insurance Benefit Evaluation Tool",
                        titleWidth = 350,
                        dropdownMenuOutput("msgOutput")),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Single Policy",tabName = "spolicy", icon = icon("user")),
                menuItem("Couple Policy",tabName = "dpolicy", 
                            icon = icon("users")))
            ),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "spolicy",
                        fluidRow(
                            box(width = 5,
                                solidHeader = T,status = "primary",title = "PLOT CONTROLS",
                                selectInput("Policy","Policy Holder Life",
                                            c("Life X"
                                              ,"Life Y")),
                                sliderInput(inputId="Age",label="Age", value = 30,min=20,max=100),
                                
                                numericInput("IR",label="Interest Rate (in %)",value=4,min=0,max =50),
                                
                                radioButtons("BenefitPayment",label="Insurance Benefit payment", 
                                             choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
                                
                                numericInput(inputId="AssuredSum",label="Assured Sum",value=1,min=0,max=100000000),
                                
                                radioButtons("products",label="Select Products", 
                                             choices = list("Annuity","Assurance"), selected = "Assurance"),
                                conditionalPanel(condition = "input.products == 'Assurance'",
                                                 
                                                 selectInput("assurance_product","Select Assurance Product",
                                                             c("Pure Endowment",
                                                               "Endowment Assurance","Term Assurance","Whole Life Assurance"))
                                ),
                                conditionalPanel(condition = "input.products == 'Annuity'",
                                                 
                                                 selectInput("annuity_product","Select Annuity Product",
                                                             c("Whole Life Annuity"
                                                               ,"Guranteed Whole Life Annuity","Temporary Annuity"))
                                ),
                               
                        
                                sliderInput(inputId="term",label="Select Term", value = 10,min=1,max=70)
                      ),
                          
                            box(width = 7,
                                plotlyOutput("id1"),
                                solidHeader = T,status = "primary",title = "LINE PLOT",
                                
                                ),
                      
                    
                      infoBoxOutput("premeum_cost"),
                      
                        ),
                      fluidRow(
                          box(width = 6,title = "PREMIUM",status = "primary", solidHeader = T,
                              radioButtons(inputId = "premium_payment",label="Premium Payments", 
                                           choices = list("Level"," Single"), selected = "Level"),
                              conditionalPanel(condition = "input.premium_payment == 'Level'",
                                             selectInput("premium_frequency","Premium Payment Frequency",
                                                         c("Yearly"
                                                           ,"Monthly","Quarterly","Semi Annually"))
                                             
                                             ),
                              
                              
                          ),
                          box(width = 6,title = "EXPENSES",status = "primary", solidHeader = T,
                              conditionalPanel(condition = "input.premium_payment == 'Level'",
                                               
                                               sliderInput(inputId="renewal_expenses",label="Renewal Expenses", value = 15,min=0,max=50)
                                               ),
                              conditionalPanel(condition = "input.products== 'Assurance'",
                                               
                                               sliderInput(inputId="Claim_expenses",label="Claim Expenses", value = 15,min=0,max=50)
                              ),
                              sliderInput(inputId="initial_expenses",label="Initila Expenses", value = 15,min=0,max=50)
                             # sliderInput(inputId="Claim_expenses",label="Claim Expenses", value = 15,min=0,max=50)
                                
                      )
                      )
                    ),
                    
                tabItem(tabName = "dpolicy",
                        fluidRow(
                            box(width = 5,
                                solidHeader = T,status = "primary",title = "PLOT CONTROLS",
                                sliderInput(inputId="Age_1",label="Firts Policy Holder Age", value = 30,min=20,max=100),
                                sliderInput(inputId="Age_2",label="Second Policy Holder Age", value = 30,min=20,max=100),
                                numericInput("IR_1",label="Interest Rate (in %)",value=4,min=0,max =50),
                                radioButtons("BenefitPayment_1",label="Insurance Benefit payment", 
                                             choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
                                numericInput(inputId="AssuredSum_1",label="Assured Sum",value=1,min=0,max=100000000),
                                radioButtons("products_1",label="Select Products", 
                                             choices = list("Annuity","Assurance"), selected = "Assurance"),
                                conditionalPanel(condition = "input.products_1 == 'Assurance'",
                                                 
                                                 selectInput("assurance_product_1","Select Assurance Product",
                                                             c("Joint Whole Life Assurance",
                                                               "Joint Term Assurance"))
                                ),
                                conditionalPanel(condition = "input.products_1 == 'Annuity'",
                                                 
                                                 selectInput("annuity_product_1","Select Annuity Product",
                                                             c("Joint Whole Life Annuity"
                                                               ,"Joint Temporary Annuity"))
                                ),
                              
                                sliderInput(inputId="term_1",label="Select Term", value = 10,min=1,max=70)
                                
                            ),
                            
                            box(width = 7,
                                plotlyOutput("id1_1"),
                                solidHeader = T,status = "primary",title = "LINE PLOT",
                            ),
                            
                            infoBoxOutput("premeum_cost_1"),
                            
                        ),
                        fluidRow(
                            box(width = 6,title = "PREMIUM",status = "primary", solidHeader = T,
                                radioButtons(inputId = "premium_payment_1",label="Premium Payments", 
                                             choices = list("Level"," Single"), selected = "Level"),
                                conditionalPanel(condition = "input.premium_payment_1 == 'Level'",
                                                 selectInput("premium_frequency_1","Premium Payment Frequency",
                                                             c("Yearly"
                                                               ,"Monthly","Quarterly","Semi Annually"))
                                                 
                                ),
                                
                                
                            ),
                            box(width = 6,title = "EXPENSES",status = "primary", solidHeader = T,
                                
                                conditionalPanel(condition = "input.premium_payment_1 == 'Level'",
                                                 
                                                 sliderInput(inputId="renewal_expenses_1",label="Renewal Expenses", value = 15,min=0,max=50)
                                ),
                                conditionalPanel(condition = "input.products_1== 'Assurance'",
                                                 
                                                 sliderInput(inputId="Claim_expenses_1",label="Claim Expenses", value = 15,min=0,max=50)),
                                
                                sliderInput(inputId="initial_expenses_1",label="Initila Expenses", value = 15,min=0,max=50)
                                
                                
                               # uiOutput("id_expenses_1"),
                            )
                            
                        )
                          
                        
                        )
            )
        )
    )
)


