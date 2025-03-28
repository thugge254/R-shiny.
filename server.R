library(shiny)
library(ggplot2)
library(plotly)


Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
Lx = Lxs[-(1:3)]
Ly = Lxs[1:101]
LT<-reactive({if(input$Policy=="Life X"){Lx}else{Ly}}) 



Wholelife_annuity = function(age,InterestRate){
    lx = Lx[-(1:(age-20))]; if (age==20) lx = Lx
    V = 1/(1+InterestRate)
    epv = sum( (V^(0:(120-age)))*lx )/Lx[age-20+1]
    return(epv)}

Wholelife_assurance = function(age,InterestRate){
    DD = InterestRate/(1+InterestRate)
    epv = 1 - DD*Wholelife_annuity(age,InterestRate)
    return(epv)}


server = function(input,output) {
    output$prem = renderText( {
        age = input$Age
        InterestRate = input$IR/100
        Insurance_Data=data.frame(Lxs)
        Insurance_Data$Time=0:input$term
        Insurance_Data$Reserve=(Lxs-input$AssuredSum)+((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate)
        
        
        if (input$BenefitPayment=="End of year of death") {EPVB = Wholelife_assurance(age,InterestRate) }
        if (input$BenefitPayment=="Immediately on death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
        
        Prem = input$AssuredSum*EPVB
        print(Prem)
    } )
    
    output$premeum_cost<-renderInfoBox({
        age = input$Age
        InterestRate = input$IR/100
      
        if (input$annuity_product=="Whole Life Annuity") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate)}
        if (input$annuity_product=="Temporary Annuity") {EPVB = Wholelife_assurance(age,InterestRate)+5 }
        if (input$annuity_product=="Guranteed Whole Life Annuity") {EPVB = Wholelife_assurance(age,InterestRate)+10 }
        if (input$assurance_product=="Term Assurance") {EPVB = Wholelife_assurance(age,InterestRate)^0.5}
        if (input$assurance_product=="Pure Endowment") {EPVB = 10+Wholelife_assurance(age,InterestRate)^0.5}
        if (input$BenefitPayment=="End of year of death") {EPV = Wholelife_assurance(age,InterestRate) }
        if (input$BenefitPayment=="Immediately on death") {EPV = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
        if (input$Policy =="Life Y"){EPVB = Wholelife_assurance(age,InterestRate)^0.6}
        if (input$Policy =="Life X"){EPVB = Wholelife_assurance(age,InterestRate)^0.4}
        
        
        
        Prem = input$AssuredSum*EPVB+input$renewal_expenses+input$initial_expenses*EPV
        
        infoBox(
            "PREMIUM COST", value =paste0("$",round(Prem,digits = 4)) , icon = icon("list"),
            subtitle = if (input$premium_payment=="Level"){
                input$premium_frequency
                }else {
                    "Single Payment"
                },
            color = "purple"
        )
        
    })

    
    output$id1=renderPlotly({
        age = input$Age
        InterestRate = input$IR/100
        i=seq(0,0.01,length=104)
        Insurance_Data=data.frame(Lxs)
        Insurance_Data$Time=  seq(0,input$term, by = (length(1:input$term))/length(Insurance_Data$Lxs))[-4]
        
        if(input$products=="Assurance"){
            i=seq(0,1.5,length=104)
            Insurance_Data$Reserve= (seq(0,input$AssuredSum,(length(1:input$AssuredSum))/length(Insurance_Data$Lxs))[-4])^(exp(i))^Wholelife_assurance(age,InterestRate)
        }else{
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve= (seq(0,input$AssuredSum,(length(1:input$AssuredSum))/length(Insurance_Data$Lxs))[-4])^(exp(i))+Wholelife_assurance(age,InterestRate)
        }
        if (input$annuity_product=="Whole Life Annuity"){
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve=-Insurance_Data$Reserve#(seq(-input$AssuredSum,0,(length(1:input$AssuredSum))/length(Insurance_Data$Lxs))[-4])^(exp(i))+Wholelife_assurance(age,InterestRate)
        }else if (input$annuity_product=="Temporary Annuity"){
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve=(Insurance_Data$Time-5)^2-4
        }else{
          i=seq(0,0.01,length=104)
          Insurance_Data$Reserve=(Insurance_Data$Time-5)+50
          
          
        }
            
        if(input$assurance_product=="Term Assurance"){
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve=-(Insurance_Data$Time-5)^2-4
            
        }
        
        
        if (input$Policy =="Life Y"){
          i=seq(0,0.01,length=104)
          Insurance_Data$Reserve=-Insurance_Data$Reserve+60
        }else{
          i=seq(0,0.01,length=104)
          Insurance_Data$Reserve=-Insurance_Data$Reserve+20
        }
        
        
        if (input$BenefitPayment=="End of year of death") {
            EPVB = Wholelife_assurance(age,InterestRate)
        }else{
            EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate)
            
        }
        
       # if (input$BenefitPayment=="End of year of death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
        ggplot(Insurance_Data, aes(x=Time, y=Reserve)) +
            geom_line(col="blue",size=2) +
            labs(title =paste("Reserves"),
                 x="Policy in Years",y="Required Reserve")+
            theme(
                axis.text= element_text(size=12, face="bold", colour = "black")
                
            )
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$premeum_cost_1<-renderInfoBox({
        age_1 = input$Age_1
        age_2=input$Age_2
        InterestRate_1 = input$IR_1/100
        
        if (input$annuity_product_1=="Joint Whole Life Annuity") {EPVB_1 = age_2+((1+InterestRate_1)^0.5)*Wholelife_assurance(age_1,InterestRate_1)}
        if (input$annuity_product_1=="Joint Temporary Annuity") {EPVB_1 = age_2+Wholelife_assurance(age_1,InterestRate_1)+5 }
        #if (input$annuity_product_1=="Guranteed Whole Life Annuity") {EPVB_1 = 10.546+((1+InterestRate_1)^0.5)*Wholelife_assurance(age_1,InterestRate_1)}
        if (input$assurance_product_1=="Joint Whole Life Assurance") {EPVB_1 = age_2+10+Wholelife_assurance(age_1,InterestRate_1)^0.5}
        if (input$assurance_product_1=="Term Assurance") {EPVB_1 = age_2+Wholelife_assurance(age_1,InterestRate_1)^0.5}
        if (input$BenefitPayment_1=="End of year of death") {EPV_1 = age_2+Wholelife_assurance(age_1,InterestRate_1) }
        if (input$BenefitPayment_1=="Immediately on death") {EPV_1 = age_2+((1+InterestRate_1)^0.5+20)*Wholelife_assurance(age_1,InterestRate_1) }
        
        Prem_1 = input$AssuredSum_1+EPVB_1+input$renewal_expenses_1+input$initial_expenses_1+EPV_1-EPV_1
        
        infoBox(
            "PREMIUM COST", value =paste0("$",round(Prem_1,digits = 4)) , icon = icon("list"),
            subtitle = if (input$premium_payment_1=="Level"){
                input$premium_frequency_1
            }else {
                "Single Payment"
            },
            color = "purple"
        )
        
    })
    
    
 
    
    
    
    output$id1_1=renderPlotly({
        
        age_1 = input$Age_1
        age_2=input$Age_2
        InterestRate_1 = input$IR_1/100
        Insurance_Data=data.frame(Lxs)
        i=seq(0,0.01,length=104)
        Insurance_Data$Time=seq(0,input$term_1, by = (length(1:input$term_1))/length(Insurance_Data$Lxs))[-4]
        #Insurance_Data$Reserve=(seq(0,input$AssuredSum_1,(length(1:input$AssuredSum_1))/length(Insurance_Data$Lxs))[-4])^(exp(i))
        
   
        

        
        if(input$products_1=="Assurance"){
            i=seq(0,1.5,length=104)
            Insurance_Data$Reserve= (seq(0,input$AssuredSum_1,(length(1:input$AssuredSum_1))/length(Insurance_Data$Lxs))[-4])^(exp(i))^Wholelife_assurance(age_1,InterestRate_1)
        }else{
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve= (seq(0,input$AssuredSum_1,(length(1:input$AssuredSum_1))/length(Insurance_Data$Lxs))[-4])^(exp(i))+Wholelife_assurance(age_1,InterestRate_1)
            
        }
        if (input$annuity_product_1=="Joint Whole Life Annuity"){
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve=-Insurance_Data$Reserve#(seq(-input$AssuredSum,0,(length(1:input$AssuredSum))/length(Insurance_Data$Lxs))[-4])^(exp(i))+Wholelife_assurance(age,InterestRate)
        }else{
            i=seq(0,0.01,length=104)
            #EPVB_1 = age_2+Wholelife_assurance(age_1,InterestRate_1)
            Insurance_Data$Reserve=(Insurance_Data$Time-5)^2-4
            
        }
        
        if(input$assurance_product_1=="Joint Term Assurance"){
            i=seq(0,0.01,length=104)
            Insurance_Data$Reserve=-(Insurance_Data$Time-5)^2-4
            
        }
        if (input$BenefitPayment_1=="End of year of death") {EPVB_1 = age_2+Wholelife_assurance(age_1,InterestRate_1) }
        if (input$BenefitPayment_1=="End of year of death") {EPVB_1 = age_2+Wholelife_assurance(age_1,InterestRate_1) }
        
        ggplot(Insurance_Data, aes(x=Time, y=Reserve)) +
            geom_line(col="blue",size=2) +
            labs(title =paste("Reserves"),
                 x="Policy in Years",y="Required Reserve")+
            theme(
                axis.text= element_text(size=12, face="bold", colour = "black")
                
            )
    })
    
    
    
    
}


