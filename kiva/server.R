library(dplyr)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(shinydashboard)
library(DT)

shinyServer(function(input,output){

  #Render map on the map tab
  output$map <- renderGvis({
    gvisGeoChart(b,locationvar = "country",colorvar=input$metrics,sizevar=input$selected,
                 options=list(width=1000, height=800,title="KIVA Microfinancing Trend",
                              colors="['#c1e7ff','#abd2ec','#94bed9','#7faac6','#6996b3','#5383a1','#3d708f','#255e7e','#004c6d']"))
    # using width="auto" and height="auto" to
    # automatically adjust the map size
  })
  #Render info boxes for map tab 
  output$maxBox <- renderInfoBox({
    max_value <- max(b[,input$selected])
    max_country <- 
     b$country[b[,input$selected] == max_value]
     infoBox(max_country, paste0("$", formatC(max_value, format="f", digits=0, big.mark=",")), 
             icon = icon("angle-double-up"),fill=T, color="blue")
   })
  output$minBox <- renderInfoBox({
     min_value <- min(b[,input$selected])
     min_country <- 
      b$country[b[,input$selected] == min_value]
     infoBox(min_country, paste0("$", formatC(min_value, format="f", digits=0, big.mark=",")),
             icon = icon("angle-double-down"),fill=T,color="blue")
   })
  output$avgBox <- renderInfoBox({
    avg_value <- round(colMeans(b[,input$selected]))
    input <- paste('Mean of',input$selected)
    infoBox(input, paste0("$", formatC(avg_value, format="f", digits=0, big.mark=",")),
            icon = icon("balance-scale"),fill=T,color="blue")
  })
  output$mpi1Box <-renderValueBox({
    mpi1 <- unique(kivaloans%>%filter(country==input$country1)%>%select(.,MPI))
    input <- paste("MPI for", input$country1)
    valueBox(format(mpi1,digits=3),icon=icon("money-bill-wave"),input,color="blue")
  })
  output$avgloan1Box <- renderValueBox({
    avgloan1 <- unique(b%>%filter(country==input$country1)%>%select(.,Avg.Loan.Amount))
    avgloan1 <- avgloan1[[1]]
    input <- paste("Avg Loan in", input$country1)
    valueBox(paste0("$", formatC(avgloan1, format="f", digits=0, big.mark=",")),input,
             icon = icon("landmark"),color="blue")
  })
  output$fund1Box <- renderValueBox({
    fund1<- unique(p%>%filter(country==input$country1)%>%select(.,Partner.Count))
    fund1 <- fund1[[1]]
    input <- paste("Partner Count for", input$country1)
    valueBox(paste0(formatC(fund1,digits=0)),input,
             icon = icon("hands-helping"),color="blue")
  })
  output$mpi2Box <-renderValueBox({
    mpi2 <- unique(kivaloans%>%filter(country==input$country2)%>%select(.,MPI))
    input <- paste("MPI for",input$country2)
    valueBox(format(mpi2,digits=3),icon=icon("money-bill-wave"),input,color="orange")
  })
  output$avgloan2Box <- renderValueBox({
    avgloan2 <- unique(b%>%filter(country==input$country2)%>%select(.,Avg.Loan.Amount))
    avgloan2 <- avgloan2[[1]]
    input <- paste("Avg Loan in", input$country2)
    valueBox(paste0("$", formatC(avgloan2, format="f", digits=0, big.mark=",")),
            icon = icon("landmark"),input,color="orange")
  })
  output$fund2Box <- renderValueBox({
    fund2<- unique(p%>%filter(country==input$country2)%>%select(.,Partner.Count))
    fund2 <- fund2[[1]]
    input <- paste("Partner Count for",input$country2)
    valueBox(paste0(formatC(fund2,digits=0)),input,
             icon = icon("hands-helping"),color="orange")
  })

  output$loansectors1<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Unfunded.Amount)
    bar2<-filter(bar1,country==input$country1)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Unfunded.Amount), y=Total.Unfunded.Amount, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+scale_fill_brewer(palette = "Blues")+theme_bw()+
      labs(x="Sector", y="Total Unfunded Amount",title=paste("Top Unfunded Sectors for",input$country1))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$loansectors2<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Unfunded.Amount)
    bar2<-filter(bar1,country==input$country2)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Unfunded.Amount), y=Total.Unfunded.Amount, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+
      scale_fill_brewer(palette = "Oranges")+theme_bw()+
      labs(x="Sector", y="Total Unfunded Amount",title=paste("Top Unfunded Sectors for",input$country2))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$loansectors3<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Loan.Balance)
    bar2<-filter(bar1,country==input$country1)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Loan.Balance), y=Total.Loan.Balance, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+
      scale_fill_brewer(palette = "Blues")+theme_bw()+
      labs(x="Sector", y="Total Loan Amount",title=paste("Top Loan Amount for",input$country1))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$loansectors4<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Loan.Balance)
    bar2<-filter(bar1,country==input$country2)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Loan.Balance), y=Total.Loan.Balance, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+
      scale_fill_brewer(palette = "Oranges")+theme_bw()+
      labs(x="Sector", y="Total Loan Amount",title=paste("Top Loan Amount for",input$country1))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$loansectors5<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Funded.Amount)
    bar2<-filter(bar1,country==input$country1)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Funded.Amount), y=Total.Funded.Amount, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+
      scale_fill_brewer(palette = "Blues")+theme_bw()+
      labs(x="Sector", y="Total Funded Amount",title=paste("Top Funded Amount for",input$country1))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$loansectors6<-renderPlot({
    bar1<-bar1%>%top_n(5,Total.Funded.Amount)
    bar2<-filter(bar1,country==input$country2)
    ggplot(data=bar2,aes(x=reorder(Sector,Total.Funded.Amount), y=Total.Funded.Amount, fill=Sector))+
      geom_bar(stat="identity")+coord_flip()+
      scale_fill_brewer(palette = "Oranges")+theme_bw()+
      labs(x="Sector", y="Total Funded Amount",title=paste("Top Funded Amount for",input$country1))+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  })
  output$gender1<-renderPlot({
    ggplot(data=g,aes(x=unfunded_amount, color=gender))+
      geom_histogram(aes(y=..density..,fill=gender),binwidth=input$slider2, alpha=0.5,position='dodge')+
    geom_density(size=0.8,alpha=1,show.legend = F)+
      xlim(input$slider1)+ylim(input$slider3)+
      scale_color_manual(values=c("#E69F00","#56B4E9","#999999"))+
      scale_fill_manual(values=c("#E69F00","#56B4E9","#999999"))+
      theme_bw()+
      labs(x="Unfunded Amount",y="",title="Unfunded Distribution",fill="Gender")+
      theme(plot.title = element_text(hjust = 0.5,size=16))+
      guides(color=F, fill = guide_legend("Gender"))
    
  })
  output$gender2<-renderPlot({
    ggplot(data=g,aes(x=term_in_months,color=gender))+
      geom_histogram(aes(fill=gender),binwidth=6, alpha=0.5,position='dodge')+
      scale_color_manual(values=c("#E69F00","#56B4E9","#999999"))+
      scale_fill_manual(values=c("#E69F00","#56B4E9","#999999"))+
      theme_bw()+xlim(0,50)+
      labs(x="Term in Months",y="",title="Lending Terms",fill="Gender")+
      theme(plot.title = element_text(hjust = 0.5,size=16))+
      guides(color=F, fill = guide_legend("Gender"))
  })
  output$gender3<-renderPlot({
    ggplot(data=g,aes(x=lender_count,color=gender))+
      geom_histogram(aes(fill=gender),binwidth=5, alpha=0.5,position='dodge')+
      scale_color_manual(values=c("#E69F00","#56B4E9","#999999"))+
      scale_fill_manual(values=c("#E69F00","#56B4E9","#999999"))+
      theme_bw()+xlim(0,50)+
      labs(x="Number of Lenders Per Loan",y="",title="Lending Terms",fill="Gender")+
      theme(plot.title = element_text(hjust = 0.5,size=16))+
      guides(color=F, fill = guide_legend("Gender"))
    
  })
  output$table <- DT::renderDataTable({
     datatable(b, rownames=FALSE) %>% 
       formatStyle(input$selected,  
                  background="#56B4E9", fontWeight='bold')
  })
})
