
observe({
  
  plotHighData<-pisaData2%>%filter(Subject==input$Subject, Performers=="High")%>%select(-Subject)
  
  if(is.null(input$Gender)){
    if(is.null(input$Escs)){
      plotHighData2<- plotHighData %>%
        filter(Gender==0, ESCS==0)
    } else {
      plotHighData2<- plotHighData %>%
        filter(Gender == 0)%>%
        filter(ESCS %in% c(input$Escs))
    }
  } else {
    if(length(input$Gender)==1){
      if(is.null(input$Escs)) {
        plotHighData2<- plotHighData %>%
          filter(Gender  == input$Gender)%>%
          filter(ESCS == 0)
      } else {
        plotHighData2<- plotHighData %>%
          filter(Gender  == input$Gender)%>%
          filter(ESCS %in% c(input$Escs))
      }
    } else {
      plotHighData2<- plotHighData %>%
        filter(Gender %in% c(input$Gender))%>%
        filter(ESCS == 0)
    } 
  }
  
  expertiseHighPlotFunction<-function(country){
    x<-Countries%>%filter(Hebrew==country)%>%select(CNT)
    plotHighData3 <- plotHighData2%>%filter(Country==x[1,1])
    
    gh<-ggplot(plotHighData3, aes(x=Year, y=Average, colour=GenderESCS, text=round(Average, digits = 1))) +
      scale_colour_manual(values =groupColours) +
      guides(colour=FALSE) +
      labs(title="", y="%" ,x= "") +
      theme_bw() +
      theme(plot.margin=unit(c(0,15,0,0), "pt"),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            legend.position="none",
            panel.grid.major.x=element_blank(),
            panel.grid.major.y = element_line(colour="#e0e0e0", size=0.3),
            #TODO hust does not work https://github.com/hadley/ggplot2/issues/1435
            #plot.title = element_text(hjust=0),
            #axis.title.y = element_text(angle=90, vjust=1),
            axis.line.x = element_line(color="#c7c7c7", size = 0.3),
            axis.line.y = element_line(color="#c7c7c7", size = 0.3)
            ) + 
      scale_x_continuous(breaks=c(2006, 2009, 2012)) +
      scale_y_continuous(limits=c(0, 105), breaks=c(0, 20, 40, 60, 80, 100),
                         expand = c(0,0))
    
    if("2012" %in% plotHighData3$Year) {
      if("2009" %in% plotHighData3$Year) {
        gd<-gh+geom_line(size=1)
        ggplotly(gd, tooltip = list("text", color="green"))%>%
          config(p = ., displayModeBar = FALSE)%>%
          layout(hovermode="x")
      } else{
        gd<-gh+geom_point(size=2)
        ggplotly(gd, tooltip = c("text"))%>%
          config(p = ., displayModeBar = FALSE)%>%
          layout(hovermode="x")
    }
      }else {
      gh+annotate("text", label = "לא השתתפה",
                  x = 2012, y = 500, size = 6, 
                  colour = "#c7c7c7")%>%config(p = ., displayModeBar = FALSE)
    }
  }
  
  if(!input$Country1==""){
  output$Country1HighPlot<-renderPlotly({
    expertiseHighPlotFunction(input$Country1)
  })

  output$Country2HighPlot<-renderPlotly({
    expertiseHighPlotFunction(input$Country2)
  })
  
  output$Country3HighPlot<-renderPlotly({
    expertiseHighPlotFunction(input$Country3)
  })
  
  output$Country4HighPlot<-renderPlotly({
    expertiseHighPlotFunction(input$Country4)
  })

  }

##############################################


  
  plotLowData<-pisaData2%>%filter(Subject==input$Subject, Performers=="Low")%>%select(-Subject)
  
  if(is.null(input$Gender)){
    if(is.null(input$Escs)){
      plotLowData2<- plotLowData %>%
        filter(Gender==0, ESCS==0)
    } else {
      plotLowData2<- plotLowData %>%
        filter(Gender == 0)%>%
        filter(ESCS %in% c(input$Escs))
    }
  } else {
    if(length(input$Gender)==1){
      if(is.null(input$Escs)) {
        plotLowData2<- plotLowData %>%
          filter(Gender  == input$Gender)%>%
          filter(ESCS == 0)
      } else {
        plotLowData2<- plotLowData %>%
          filter(Gender  == input$Gender)%>%
          filter(ESCS %in% c(input$Escs))
      }
    } else {
      plotLowData2<- plotLowData %>%
        filter(Gender %in% c(input$Gender))%>%
        filter(ESCS == 0)
    } 
  }
  
  expertiseLowPlotFunction<-function(country){
    x<-Countries%>%filter(Hebrew==country)%>%select(CNT)
    plotLowData3 <- plotLowData2%>%filter(Country==x[1,1])
    
    gh<-ggplot(plotLowData3, aes(x=Year, y=Average, colour=GenderESCS, text=round(Average, digits = 1))) +
      scale_colour_manual(values = groupColours) +
      guides(colour=FALSE) +
      labs(title="", y="%" ,x= "") +
      theme_bw() +
      theme(plot.margin=unit(c(0,15,0,0), "pt"),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x=element_blank(),
            legend.position="none",
            panel.grid.major.y = element_line(colour="#e0e0e0", size=0.3),
            axis.line.x = element_line(color="#c7c7c7", size = 0.3),
            axis.line.y = element_line(color="#c7c7c7", size = 0.3)
            ) + 
      scale_x_continuous(breaks=c(2006, 2009, 2012)) +
      scale_y_continuous(limits=c(0, 105), breaks=c(0, 20, 40, 60, 80, 100),
                         expand = c(0,0))
    
    if("2012" %in% plotLowData3$Year) {
      if("2009" %in% plotLowData3$Year) {
        gd<-gh+geom_line(size=1)
        ggplotly(gd, tooltip = c("text"))%>%
          config(p = ., displayModeBar = FALSE)%>%
          layout(hovermode="x")
      } else{
        gd<-gh+geom_point(size=2)
        ggplotly(gd, tooltip = c("text"))%>%
          config(p = ., displayModeBar = FALSE)%>%
          layout(hovermode="x")
      }
    } else {
      gh+annotate("text", label = "לא השתתפה",
                  x = 2012, y = 500, size = 6, 
                  colour = "#c7c7c7")%>%config(p = ., displayModeBar = TRUE)
    }
  }

  if(!input$Country1==""){
    
  output$Country1LowPlot<-renderPlotly({
    expertiseLowPlotFunction(input$Country1)
  })

  
  output$Country2LowPlot<-renderPlotly({
    expertiseLowPlotFunction(input$Country2)
  })
  
  output$Country3LowPlot<-renderPlotly({
    expertiseLowPlotFunction(input$Country3)
  })
  
  output$Country4LowPlot<-renderPlotly({
    expertiseLowPlotFunction(input$Country4)
  })
}
})

