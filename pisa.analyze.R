observe({
  updateSelectInput(session, inputId="AnalyzeYear", label="",
                    choices = c(2015, 2012), selected = 2015)
})

hebVariables<-c("מדדים", "מדדי בית ספר")
observe({
  updateSelectInput(session, "AnalyzeVariable", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$AnalyzeYear, HebSubject %in% hebVariables), HebCategory))), selected = "גודל כיתה")
})

observe({
  updateSelectInput(session, inputId="ModelId", label="", choices = c(
    "רגרסיה לינארית"="lm",
    "רגרסיה מקומית"="loess"
  ),
  selected="lm")
})

 
#Analyze
observe({
  
  if(input$AnalyzeYear=="2015" & input$worldOrIsrael=="Israel"){
    surveyData<-israel2015
  }
  if(input$AnalyzeYear=="2015" & input$worldOrIsrael=="World"){
    surveyData<-pisa2015
  }
  if(input$AnalyzeYear=="2012" & input$worldOrIsrael=="Israel"){
    surveyData<-israel2012
  }
  if(input$AnalyzeYear=="2012" & input$worldOrIsrael=="World"){
    surveyData<-pisa2012
  }
  
  
  switch (input$Subject,
          Math = {analyzeSubject<-"PV1MATH"},
          Science={analyzeSubject<-"PV1SCIE"},
          Reading={analyzeSubject<-"PV1READ"}
          # ProblemSolving={analyzeSubject<-"PV1CPRO"},
          # Financial={analyzeSubject<-"PV1FLIT"}
  )
  
  
  analyzeSelectedID <- as.vector(unlist(select(filter(pisaDictionary, Year == input$AnalyzeYear, HebSubject %in% hebVariables, HebCategory==input$AnalyzeVariable), ID)))

  analyzePlotFunction<-function(country) {

    Country<-as.vector(unlist(Countries%>%filter(Hebrew==country)%>%select(Country)))

    analyzeData1<-surveyData%>%select_("COUNTRY", analyzeSelectedID, "ST04Q01", "ESCS", analyzeSubject)%>%filter(COUNTRY==Country)
    # analyzeData1<-collect(analyzeData1)

    if(is.null(input$Gender)){
      if(is.null(input$Escs)){
        #General
        analyzeData2<-analyzeData1%>%
          mutate(groupColour="General")
      } else {
        # General Escs
        analyzeData2<-analyzeData1%>%
          rename_(group="ESCS") %>%
          mutate(groupColour=str_c("General", group))
      }
    } else {
      if(length(input$Gender)==1){
        if(is.null(input$Escs)) {
          #Only gender
          analyzeData2<-analyzeData1%>%
            rename_(groupColour="ST04Q01")
        } else {
          analyzeData2<-analyzeData1%>%
            mutate(group1=input$Gender)%>%
            rename_(group="ESCS")%>%
            mutate(groupColour=str_c(group1, group))
        }
      } else {
        analyzeData2<-analyzeData1%>%
          rename_(groupColour="ST04Q01")
      }
    }

    ggplot(data=analyzeData2, aes_string(y=analyzeSubject, x=analyzeSelectedID)) +
      geom_smooth(method=input$ModelId, aes(colour=groupColour), se=TRUE) +
      geom_point(aes(colour=groupColour), alpha = 0.1) +
      scale_colour_manual(values = groupColours) +
      labs(title="", y="ציון" ,x= "") +
      theme_bw() +
      guides(colour=FALSE) +
      scale_y_continuous(limits = c(0,800)) +
      theme(plot.margin=unit(c(5,15,5,10), "pt"),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.major.y = element_line(colour="#e0e0e0", size=0.3),
            legend.position="none",
            axis.line.x = element_line(color="#c7c7c7", size = 0.3),
            axis.line.y = element_line(color="#c7c7c7", size = 0.3),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.title.x=element_blank(),
            # axis.text.x=element_blank(),
            axis.ticks.x=element_blank()
      ) +
      scale_y_continuous(limits = c(0,800)) 

    # ggplotly(gh, tooltip = c("text"))%>%
    # config(p = ., displayModeBar = FALSE)%>%
    # layout(hovermode="y")
  }

  analyzeFunction<-function(country) {
    Country<-as.vector(unlist(Countries%>%filter(Hebrew==country)%>%select(Country)))
    
    analyzeData1<-surveyData%>%select_("COUNTRY", analyzeSelectedID, "ST04Q01", "ESCS", analyzeSubject)%>%filter(COUNTRY==Country)
    # analyzeData1<-collect(analyzeData1)
    
    if(is.null(input$Gender)){
      if(is.null(input$Escs)){
        #General
        analyzeData2<-analyzeData1%>%
          mutate(groupColour="General")
      } else {
        # General Escs
        analyzeData2<-analyzeData1%>%
          rename_(group="ESCS") %>%
          mutate(groupColour=str_c("General", group))
      }
    } else {
      if(length(input$Gender)==1){
        if(is.null(input$Escs)) {
          #Only gender
          analyzeData2<-analyzeData1%>%
            rename_(groupColour="ST04Q01")
        } else {
          analyzeData2<-analyzeData1%>%
            mutate(group1=input$Gender)%>%
            rename_(group="ESCS")%>%
            mutate(groupColour=str_c(group1, group))
        }
      } else {
        analyzeData2<-analyzeData1%>%
          rename_(groupColour="ST04Q01")
      }
    }
    
    analyzeData1[, analyzeSelectedID]<-as.numeric(analyzeData1[, analyzeSelectedID])
    analyzeData1[, analyzeSubject]<-as.numeric(analyzeData1[, analyzeSubject])
    analyzeData3<-analyzeData2 %>% group_by(groupColour) %>% do(glance(lm(get(analyzeSubject) ~ get(analyzeSelectedID), data=.)))

    paste0(input$Gender, ":", input$Escs, " R²=", round(analyzeData3$r.squared, digits=3), ", N=", analyzeData3$df.residual, ".  ")
  }
  
  #### Plots ####
  if(length(analyzeSelectedID)==1){
    output$Country1AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country1)
    })
    output$Country1AnalyzePlotText <-renderText({
      analyzeFunction(input$Country1)
    })
    output$Country2AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country2)
    })
    output$Country2AnalyzePlotText <-renderText({
      analyzeFunction(input$Country2)
    })
    output$Country3AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country3)
    })
    output$Country3AnalyzePlotText <-renderText({
      analyzeFunction(input$Country3)
    })
    output$Country4AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country4)
    })
    output$Country4AnalyzePlotText <-renderText({
      analyzeFunction(input$Country4)
    })
  }
})
