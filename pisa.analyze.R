observe({
  updateSelectInput(session, inputId="AnalyzeYear", label="",
                    choices = c(2015, 2012), selected = 2015)
})

hebVariables<-c("מדדים", "מדדי בית ספר")
observe({
  updateSelectInput(session, "AnalyzeVariable", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$AnalyzeYear, HebSubject %in% hebVariables), HebCategory))), selected = "עושר")
})

# observe({
#   updateSelectInput(session, inputId="ModelId", label="", choices = c(
#     "רגרסיה לינארית"="lm",
#     "רגרסיה מקומית"="loess"
#   ),
#   selected="lm")
# })

 
#Analyze
observe({
  
  switch(input$AnalyzeYear,
         "2015"={surveyData<-pisa2015},
         "2012"={surveyData<-pisa2012},
         "2009"={surveyData<-pisa2012},
         "2006"={surveyData<-pisa2012}
  )
  
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

    analyzeData2[, analyzeSelectedID]<-as.numeric(unlist(analyzeData1[, analyzeSelectedID]))
    analyzeData2[, analyzeSubject]<-as.numeric(unlist(analyzeData1[, analyzeSubject]))
    
    analyzeData3<-analyzeData2 %>% group_by(groupColour) %>% summarize(correlation = cor(analyzeData2[,analyzeSubject], analyzeData2[,analyzeSelectedID], use="complete", method = "pearson"))
    corData<-as.data.frame(analyzeData3)
    names(corData)<-c("Group", "Cor")
    corData$Cor<-round(corData$Cor, digits = 2)
    
    #lm
    #lm(WEALTH~PV1MATH, analyzeData1)
    #dplyr+lm
    #analyzeData2%>%group_by(groupColour) %>%do(mod=lm(WEALTH ~ PV1MATH, data= .))

    #broom+dplyr
    #analyzeData2 %>% group_by(groupColour) %>% summarize(correlation = cor(PV1MATH, WEALTH, use="complete", method = "pearson"))
    #analyzeData2 %>% group_by(groupColour) %>% do(tidy(lm(WEALTH ~ PV1MATH, data=.)))

    #Intercept: How highly expressed the gene is when it’s starved of that nutrient.
    #rate: How much the gene’s expression responds to an increasing supply of that nutrient (and therfore an increasing growth rate)

    #library(data.table)
    #analyzeData3<-setDT(analyzeData2)[, list(Slope = summary(lm(WEALTH ~ PV1MATH))$coeff[2], Pearson=cor(WEALTH, use="complete", PV1MATH, method = "pearson")), groupColour]

    ggplot(data=analyzeData2, aes_string(y=analyzeSubject, x=analyzeSelectedID)) +
      geom_smooth(method="lm", aes(colour=groupColour), se=TRUE) +
      geom_point(aes(colour=groupColour)) +
      geom_text(data=corData, aes(x=0, y=800, label=paste("Cor", Cor), show_guide=F)) +
      scale_colour_manual(values = groupColours) +
      labs(title="", y="" ,x= "") +
      theme_bw() +
      guides(colour=FALSE) +
      scale_y_continuous(limits = c(0,800)) +
      theme(plot.margin=unit(c(0,15,5,10), "pt"),
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

  #### Plots ####
  if(length(analyzeSelectedID)==1){
    output$Country1AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country1)
    })
    output$Country2AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country2)
    })
    output$Country3AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country3)
    })
    output$Country4AnalyzePlot<-renderPlot({
      analyzePlotFunction(input$Country4)
    })
  }
})
