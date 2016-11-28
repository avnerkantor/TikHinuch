observe({
  updateSelectInput(session, inputId="AnalyzeYear", label="",
                    choices = c(2012), selected = 2012)
})

hebVariables<-c("מדדים", "מדדי בית ספר")
observe({
  updateSelectInput(session, "AnalyzeVariable", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$AnalyzeYear, HebSubject %in% hebVariables), HebCategory))), selected = "עושר")
})

observe({
  updateSelectInput(session, inputId="ModelId", label="", choices = c(
    "רגרסיה לינארית"="lm",
    "רגרסיה מקומית"="loess"
  ),
  selected="lm")
})

#https://rstudio.github.io/DT/options.html
# output$pisaScoresTable <- DT::renderDataTable(
#   filter='bottom',
#   colnames = c('שם משתנה', 'תיאור באנגלית', 'נושא', 'תחום', 'תת-תחום'),
#   options=list(
#     pageLength = 5,
#     searching=TRUE,
#     autoWidth = TRUE,
#     language=list(url="//cdn.datatables.net/plug-ins/1.10.12/i18n/Hebrew.json"),
#     order = list(list(3, 'desc'), list(4, 'desc'))
#   ), rownames= FALSE,
#   {
#     pisaDictionary%>%select(ID, Measure, HebSubject, HebCategory, HebSubCategory)
#   #  %>%filter(HebSubject=="מדדים")
# 
#   })

# 
#Analyze
observe({
  
  switch(input$AnalyzeYear,
         "2012"={surveyData<-pisa2012},
         "2009"={surveyData<-pisa2012},
         "2006"={surveyData<-pisa2012}
  )
  
  switch (input$Subject,
          Math = {analyzeSubject<-"PV1MATH"},
          Science={analyzeSubject<-"PV1SCIE"},
          Reading={analyzeSubject<-"PV1READ"},
          ProblemSolving={analyzeSubject<-"PV1CPRO"},
          Financial={analyzeSubject<-"PV1FLIT"}
  )
  
  
  analyzeSelectedID <- as.vector(unlist(select(filter(pisaDictionary, Year == input$AnalyzeYear, HebSubject %in% hebVariables, HebCategory==input$AnalyzeVariable), ID)))

  analyzePlotFunction<-function(country) {

    Country<-as.vector(unlist(Countries%>%filter(Hebrew==country)%>%select(CNT)))

    analyzeData1<-surveyData%>%select_("CNT", analyzeSelectedID, "ST04Q01", "ESCS", analyzeSubject)%>%filter(CNT==Country)
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
      geom_smooth(method=input$ModelId, aes(colour=groupColour), se=FALSE) +
      #geom_point(aes(colour=groupColour)) +
      geom_text(data=corData, aes(x=0, y=800, label=paste("Cor", Cor), show_guide=F)) +
      scale_colour_manual(values = groupColours) +
      labs(title="", y="" ,x= "") +
      theme_bw() +
      guides(colour=FALSE) +
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
            axis.text.x=element_blank(),
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
