######UI #####
observe({
  updateSelectInput(session, inputId="SurveyYear", label="", 
                    choices = c(2015, 2012), selected = 2015)
})

observeEvent(input$SurveyYear,{
  updateSelectInput(session, "SurveySubject", "", choices = c(unique(pisaDictionary%>%filter(Year==input$SurveyYear)%>%select(HebSubject))),
  selected="זמינות ושימוש באמצעי תקשוב")
})

observeEvent(input$SurveySubject,{
  updateSelectInput(session, "SurveyCategory", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject), HebCategory))))
})

observeEvent(input$SurveyCategory,{
  updateSelectInput(session, "SurveySubCategory", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject, HebCategory==input$SurveyCategory), HebSubCategory)))
  )
})

observe({
  
  SurveySelectedID <- as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject, HebCategory==input$SurveyCategory, HebSubCategory==input$SurveySubCategory), ID))) 
  # SurveySelectedMeasure <- as.vector(unlist(pisaDictionary%>%filter(ID==SurveySelectedID, Year == input$SurveyYear)%>%select(Measure)))
  SurveySelectedMeasure <- as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject, HebCategory==input$SurveyCategory, HebSubCategory==input$SurveySubCategory), Measure))) 
  
  
  surveyPlotFunction<-function(country) {
    if(input$SurveyYear=="2015" & input$worldOrIsrael=="Israel"){
      surveyData<-israel2015
    }
    if(input$SurveyYear=="2015" & input$worldOrIsrael=="World"){
      surveyData<-pisa2015
    }
    if(input$SurveyYear=="2012" & input$worldOrIsrael=="Israel"){
      surveyData<-israel2012
    }
    if(input$SurveyYear=="2012" & input$worldOrIsrael=="World"){
      surveyData<-pisa2012
    }
    

    Country<-as.vector(unlist(Countries%>%filter(Hebrew==country)%>%select(Country)))
    
    surveyData1<-surveyData%>%select_("COUNTRY", SurveySelectedID, "ST04Q01", "ESCS")%>%filter(COUNTRY==Country)
    
    output$SurveySelectedIDOutput<-renderText(paste0(SurveySelectedMeasure, ' (',SurveySelectedID, ')'))
    
    if(is.null(input$Gender)){
      if(is.null(input$Escs)){
        #General
        surveyTable<-surveyData1%>%
          count_(SurveySelectedID)
         # surveyTable<-collect(surveyTable)
        surveyTable<-surveyTable%>% mutate(freq = round(100 * n/sum(n), 1), groupColour="General")%>%
          rename_(answer=SurveySelectedID)
      } else {
        # General Escs
        surveyTable<-surveyData1%>%
          filter(ESCS %in% c(input$Escs))%>%
          group_by_("ESCS", SurveySelectedID)%>%
          tally  %>%
          group_by(ESCS)
         # surveyTable<-collect(surveyTable)
        surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
          rename_(answer=SurveySelectedID, group="ESCS") %>%
          mutate(groupColour=str_c("General", group))
      }
    } else {
      if(length(input$Gender)==1){
        if(is.null(input$Escs)) {
          #Only gender
          surveyTable<-surveyData1%>%
            filter(ST04Q01 %in% c(input$Gender))%>%
            group_by_("ST04Q01", SurveySelectedID)%>%
            tally  %>%
            group_by(ST04Q01)
           # surveyTable<-collect(surveyTable)
          surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
            rename_(answer=SurveySelectedID, groupColour="ST04Q01") 
          
        } else {
          surveyTable<-surveyData1%>%
            filter(ST04Q01  %in% c(input$Gender))%>%
            filter(ESCS %in% c(input$Escs))%>%
            group_by_("ESCS", SurveySelectedID)%>%
            tally  %>%
            group_by(ESCS)
           # surveyTable<-collect(surveyTable)
          surveyTable<-surveyTable%>%  mutate(freq = round(100 * n/sum(n), 0), group1=input$Gender)%>%
            rename_(answer=SurveySelectedID, group="ESCS")%>%
            mutate(groupColour=str_c(group1, group))
          
        }
      } else {
        surveyTable<-surveyData1%>%
          filter(ST04Q01 %in% c(input$Gender))%>%
          group_by_("ST04Q01", SurveySelectedID)%>%
          tally  %>%
          group_by(ST04Q01)
         # surveyTable<-collect(surveyTable)
        surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
          rename_(answer=SurveySelectedID, groupColour="ST04Q01")
      } 
    }
    ####ggplot####
    gh<-ggplot(data=surveyTable, aes(x=answer, y=freq, text=paste0(round(freq, digits = 1), "%"))) +
      geom_bar(aes(colour=groupColour, fill=groupColour), stat="identity") +
      coord_flip() +
      scale_colour_manual(values =groupColours) +
      scale_fill_manual(values = groupColours) +
      labs(title="", y="" ,x= "") +
      theme_bw() +
      guides(colour=FALSE) +
      facet_grid(. ~groupColour) +
      scale_y_continuous(breaks=c(0, 100), limits = c(0, 100)) +
      theme(plot.margin=unit(c(0,0,0,0), "pt"),
        panel.border = element_blank(),
        panel.grid.major=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none",
        strip.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=8, angle=0),
        panel.spacing.x=unit(2, "lines")
        #axis.line.x = element_line(color="#c7c7c7", size = 0.3),
        #axis.line.y = element_line(color="#c7c7c7", size = 0.3)
        ) 
    
    ggplotly(gh, tooltip = c("text"))%>%
      config(p = ., displayModeBar = FALSE)%>%
      layout(hovermode="y")
  }
  
  ### Plots ####  
  if(length(SurveySelectedID)==1){
    output$Country1SurveyPlot<-renderPlotly({
      surveyPlotFunction(input$Country1)
    })
    output$Country2SurveyPlot<-renderPlotly({
      surveyPlotFunction(input$Country2)
    })
    output$Country3SurveyPlot<-renderPlotly({
      surveyPlotFunction(input$Country3)
    })
    output$Country4SurveyPlot<-renderPlotly({
      surveyPlotFunction(input$Country4)
    })
  }
})

#https://rstudio.github.io/DT/options.html
output$pisaScoresTable <- DT::renderDataTable(
  #filter='bottom',
  #colnames = c('שם משתנה', 'תיאור באנגלית', 'נושא', 'תחום', 'תת-תחום'),
  options=list(
    pageLength = 5,
    searching=TRUE,
    autoWidth = TRUE,
    language=list(url="//cdn.datatables.net/plug-ins/1.10.12/i18n/Hebrew.json"),
    order = list(list(3, 'desc'), list(4, 'desc'))
  ), rownames= FALSE, 
  {
    pisaDictionary%>%filter(Year==input$SurveyYear)%>%select(ID, Measure, HebSubject, HebCategory, HebSubCategory)
    #  %>%filter(HebSubject=="מדדים")
    
  })

observeEvent(input$pisaDictionary, {
  showModal(modalDialog(
    title = "Important message",
    "This is an important message!",
    easyClose = TRUE
  ))
})
