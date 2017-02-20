######UI #####
observe({
  updateSelectInput(session, inputId="SurveyYear", label="", 
                    choices = c(2015, 2012), selected = 2015)
})

observeEvent(input$SurveyYear,{
  choices<-list("שאלון תלמידים"=c("משפחה ובית", "תחושות","בית הספר", "לימודי מדעים", "אמצעי תקשוב"), "שאלון תלמידים (לא בישראל)" =  c("שיעורי עזר", "שיעורי עזר במדעים", "שיעורי עזר במתמטיקה", "שיעורי עזר בקריאה"), "שאלון מנהלים"=c("מאפייני בית הספר", "ניהול בית הספר", "צוות ההוראה", "מדידה והערכה", "הקבצות", "אקלים בית הספר"), "מדדים"=c("מדדי תלמידים","מדדי בית ספר"))
  switch(input$SurveyYear,
         "2015"={choices<-choices},
         "2012"={choices<-c(unique(pisaDictionary%>%filter(Year=="2012")%>%select(HebSubject)))}
  )
  updateSelectInput(session, "SurveySubject", "", choices = choices, selected="אמצעי תקשוב")})

observeEvent(input$SurveySubject,{
  updateSelectInput(session, "SurveyCategory", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject), HebCategory))))
})

observeEvent(input$SurveyCategory,{
  updateSelectInput(session, "SurveySubCategory", "", as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject, HebCategory==input$SurveyCategory), HebSubCategory)))
  )
})

recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

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
    surveyData1<-surveyData%>%select_("COUNTRY", SurveySelectedID, "ST04Q01", "ESCS")%>%filter(COUNTRY==Country)%>%na.omit() 
    #dic<-data.frame(English=c("Checked"), Hebrew=c("כן"))
    surveyData1 <- data.frame(lapply(surveyData1, function(x) recoderFunc(x, itemsDictionary$English, itemsDictionary$Hebrew)))
    
    #output$SurveySelectedIDOutput<-renderText(paste0(SurveySelectedMeasure, ' (',SurveySelectedID, ')'))
    if(all(is.na(surveyData1[,SurveySelectedID]))){
      ggplot() + annotate("text", label = "אין נתונים",
                          x = 2012, y = 500, size = 6, 
                          colour = "#c7c7c7")%>%config(p = ., displayModeBar = FALSE) +
        theme_void() + theme(legend.position="none")
    } else {
      if(is.null(v$Gender)){
        if(is.null(v$Escs)){
          #General
          surveyTable<-surveyData1%>%
            count_(SurveySelectedID)
          # surveyTable<-collect(surveyTable)
          surveyTable<-surveyTable%>% mutate(freq = round(100 * n/sum(n), 1), groupColour="General")%>%
            rename_(answer=SurveySelectedID)
        } else {
          # General Escs
          surveyTable<-surveyData1%>%
            filter(ESCS %in% c(v$Escs))%>%
            group_by_("ESCS", SurveySelectedID)%>%
            tally  %>%
            group_by(ESCS)
          # surveyTable<-collect(surveyTable)
          surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
            rename_(answer=SurveySelectedID, group="ESCS") %>%
            mutate(groupColour=str_c("General", group))
        }
      } else {
        if(length(v$Gender)==1){
          if(is.null(v$Escs)) {
            #Only gender
            surveyTable<-surveyData1%>%
              filter(ST04Q01 %in% c(v$Gender))%>%
              group_by_("ST04Q01", SurveySelectedID)%>%
              tally  %>%
              group_by(ST04Q01)
            # surveyTable<-collect(surveyTable)
            surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
              rename_(answer=SurveySelectedID, groupColour="ST04Q01") 
            
          } else {
            surveyTable<-surveyData1%>%
              filter(ST04Q01  %in% c(v$Gender))%>%
              filter(ESCS %in% c(v$Escs))%>%
              group_by_("ESCS", SurveySelectedID)%>%
              tally  %>%
              group_by(ESCS)
            # surveyTable<-collect(surveyTable)
            surveyTable<-surveyTable%>%  mutate(freq = round(100 * n/sum(n), 0), group1=v$Gender)%>%
              rename_(answer=SurveySelectedID, group="ESCS")%>%
              mutate(groupColour=str_c(group1, group))
            
          }
        } else {
          surveyTable<-surveyData1%>%
            filter(ST04Q01 %in% c(v$Gender))%>%
            group_by_("ST04Q01", SurveySelectedID)%>%
            tally  %>%
            group_by(ST04Q01)
          # surveyTable<-collect(surveyTable)
          surveyTable<-surveyTable%>%   mutate(freq = round(100 * n/sum(n), 0))%>%
            rename_(answer=SurveySelectedID, groupColour="ST04Q01")
        } 
      }
      
      gh<-ggplot(data=surveyTable, aes(x=answer, y=freq, text=paste0(round(freq, digits = 1), "%"))) +
        geom_bar(aes(colour=groupColour, fill=groupColour), stat="identity") +
        coord_flip() +
        scale_x_discrete(limits = rev(levels(surveyTable$answer))) +
        scale_colour_manual(values =groupColours) +
        scale_fill_manual(values = groupColours) +
        labs(title="", y="אחוז עונים" ,x= "") +
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
              panel.spacing.x=unit(2, "lines"),
              axis.title=element_text(colour="#777777", size=10)
              #axis.line.x = element_line(color="#c7c7c7", size = 0.3),
              #axis.line.y = element_line(color="#c7c7c7", size = 0.3)
        ) 
      ggplotly(gh, tooltip = c("text"))%>%
        config(p = ., displayModeBar = FALSE, displaylogo = FALSE, linkText="עריכה",
               showLink = TRUE)%>%
        layout(hovermode="y")
    }
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
  ##These 2 lines changed UI
  filter='bottom',
  colnames = c('שם משתנה', 'תיאור באנגלית', 'נושא', 'תחום', 'תת-תחום'),
  ##
  options=list(
    pageLength = 5,
    searching=TRUE,
    autoWidth = TRUE,
    language=list(url="//cdn.datatables.net/plug-ins/1.10.12/i18n/Hebrew.json"),
    order = list(list(3, 'desc'), list(4, 'desc'))
  ), rownames= FALSE, 
  {
    pisaDictionary%>%filter(Year==input$SurveyYear)%>%select(ID, Measure, HebSubject, HebCategory, HebSubCategory)
  })

