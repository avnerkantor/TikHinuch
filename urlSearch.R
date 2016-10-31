#https://gallery.shinyapps.io/032-client-data-and-query-string/?a=xxx&b=yyy#zzz
#https://github.com/rstudio/shiny-examples/tree/master/061-server-to-client-custom-messages
#http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs

# output$queryText <- renderText({
#   query <- parseQueryString(session$clientData$url_search) 
#   # Return a string with key-value pairs
#   paste(names(query), query, sep = "=", collapse=", ")
# })

#Pull from url
observe({
  # Parse the GET query string
  querySearch <- parseQueryString(session$clientData$url_search)
  
  updateSelectInput(session, "Subject", selected=querySearch$subject)
  updateCheckboxGroupInput(session, inputId="Gender", selected = querySearch$gender)
  updateCheckboxGroupInput(session, inputId="Escs", selected = querySearch$escs)
  updateRadioButtons(session, "worldOrIsrael", selected=querySearch$worldOrIsrael)
  updateSelectInput(session, "Country1", selected=querySearch$country1)
  updateSelectInput(session, "Country2", selected=querySearch$country2)
  updateSelectInput(session, "Country3", selected=querySearch$country3)
  updateSelectInput(session, "Country4", selected=querySearch$country4)
  #updateNumericInput(session, "LevelNumber", selected=querySearch$level)
  updateSelectInput(session, "SurveyYear", selected=querySearch$surveyYear)
  updateSelectInput(session, "SurveySubject", selected=querySearch$hebSubject)
  updateSelectInput(session, "SurveyCategory", selected=querySearch$hebCategory)
  #updateSelectInput(session, "ModelId", selected=querySearch$modelId)
})

#Push to url
observe({
  #query search is case sensitive
  queryHash <- parseQueryString(session$clientData$url_hash_initial)
  data<-paste0(queryHash, "?subject=", input$Subject, 
               "&gender=", input$Gender, "&escs=", input$Escs,
               "&worldOrIsrael=", input$worldOrIsrael,
               "&country1=", input$Country1, "&country2=", input$Country2, "&country3=", input$Country3,
                "&country4=", input$Country4, 
              "&level=", input$LevelNumber, 
              "&surveyYear=", input$SurveyYear, "&hebSubject=", input$SurveySubject,
              "&hebCategory=", input$SurveyCategory, "&hebSubCategory="=input$SurveySubCategory)
        # "$modelId=", ModelId)  
  session$sendCustomMessage(type='updateSelections', message=data)    
})

