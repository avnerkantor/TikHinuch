# observe({
#   updateSelectizeInput(session, 'analyzeVariables',
#                        choices = as.character(pisaDictionary$ID),
#                        selected = "WEALTH",
#                        options=list(placeholder="בחר/י"))
# })
# observe({
#   updateSelectInput(session, inputId="ModelId", label="", choices = c(
#     "רגרסיה לינארית"="lm",
#     "רגרסיה מקומית"="loess"
#   ),
#   selected="loess")
# })

#https://rstudio.github.io/DT/options.html
output$pisaScoresTable <- DT::renderDataTable(
  filter='bottom',
  colnames = c('שם משתנה', 'תיאור באנגלית', 'נושא', 'תחום', 'תת-תחום'),
  options=list(
    pageLength = 5,
    searching=TRUE,
    autoWidth = TRUE,
    language=list(url="//cdn.datatables.net/plug-ins/1.10.12/i18n/Hebrew.json"),
    order = list(list(3, 'desc'), list(4, 'desc'))
  ), rownames= FALSE,
  {
    pisaDictionary%>%select(ID, Measure, HebSubject, HebCategory, HebSubCategory)
  #  %>%filter(HebSubject=="מדדים")

  })

# 
# #Analyze
# observe({
#   SurveySelectedID <- as.vector(unlist(select(filter(pisaDictionary, Year == input$SurveyYear, HebSubject == input$SurveySubject, HebCategory==input$SurveyCategory, HebSubCategory==input$SurveySubCategory), ID))) 
#   
#   analyzePlotFunction<-function(country) {
#     
#     Country<-as.vector(unlist(Countries%>%filter(Hebrew==country)%>%select(CNT)))
#     
#     switch ( input$Subject,
#              Math = {analyzeSubject<-"PV1MATH"},
#              Science={analyzeSubject<-"PV1SCIE"},
#              Reading={analyzeSubject<-"PV1READ"}
#     )
#     surveyData<-pisa2012bq
#     if(input$SurveyYear==2012)
#       surveyData<-pisa2012bq
#     
#     #analyzeData1<-pisa2012%>%select_("CNT", "WEALTH", "ST04Q01", "ESCS", "PV1MATH")%>%filter(CNT=="ISR")
#     #SurveySelectedID<-"WEALTH"
#     #analyzeSubject<-"PV1MATH"
#     
#     
#     analyzeData1<-surveyData%>%select_("CNT", SurveySelectedID, "ST04Q01", "ESCS", analyzeSubject)%>%filter(CNT==Country)
#     analyzeData1<-collect(analyzeData1)
#     
#     #analyzeData1$WEALTH<- as.numeric(as.character(analyzeData1$WEALTH))
#     
#     if(is.null(input$Gender)){
#       if(is.null(input$Escs)){
#         #General
#         analyzeData2<-analyzeData1%>%
#           mutate(groupColour="General")
#       } else {
#         # General Escs
#         analyzeData2<-analyzeData1%>%
#           rename_(group="ESCS") %>%
#           mutate(groupColour=str_c("General", group))
#       }
#     } else {
#       if(length(input$Gender)==1){
#         if(is.null(input$Escs)) {
#           #Only gender
#           analyzeData2<-analyzeData1%>%
#             rename_(groupColour="ST04Q01") 
#         } else {
#           analyzeData2<-analyzeData1%>%
#             mutate(group1=input$Gender)%>%
#             rename_(group="ESCS")%>%
#             mutate(groupColour=str_c(group1, group))
#         }
#       } else {
#         analyzeData2<-analyzeData1%>%
#           rename_(groupColour="ST04Q01") 
#       } 
#     }
#     
#     #lm
#     #lm(WEALTH~PV1MATH, analyzeData1)
#     #dplyr+lm
#     #analyzeData2%>%group_by(groupColour) %>%do(mod=lm(WEALTH ~ PV1MATH, data= .))
#     
#     #broom+dplyr
#     #analyzeData2 %>% group_by(groupColour) %>% summarize(correlation = cor(PV1MATH, WEALTH, use="complete", method = "pearson"))
#     #analyzeData2 %>% group_by(groupColour) %>% do(tidy(lm(WEALTH ~ PV1MATH, data=.)))
#     
#     #Intercept: How highly expressed the gene is when it’s starved of that nutrient.
#     #rate: How much the gene’s expression responds to an increasing supply of that nutrient (and therfore an increasing growth rate)
#     
#     #library(data.table)
#     #analyzeData3<-setDT(analyzeData2)[, list(Slope = summary(lm(WEALTH ~ PV1MATH))$coeff[2], Pearson=cor(WEALTH, use="complete", PV1MATH, method = "pearson")), groupColour]
#     
#     
#     
#     ggplot(data=analyzeData2, aes_string(y=analyzeSubject, x=SurveySelectedID)) +
#       geom_smooth(method=input$modelId, aes(colour=groupColour)) + 
#       scale_colour_manual(values = groupColours) +
#       labs(title="", y="" ,x= "") +
#       theme_bw() +
#       guides(colour=FALSE) +
#       theme(plot.margin=unit(c(0,15,5,10), "pt"),
#             panel.border = element_blank(),
#             axis.ticks = element_blank(),
#             panel.grid.major.x=element_blank(),
#             panel.grid.major.y = element_line(colour="#e0e0e0", size=0.3),
#             legend.position="none",
#             axis.line.x = element_line(color="#c7c7c7", size = 0.3),
#             axis.line.y = element_line(color="#c7c7c7", size = 0.3),
#             strip.background = element_blank(),
#             strip.text.x = element_blank(),
#             axis.title.x=element_blank(),
#             axis.text.x=element_blank(),
#             axis.ticks.x=element_blank()
#       )
#     
#     # ggplotly(gh, tooltip = c("text"))%>%
#     # config(p = ., displayModeBar = FALSE)%>%
#     # layout(hovermode="y")
#   }
#   
#   #### Plots ####
#   if(length(SurveySelectedID)==1){
#     # if(!input$Subject==""){
#     output$Country1AnalyzePlot<-renderPlotly({
#       analyzePlotFunction(input$Country1)
#     })
#     
#     output$Country2AnalyzePlot<-renderPlotly({
#       analyzePlotFunction(input$Country2)
#     })
#     
#     output$Country3AnalyzePlot<-renderPlotly({
#       analyzePlotFunction(input$Country3)
#     })
#     
#     output$Country4AnalyzePlot<-renderPlotly({
#       analyzePlotFunction(input$Country4)
#     })
#   }
# })
