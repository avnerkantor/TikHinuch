shinyUI(fluidPage(  includeCSS("www/style.css"),  
  tags$head(tags$script(src="custom.js")),
  titlePanel("Tik"),
  mainPanel(
  #includeHTML("www/index-1.html")
    htmlOutput("inc")
  ))
)

#http://shiny.rstudio.com/articles/templates.html