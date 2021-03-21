#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)


if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)


shinyUI(fluidPage(
# Application title
    titlePanel("COVID-MAP"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
    sidebarPanel(tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #ffffff;
               background-color: #3399ff;
               z-index: 105;
             }
          ")),
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                      tags$div("Loading...",id="loadmessage")),
          h4("アニメーションの設定"),
          uiOutput("date1"),
          uiOutput("date2"),
          numericInput("w",label = h5("累積日数を入力"),value="7"),
          numericInput("x",label = h5("アニメーションの日数の間隔を入力"),value="3"),
          actionButton("submit", "描画"),
          downloadButton('downloadData', 'GIF Download'),
          sliderInput("color",
                      label = "色の調整(10万人当たりの感染者数の最大値より大きくしてください。)",
                      min = 10,
                      max = 1000,
                      value = 500,)
                     
                     
                     ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            imageOutput("anime"),
        )
    )


)
)
