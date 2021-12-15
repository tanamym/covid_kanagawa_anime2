
if (!require(htmltools)) {
  install.packages("htmltools")
}
library(htmltools)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
if (!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

if (!require(lubridate)) {
  install.packages("lubridate")
}
library(lubridate)

if (!require(rsconnect)) {
  install.packages("rsconnect")
}
library(rsconnect)

if (!require(mapview)) {
    install.packages("mapview")
}
library(mapview)
if (!require(magick)) {
    install.packages("magick")
}
library(stringr)
if (!require(stringr)) {
  install.packages("stringr")
}
library(magick)
if (!require(webshot)) {
    install.packages("webshot")
   
}
# library(rgdal)
# if (!require(rgdal)) {
#   install.packages("rgdal")
#   
# }
library(sf)
if (!require(sf)) {
  install.packages("sf")

}

#webshot :: install_phantomjs()



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
 
  data2020 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2020.csv",encoding="UTF-8")
  
  data202106 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202106.csv",encoding="UTF-8")
  data202109 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202109.csv",encoding="UTF-8")
  
  data2021 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2021.csv",encoding="UTF-8")
  ycd <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/yoko_covid.csv",encoding="UTF-8") %>%
    mutate(Fixed_Date=as.Date(Date),
           Residential_City=City)
  data7 <-
    rbind(data2020,data202106,data202109,data2021) %>%
    mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
    arrange(desc(Fixed_Date),Hos,hos)%>%
    count(Fixed_Date,Residential_City,hos,X,Y)%>%
    full_join(ycd%>%
                mutate(hos="yokohama"))%>%
    mutate(Residential_City=ifelse(!is.na(City),City,Residential_City)) %>%
    mutate(n=ifelse(!is.na(City),count,n))
  
  
  date <- 
    data.frame(Date=min(data7$Fixed_Date):max(data7$Fixed_Date)) %>%
    arrange(desc(Date)) %>%
    mutate(Date=as.Date(Date,origin="1970-01-01")) %>%
    filter(Date>="2020-04-20")
    output$date1<-
      renderUI({
        dateInput("z1",
                  label = h5("アニメーションの開始日を入力"),
                  max = date[1,1],value = date[1,1])
      })
    output$date2<-
      renderUI({
        dateInput("z2",
                  label = h5("アニメーションの終了日を入力"),
                  max = date[1,1],value = date[1,1])
      })
   


    jinko<-read.csv("jinko.csv")
    jinko<-data.frame(jinko)
    # layers <- ogrListLayers("N03-190101_14_GML/N03-19_14_190101.shp")
    # # windowsの場合はencodingを指定しないと文字化けする
    # Encoding(layers[1]) <- "UTF-8"
    # shp <- readOGR("N03-190101_14_GML/N03-19_14_190101.shp", layer=layers[1],
    #                stringsAsFactors = FALSE, encoding = "UTF-8")
    # #神奈川県に絞る
    # 
    # shp@data<-
    #   mutate(shp@data,N03_003=ifelse(is.na(N03_003),"",N03_003))%>%
    #   mutate(N03_004=ifelse(N03_003=="横浜市","横浜市",
    #                         ifelse(N03_003=="相模原市","相模原市",N03_004)))%>%
    #   mutate(N03_004=ifelse(N03_003=="川崎市",paste0(N03_003,N03_004),N03_004))
    shp <-read_sf("N03-190101_14_GML/N03-19_14_190101_2.shp",options = "ENCODING=CP932") 
    l1=function(a,b){
       #集計
       data7.1<-data7%>%
         filter(Fixed_Date>=a,Fixed_Date<=b)%>%
         group_by(Residential_City,X,Y)%>%
         summarise(count=n())%>%
         ungroup()%>%
         filter(X>0,Y>0)
      
      
       jinko2<-left_join(data7.1,jinko,by=c("Residential_City"="City"))
       jinko3<-jinko2%>%
         mutate(count_j=count/jinko*100000)%>%
         mutate(N03_004=Residential_City)
       data7.2<-
         sp::merge(shp, jinko3,
                   by="N03_004", all=F,duplicateGeoms = TRUE)
       #色設定
       pal <- colorNumeric(palette=c("white","red"),domain=c(0,input$color), reverse=F)
       pal2<-
         data7.2%>%
         mutate(col=pal(count_j),
                col2=ifelse(count_j>input$color,"red",col),
                flag=ifelse(count_j>input$color,paste0(input$color,"~"),paste0(count_j%/%10*10,"~")))
       data7.2%>%
         leaflet() %>%
         #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>% 
         setView(lng=139.424343, lat=35.417843,zoom=10)%>%
         addProviderTiles(providers$CartoDB.Positron) %>% 
         addPolygons(fillOpacity = 1,
                     weight=1,
                     color = "#666",
                     #labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                     fillColor = ~pal2$col2,
                     label = paste0(data7.2$N03_004,round(data7.2$count_j,2))
         )%>%
         addControl(tags$div(HTML(paste(a,b,sep = "~")))  , position = "topright")
       
       
    }
    
   output$anime<-renderImage({
     num<-
         as.numeric(lubridate::ymd(input$z2)-lubridate::ymd(input$z1))
       num2<-
         num%/%input$x
     action<-eventReactive(input$submit,{
       
      for (i in 1:num2) {
        date<-lubridate::ymd(input$z2)-input$x*(num2-i)
       date2<-lubridate::ymd(input$z2)-input$w-input$x*(num2-i)+1
       map<-l1(date2,date)
       mapshot(map, file =paste0("map_", formatC(i,width=2,flag="0"), ".png"))
      }
      
     file_names <- list.files(pattern = "map_\\d+.png$", full.names = TRUE)
     
     image_read(file_names) %>%
       image_animate(fps = 1) %>%
       image_write("output.gif")
     width  <- session$clientData$output_anime_width
     height <- session$clientData$output_anime_height
     list(src = "output.gif",
          contentType = 'image/gif',
          width = width,
          height = height,
          alt = "This is alternate text")
     })
     action()
     
     
   }, deleteFile = TRUE)
   
   output$downloadData <- downloadHandler(
     filename = "covid.gif",
     contentType = 'image/gif',
     content = function(file){
       num<-
         as.numeric(lubridate::ymd(input$z2)-lubridate::ymd(input$z1))
       num2<-
         num%/%input$x
       for (i in 1:num2) {
         date<-lubridate::ymd(input$z2)-input$x*(num2-i)
         date2<-lubridate::ymd(input$z2)-input$w-input$x*(num2-i)+1
         map<-l1(date2,date)
         mapshot(map, file =paste0("map_", formatC(i,width=2,flag="0"), ".png"))
       }
       
       
       file_names <- list.files(pattern = "map_\\d+.png$", full.names = TRUE)

       g1<-image_read(file_names) %>%
         image_animate(fps = 1)%>%
         image_write(file)


     }
     
   )
  
   
})
