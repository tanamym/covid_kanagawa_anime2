
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
library(rgdal)
if (!require(rgdal)) {
  install.packages("rgdal")
  
}

webshot :: install_phantomjs()



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  data<-
    fread("kanagawa.csv", encoding="UTF-8") %>%
    mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
    select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
    filter(居住都道府県=="神奈川県")%>%
    filter(居住市区町村=="川崎市"|居住市区町村=="")%>%
    mutate(Residential_City=paste0(居住市区町村,備考))%>%
    select(-居住市区町村,-備考,-X,-Y)%>%
    rename("Fixed_Date"="確定日",
           "Hospital_Pref"="受診都道府県",
           "Residential_Pref"="居住都道府県")%>%
    filter(Residential_City!="")
  data2<-fread("kanagawa.csv", encoding="UTF-8") %>%
    mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
    select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
    filter(居住都道府県=="神奈川県")%>%
    filter(居住市区町村!="川崎市",居住市区町村!="")%>%
    rename("Residential_City"="居住市区町村")%>%
    select(-備考)%>%
    rename("Fixed_Date"="確定日",
           "Hospital_Pref"="受診都道府県",
           "Residential_Pref"="居住都道府県")
  
  patient<-
    read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
    filter(!str_detect(居住地,"管内")) %>%
    filter(発表日>="2020-12-01") %>%
    rename("Fixed_Date"="発表日","Residential_City"="居住地") %>%
    select(-年代,-性別)%>%
    mutate(Residential_City = str_replace(Residential_City,"神奈川県",""))%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))
  
  kanagawa<-read.csv("kanagawa2.csv") %>%
    select(-X,-note)%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))
  
  
  kanagawa2<-rbind(kanagawa,patient) %>%
    mutate(Hospital_Pref ="神奈川県",
           Residential_Pref="神奈川県"
    )
  
  xy<-read.csv("xy.csv") %>%
    select(-X.1)%>%
    rename("Residential_City"="居住市区町村")
  chigasaki<-
    read.csv("chigasaki.csv")%>%
    mutate(Hospital_Pref ="神奈川県",
           Residential_Pref="神奈川県"
    )%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    left_join(xy,by="Residential_City")%>%
    filter(!is.na(X))
  
  list1<-read.csv("list.csv")
  data3<-
    data%>%
    left_join(list1,by=c("Residential_City"="list"))%>%
    select(-Residential_City)%>%
    rename("Residential_City"="管内")%>%
    filter(!is.na(X))
  
  kanagawa2<-
    left_join(kanagawa2,xy,by="Residential_City") %>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    filter(!is.na(X))
  
  
  kawasaki<-
    read.csv("kawasaki.csv") %>%
    select(-X)%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    left_join(list1)%>%
    select(-note,-管内,-Residential_City)%>%
    rename("Residential_City"="list")
  
  data7<-bind_rows(data2,data3,kanagawa2,kawasaki,chigasaki)
    
    date<-
      kawasaki%>%
      data.frame()%>%
      arrange(desc(Fixed_Date))%>%
      distinct(Fixed_Date)
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
    layers <- ogrListLayers("N03-190101_14_GML/N03-19_14_190101.shp")
    # windowsの場合はencodingを指定しないと文字化けする
    Encoding(layers[1]) <- "UTF-8"
    shp <- readOGR("N03-190101_14_GML/N03-19_14_190101.shp", layer=layers[1],
                   stringsAsFactors = FALSE, encoding = "UTF-8")
    #神奈川県に絞る
    
    shp@data<-
      mutate(shp@data,N03_003=ifelse(is.na(N03_003),"",N03_003))%>%
      mutate(N03_004=ifelse(N03_003=="横浜市","横浜市",
                            ifelse(N03_003=="相模原市","相模原市",N03_004)))%>%
      mutate(N03_004=ifelse(N03_003=="川崎市",paste0(N03_003,N03_004),N03_004))
    l1=function(a,b){
       #集計
       data7.1<-data7%>%
         filter(Fixed_Date>=a,Fixed_Date<=b)%>%
         group_by(Residential_City,X,Y)%>%
         summarise(count=n())%>%
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
         data7.2@data%>%
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
                     label = paste0(data7.2@data$N03_004,round(data7.2@data$count_j,2))
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
