#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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
    #コロナのデータ読み込み
    #data<-fread("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", encoding="UTF-8")
    #修正済みデータの読み込み
    #data<-fread("kanagawa.csv", encoding="UTF-8")
    data<-fread("kanagawa.csv", encoding="UTF-8") %>%
      mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))
    patient<-
      read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
      filter(!str_detect(居住地,"管内")) %>%
      filter(発表日>="2020-12-01") %>%
      rename("確定日"="発表日","居住市区町村"="居住地") %>%
      mutate(年代 = str_replace(年代,"代","")) %>%
      mutate(居住市区町村 = str_replace(居住市区町村,"神奈川県",""))
    kanagawa<-read.csv("kanagawa2.csv") %>%
      select(-X,-備考)%>%
      filter(!居住市区町村%in%c("横浜市","横須賀市","相模原市","川崎市",
                          "藤沢市"))
    kanagawa2<-rbind(kanagawa,patient) %>%
      mutate(受診都道府県 ="神奈川県",
                   居住都道府県="神奈川県"
      )
    
    xy<-read.csv("xy.csv") %>%
      select(-X.1)
    
    kanagawa2<-
      left_join(kanagawa2,xy,by="居住市区町村") %>%
      mutate(確定日=as.Date(確定日))
    kawasaki<-
      read.csv("kawasaki.csv") %>%
      select(-X,番号,番号2)%>%
      mutate(確定日=as.Date(確定日))
    
    data<-bind_rows(data,kanagawa2,kawasaki)
    
    date<-
      kawasaki%>%
      data.frame()%>%
      arrange(desc(確定日))%>%
      distinct(確定日)
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
    #data$確定日 <- lubridate::mdy(data$確定日)
    data$発症日 <- lubridate::mdy(data$発症日)
    data1<-data%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住管内,居住市区町村,備考,X,Y)%>%
        filter(居住都道府県=="神奈川県")


    data2<-data%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村,備考)%>%
        filter(居住都道府県=="神奈川県")
    list1<-read.csv("list.csv")
    #無理やり結合させたいので
    #川崎市備考NAあり
    #居住市区町村NAの場合に結合
    data3<-data2%>%
        filter(居住市区町村=="")
    list1$list<-as.character(list1$list)
    data5.1<-inner_join(data3,list1,by=c("備考"="list"))
    #川崎市はNAがあるため、埋まっている部分だけ結合
    data4<-data2%>%
        filter(居住市区町村=="川崎市",備考!="")
    data5.2<-inner_join(data4,list1,by=c("備考"="list"))
    #それ以外のデータ
    #川崎市
    data5.3<-data1%>%
        filter(居住市区町村=="川崎市",備考=="")
    #居住市区町村NAでの備考のリストに当てはまらない部分
    data5.4<-data1%>%
        filter(居住市区町村=="")%>%
        anti_join(list1,by=c("備考"="list"))
    #居住市区町村が川崎市以外で""ではない市区町村
    data5.5<-data1%>%
        filter(居住市区町村!=""&居住市区町村!="川崎市")
    #川崎市NA
    data5.6<-data1%>%
      filter(居住市区町村=="川崎市",is.na(備考))
    #data5.1~data5.5まで結合
    
    data6<-bind_rows(data5.1,data5.2,data5.3,data5.4,data5.5,data5.6)
    #data5.1~data5.5まで結合
    #data6<-bind_rows(data5.1,data5.2,data5.3,data5.4,data5.5)
    #居住市区町村と備考と管内を一つにまとめたい
    #居住市区町村""
    data6.1<-data6%>%
        filter(居住市区町村=="")%>%
        mutate(居住市区町村及び管内=管内)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    #川崎市
    data6.2<-data6%>%
        filter(居住市区町村=="川崎市")%>%
        tidyr::unite(col=居住市区町村及び管内,居住市区町村,管内,sep="",remove=T)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    #川崎市以外で居住市区町村があるところ

    data6.3<-data6%>%
        filter(居住市区町村!="",居住市区町村!="川崎市")%>%
        mutate(居住市区町村及び管内=居住市区町村)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    data6.1$居住市区町村及び管内<-as.character(data6.1$居住市区町村及び管内)
    #結合
    data7<-dplyr::bind_rows(data6.1, data6.2,data6.3)

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
         filter(確定日>=a,確定日<=b)%>%
         group_by(居住市区町村及び管内,X,Y)%>%
         summarise(count=n())%>%
         filter(X>0,Y>0)
      
      
       jinko2<-left_join(data7.1,jinko,by=c("居住市区町村及び管内"="市区町村"))
       jinko3<-jinko2%>%
         mutate(count_j=count/人口*100000)%>%
         mutate(N03_004=居住市区町村及び管内)
       data7.2<-
         sp::merge(shp, jinko3,
                   by="N03_004", all=F,duplicateGeoms = TRUE)
       #色設定
       pal <- colorNumeric(palette=c("white","red"),domain=c(0,input$color), reverse=F)
       data7.2%>%
         leaflet() %>%
         fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>% 
         addProviderTiles(providers$CartoDB.Positron) %>% 
         addPolygons(fillOpacity = 1,
                     weight=1,
                     color = "#666",
                     #labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                     fillColor = ~pal(data7.2@data$count_j),
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
   output$text<-
     renderText({
       as.numeric(lubridate::ymd(input$z2)-lubridate::ymd(input$z1))%/%input$x
     })
   
})
