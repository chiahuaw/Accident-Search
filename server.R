library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(ggmap)

source("global.R")

g<-ggmap(get_map(location = c(lon = 118.375, lat = 24.445), zoom = 12, maptype = "terrain"))

thm <- function() {
  theme_gray(base_family = "STHeiti") + # 讓Mac使用者能夠顯示中文, Windows使用者應省略這行
    theme(text=element_text(size=12)) # 將字體調整至12號
}

f<-file("data/form_all.csv",encoding="big5")
x<-read.csv(f,stringsAsFactors = F)
f2<-file("data/form2orig.csv",encoding="big5")
x2<-read.csv(f,stringsAsFactors = F)

shinyServer(function(input, output) {

  
    output$roadtable<-renderDataTable({ #列表，顯示查詢路段的資料。
    x_road<-x[grepl(input$roadnames,x$道路1)==T |
                grepl(input$roadnames,x$道路2)==T | 
                grepl(input$roadnames,x$概略位置)==T,]
    x_road[,c(59,8,9,60:62,33:54,65:78)]
    })
    
  output$plotroad <- renderPlot({ #出圖，顯示查詢路段的散佈狀況
    x_road<-x[grepl(input$roadnames,x$道路1)==T |
                grepl(input$roadnames,x$道路2)==T | 
                grepl(input$roadnames,x$概略位置)==T,] #input$roadnames
    
    g+ geom_point(data = x_road, aes(x = lon, y = lat),col="red")
    
  })
  
  output$roadplot1 <- renderPlot({ #出圖，路段的肇因分析圖
    
    x_road<-x[grepl(input$roadnames,x$道路1)==T |
                grepl(input$roadnames,x$道路2)==T | 
                grepl(input$roadnames,x$概略位置)==T,] #input$roadnames
    
    ggplot(data=x_road,aes(x=主要肇因))+geom_bar()+thm()+
      theme(axis.text.x = element_text(angle=90, colour="black"),legend.position="none")
    
  })
  
  output$causetable<-renderDataTable({ #列表，顯示查詢主要肇因的資料。
    x_cause<-x[grepl(input$cause,x$主要肇因)==T,]
    x_cause[,c(59,8,9,60:62,33:54,65:78)]
  })
  
  output$plotcause <- renderPlot({ #出圖，顯示查詢主要肇因的散佈狀況
    
    x_cause<-x[grepl(input$cause,x$主要肇因)==T,]
    g+ geom_point(data = x_cause, aes(x = lon, y = lat),col="red")
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { 'road_Accident.csv'},
    content = function(file) {
      x_road<-x[grepl(input$roadnames,x$道路1)==T |
                  grepl(input$roadnames,x$道路2)==T | 
                  grepl(input$roadnames,x$概略位置)==T,]
      write.csv(x_road[,c(59,8,9,60:62,33:54,65:78)], file,row.names=FALSE,fileEncoding = "big5")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { 'cause_Accident.csv'},
    content = function(file) {
      x_cause<-x[grepl(input$cause,x$主要肇因)==T,]
      write.csv(x_cause[,c(59,8,9,60:62,33:54,65:78)], file,row.names=FALSE,fileEncoding = "big5")
    }
  )
  
  output$analysistable <-renderDataTable({ #統計區間列表。
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    x_road[,c(59,8,9,60:62,33:54,65:78)]
  })
  
  output$analysismap<- renderPlot({ #統計區間散佈圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    g+ geom_point(data = x_road, aes(x = lon, y = lat),col="red")
    
  })
  
  output$analysiscauseplot<- renderPlot({ #統計區間肇因長條圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=主要肇因))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
  output$analysisroadplot<- renderPlot({ #統計區間道路型態長條圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=道路型態))+geom_bar()+thm()
    
  })
  
  output$analysiscarplot<- renderPlot({ #統計區間車種長條圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    x_road2<-data.frame()
    for (i in 1:nrow(x_road)) {
      x_road2<-rbind(x_road2,x2[x2$處理編號==x_road$處理編號[i],])
    }
    ggplot(x_road2,aes(x=車種))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
  output$analysishourplot<- renderPlot({ #統計區間道路型態長條圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=時))+geom_bar()+thm()
    
  })
  
  output$analysisstreetplot<-renderPlot({ #統計區間街道長條圖。
    
    x_road<-x[as.Date(x$日期)>=as.Date(input$times[1]) & as.Date(x$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=街道))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
})
