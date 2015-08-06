
library(shiny)
library(dplyr)
f<-file("data/form_all.csv",encoding="big5")
x<-read.csv(f,stringsAsFactors = F)

causelist<-as.character(unique(x$主要肇因))
shinyUI(navbarPage("金門縣101~103年肇事紀錄查詢",
                   tabPanel("路段查詢",
                            titlePanel("路段查詢"),
                            textInput("roadnames","路名：",value="民生路"),
                            downloadButton('downloadData1', '下載查詢結果CSV檔'),
                            navlistPanel("查詢",
                                         tabPanel("路段一覽表",dataTableOutput(outputId="roadtable")),
                                         tabPanel("位置圖",plotOutput("plotroad")),
                                         tabPanel("主要肇因長條圖",plotOutput("roadplot1"))
                                         )
                            ),
                   tabPanel("肇因查詢",
                            titlePanel("主要肇因查詢"),
                            selectInput("cause","主要肇因：",
                                        choices=causelist,
                                        selected="未依規定讓車"),
                            downloadButton('downloadData2', '下載查詢結果CSV檔'),
                            navlistPanel("查詢",
                                         tabPanel("一覽表",dataTableOutput(outputId="causetable")),
                                         tabPanel("位置圖",plotOutput("plotcause"))
                                         )
                            ),
                   tabPanel("統計圖表",
                            titlePanel("各項統計"),
                            dateRangeInput("times","統計區間：",
                                        start=as.Date(paste(format(max(as.Date(x$日期)),format="%Y"),"-01-01",sep="")),
                                        end=max(as.Date(x$日期)),
                                        min=min(as.Date(x$日期)),
                                        max=max(as.Date(x$日期)),
                                        ),
                            #downloadButton('downloadData2', '下載查詢結果CSV檔'),
                            navlistPanel("圖表選項",
                                         tabPanel("一覽表",dataTableOutput(outputId="analysistable")),
                                         tabPanel("位置圖",plotOutput("analysismap")),
                                         tabPanel("肇因分析",plotOutput("analysiscauseplot")),
                                         tabPanel("道路型態",plotOutput("analysisroadplot")),
                                         tabPanel("肇事車種",plotOutput("analysiscarplot")),
                                         tabPanel("肇事時段",plotOutput("analysishourplot")),
                                         tabPanel("肇因街道",plotOutput("analysisstreetplot"))
                            )
                   )
                   )
)
