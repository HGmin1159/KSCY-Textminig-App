
# 어플리케이션 구동 환경 조성

library(shiny)
library(shinythemes)
library(devtools)
library(readAny)
library(DT)

title = read.any("data/title.csv",header=TRUE)[,2:4]



#어플리케이션 레이아웃 부분

ui <- navbarPage("한국청소년학술논문 분석 플랫폼", theme = shinytheme("flatly"),

# 메인 분석 레이아웃
  tabPanel("키워드 분석",

# 입력 위젯 레이아웃
  sidebarLayout(
    sidebarPanel(width=3,
      wellPanel(
      selectInput("topic", label = h3("키워드 분석 : 관심 토픽을 선택하세요"), 
                  choices = list("IT/전자공학" = 0,
                                 "경제학" = 1,
                                 "교육/학생" = 2,
                                 "국제/외교" = 3,
                                 "마케팅" = 4,
                                 "문화" = 5,
                                 "생물학" = 6,
                                 "수학" = 7,
                                 "철학/사상" = 8,
                                 "화학" = 9
                                 ), 
                  selected = 0),
      actionButton("action1", label = "검색")
              ),
      wellPanel(
      radioButtons("wanttosee", label = h3("트렌드 분석 : 관심 회차를 선택하세요"),
                   choices = list("9회차" = 9, "10회차" = 10, "11회차" = 11,"12회차" = 12), 
                   selected = 9),
      actionButton("action2", label = "검색")
    )),
# 키워드 분석 레이아웃
    mainPanel(
      tabsetPanel(
        tabPanel("키워드 분석", 
                          imageOutput("topic_picture")

                 ), 
# 트렌드 분석 레이아웃
        tabPanel("트렌드 분석",
                 imageOutput("trend")
                 )
        )
      )
    )
  ),
  
  
# 원문 검색 레이아웃
  tabPanel("원문 검색" , 
          sidebarPanel(width=3,
            selectInput("topic_search", label = h3("키워드 필터"), 
                        choices = list("전체" = 0,
                                       "문화" = "문화",
                                       "화학" = "화학",
                                       "법" = "법",
                                       "컴퓨터" ="컴퓨터" ,
                                       "생물학" = "생물학",
                                       "마케팅" = "마케팅",
                                       "외교" = "외교",
                                       "물리학" = "물리학",
                                       "교육" = "교육",
                                       "경제" = "경제"
                        ), 
                        selected = 0),
            actionButton("action3", label = "검색")
            ),
          mainPanel(
            DTOutput("result")
                   )
          )
)



# 어플리케이션 함수 부분

server = function(input,output){

  
# 키워드 분석 처리 함수
  topic_tar <- reactiveValues(topic=0)
  
  topic_anal = reactive({input$topic})
  
  observeEvent(input$action1,{
    topic_tar$topic <- topic_anal()
  })
  
  output$topic_picture = renderImage({
    filename <- normalizePath(file.path('./www',
                                        paste('토픽분석 ', topic_tar$topic , '.png', sep='')))
    list(src = filename,
         width = 900,
         height = 550)}, deleteFile = FALSE)

  
  
  # 트랜드 분석 처리 함수
  number <- reactiveValues(target=9)
  
  year_see <- reactive({input$wanttosee})
  
  observeEvent(input$action2,{
    number$target <- year_see()
  })
  
  output$trend <- renderImage({
    filename <- normalizePath(file.path('./www',
                                        paste('트랜드분석 ', number$target, ".PNG", sep='')))
    list(src = filename,
         width = 900,
         height = 550
         )}, deleteFile = FALSE)
  
  
# 원문 검색 처리 함수
  filter <- reactiveValues(topic=0)
  
  topic_name = reactive({input$topic_search})
  
  observeEvent(input$action3,{
    filter$topic <- topic_name()
  })
    
  output$result = renderDT({
    if (filter$topic == 0){result = title}
      else {result = title[title["토픽"]==filter$topic,]}
    result
  })
  
  
}

shinyApp(ui=ui,server=server)

