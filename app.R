#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Loading packages --------------------------------------------------------

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(Hmisc)
library(ggplot2)
library(scales)
library(plotly)
library(stringr)

# Pre-processing ----------------------------------------------------------


question <- read_csv("Data/columns.csv")
data <- read_csv("Data/responses.csv")

data %>% group_by(`Internet usage`) %>% summarise(count=n())
data %<>% filter(!`Internet usage` %in% c("no time at all"))
data$`Internet usage` %<>% capitalize()
unique(data$`Internet usage`)
data %<>% as.data.frame()
data$`Internet usage` <- factor(data$`Internet usage`,
                                ordered = TRUE,
                                levels = c("Less than an hour a day",
                                           "Few hours a day",
                                           "Most of the day"))
unique(data$`Internet usage`)

varnames <- colnames(data)
number = c(133,76,78,80,81,91,96,100,102,103,114,121,123,124,125,129,134,137,138,139,140)
selvar <- varnames[number]
selvar
df <- data %>% select(selvar)
colnames(df)[[1]] <- "Internet"
df <- as.data.frame(df[complete.cases(df),])
colSums(is.na(df))

fullquestion <- as.list(question[number,1])[[1]]
length(fullquestion)
fullquestion[[18]] <- "I spend a lot of money on partying and socializing."
fullquestion[[21]] <- "I will happily pay more money for good, quality or healthy food."
fullquestion

meandf <- df %>% 
  group_by(Internet) %>% 
  summarise_at(vars(-group_cols()),mean)



meandf <- as.data.frame(meandf)
colnames(meandf) <- fullquestion
colnames(meandf)
colnames(meandf)[[1]] <- "Internet"
meandf %<>% gather(Question, MeanScore,2:21)
meandf$Question <- as.factor(meandf$Question)
levels(meandf$Question)


meanfilt <- meandf %>% filter(Question %in% "I often study or work even in my spare time.")
meanfilt$Question <- droplevels(meanfilt$Question)
meanfilt


# Design app --------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(tags$style('p {color:grey;}', ".selectize-input {
  max-height: 80px;
  overflow-y: auto;
  background: ghostwhite;
}")),

    # Application title
    titlePanel("Do young people who spend different amount of time on the internet have different lifestyles?"),
    p("Explore the difference in lifestyles of young people who have different internet usage from a survey completed by 1010 Slovakians aged 15 to 30 in 2013"),
    br(),
    fluidRow(

      tabsetPanel(type= "tabs",
                  tabPanel("Plot",
  
      # Sidebar with a slider input for number of bins 
          column(4,
            radioButtons("dataoption", "Choose a way to explore the data",
                         choices = c("Answer distribution of each statement","Compare average answers"),
                         inline = TRUE) ),
          column(8,
            conditionalPanel(
              condition = "input.dataoption == 'Compare average answers'",
              selectizeInput(
                "selectquestion",
                label = "Select survey statements (max = 5):",
                choices = c(levels(meandf$Question)),
                multiple = TRUE,
                options = list(maxItems = 5,
                               placeholder = "Select multiple statements to compare their average answers"),
                width = "850px",
                size = 3
              )
            )),
        column(4,
             conditionalPanel(
               condition = "input.dataoption == 'Answer distribution of each statement'",
               selectizeInput(
                 "questionforfull",
                 label = "Select a survey statement:",
                 choices = c(levels(meandf$Question)),
                 multiple = TRUE,
                 selected = NULL,
                 options = list(maxItems = 1, placeholder = "Select a statement you want to explore"),
                 width="500px"
               )
             )),
          column(4,
            conditionalPanel(
              condition = "input.dataoption == 'Answer distribution of each statement'",
              radioButtons("samplesize", "Choose the data size:",
                           choices = c("Show all data", "Show 100 random samples from each group"),
                           inline = TRUE)
            ),
            conditionalPanel(
              condition = "input.dataoption == 'Answer distribution of each statement'&input.samplesize == 'Show 100 random samples from each group'",
              actionButton("samplebutton","Generate samples"),
              p("Press the button again to generate another set of random samples")
            )),
          column(12,hr(), align="center",
                 plotOutput('dotplot',height = "650px",width="85%")
                 )),
      tabPanel("References",
        column(12,
               hr(),
               h4("References"),
               p("Sabo, M. (n.d.). Young People Survey. Kaggle. Retrieved June 24, May, from https://www.kaggle.com/datasets/miroslavsabo/young-people-survey"))
        )
      )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  meanfilt <- reactive({
    meanfilt <- meandf %>% filter(Question %in% input$selectquestion)
    meanfilt$Question <- droplevels(meanfilt$Question)
    meanfilt
  })
  colnames(df) <- fullquestion
  
  

  output$dotplot <- renderPlot({

    if (input$dataoption == 'Compare average answers') {
    validate(need(input$selectquestion, 'Please select statements to display on graph!'))
    g <- ggplot(meanfilt(), aes(y=Question,x=MeanScore,group=Internet,color=Internet))
    g + geom_point(size=4)+
      labs(title="Average answer of each Internet Usage group on statements about their lifestyle",
           x="Average Answer",
           y="Statement")  +
      scale_x_continuous(limits = c(1,5),
                         breaks = c(1,2,3,4,5),
                         labels = c("1\nStrongly Disagree", 2,3,4,"5\nStrongly Agree")) +
      scale_color_manual(values =c("#F5B7B1","#E74C3C","#943126")) +
      scale_y_discrete(labels = label_wrap(30))  +
      theme(title = element_text(size=18,face="bold"),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=15,face="bold"),
            axis.title.y = element_text(size=15,face="bold"),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15,face="bold"),
            panel.background = element_rect(fill = "white",colour = "black"),
            panel.grid = element_line(colour = "#e0e0e0")) 
    
    } else if (input$dataoption == "Answer distribution of each statement") {
      validate(need(input$questionforfull, 'Please select a statement to display on graph!'))
      chosenq <- df %>% select("How much time do you spend online?",input$questionforfull)
      sampledf <- eventReactive(input$samplebutton,
                                {as.data.frame(slice_sample(group_by(chosenq,chosenq[,1]),n=100))})
      if (input$samplesize == "Show all data") {
        p <- ggplot(chosenq, aes(y=chosenq[,1],x=chosenq[,2]))
        #p + geom_jitter(width = .3,height=.3, alpha = .5) +
        p + geom_boxplot(alpha=0.4,width=0.5) +
          scale_x_continuous(limits = c(1,5),
                             breaks = c(1,2,3,4,5),
                             labels = c("1\nStrongly Disagree", 2,3,4,"5\nStrongly Agree"))+
          labs(
               y="Internet Usage",
               title=colnames(chosenq)[[2]],
               x="Answer") +
          stat_summary(fun="mean",geom = "point", color="red",size=3) +
          theme(
            title = element_text(size=18,face="bold"),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=15,face="bold"),
            axis.title.y = element_text(size=15,face="bold"),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15,face="bold"),
            panel.background = element_rect(fill = "white",colour = "black"),
            panel.grid = element_line(colour = "#e0e0e0"),
            legend.position="top"
          ) 
      } else if (input$samplesize == "Show 100 random samples from each group") {
        
        b <- ggplot(sampledf(), aes(y=sampledf()[,1],x=sampledf()[,2]))
        b + geom_boxplot(alpha=0.4,width=0.5) +
        #b + geom_jitter(width = .2,height=.3, alpha = .5) +
          scale_x_continuous(limits = c(1,5),
                             breaks = c(1,2,3,4,5),
                             labels = c("1\nStrongly Disagree", 2,3,4,"5\nStrongly Agree"))+
          labs(y="Internet Usage",
               title=paste(colnames(chosenq)[[2]],"(Total sample size: 300)"),
               x="Answer") +
          stat_summary(fun="mean",geom = "point", color="red",size=3) +
          theme(title = element_text(size=18,face="bold"),
                axis.text.y = element_text(size=12),
                axis.text.x = element_text(size=12),
                axis.title.x = element_text(size=15,face="bold"),
                axis.title.y = element_text(size=15,face="bold"),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15,face="bold"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid = element_line(colour = "#e0e0e0"))
      }
        
      }
  })

}
  
    
  

# Run the application 
shinyApp(ui = ui, server = server)






