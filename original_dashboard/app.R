#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(ggplot2)
library(ggwordcloud)
library(plotly)
library(RColorBrewer)
library(paletteer)
library(knitr)

load("Output.RData")

cvar <- append("None",names(app_df)[-c(1:12,16:122)])
yvar <- names(app_df)[-c(1:12)]
fvar <- append("None",names(app_df)[-c(1:12,16:122)])

ui <- tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(
    
    title = div(img(src='imi-pono-title-graphic.png',style="margin-top: -10px; padding-right:10px;padding-bottom:17px", height = 60)),
    windowTitle = "‘Imi Pono Hawaiʻi Wellbeing Survey",
    tabPanel("Results",
             fluidRow(column(width = 12,offset = .7,
                             selectInput("Variable", label = "Select Question:", choices = yvar,selected = yvar[4],size = 5,selectize = FALSE,width = "95%")
             )),
             fluidRow(column(width = 6,
                             selectInput("Variable2", 
                                         label = "Primary Comparison:",
                                         choices = cvar,cvar[3],
                                         width = "90%"
                             )
             ),
             column(width = 6,
                    selectInput("Variable3", 
                                label = "Secondary Comparison:",
                                choices = fvar,selected = "None",
                                width = "90%")
             )
             ),
             
             fluidRow(conditionalPanel(condition = "output.returnedFromFunction < 10", plotOutput("distPlot")),style='padding:10px;'),
             
             fluidRow(column(width = 12, conditionalPanel(condition = "output.returnedFromFunction < 10", plotlyOutput("distPlot2"))),
                      style='padding:10px;'),
             
             fluidRow(conditionalPanel(condition = "output.returnedFromFunction >= 10", plotOutput("distPlot3")),style='padding:10px;')),
    
    tabPanel("About",
             includeHTML("RMarkdownFile.html")
    )
  ))

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$Variable3 == "None"){
      cols <- vars()
    }else {
      cols <- vars(!!sym(input$Variable3))
    }
    if(input$Variable2 == "None"){
      ggplot(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),], 
             aes(x = !!sym(input$Variable), 
                 y= (..count..)/sum(..count..))) +
        geom_bar(position = position_dodge(), fill="#0097b1") + 
        xlab("Question Response") +
        geom_text(aes(y = ((..count..)/sum(..count..)),
                      label = scales::percent(((..count..)/sum(..count..)), 2)), 
                  stat = "count", 
                  position = position_dodge(width = 1), vjust = 1.5, size =4) +
        facet_grid(cols = cols) + 
        scale_y_continuous(labels = scales::percent,name = "Percent") +
        ggtitle(stringr::str_wrap(paste("Selected Question: ", input$Variable), 100)) +
        theme(plot.title = element_text(hjust = 0.5,size = 14)) + 
        #scale_fill_paletteer_d("ggsci::category10_d3") +
        scale_fill_manual(values = c("#a1a75f","#0097b1","#7c6553","#a6286e","#e0b763"))
    }else {
      ggplot(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),], 
             aes(x = !!sym(input$Variable), 
                 y=..prop.., 
                 fill = factor(!!sym(input$Variable2)),
                 group = factor(!!sym(input$Variable2)))) +
        geom_bar(position = position_dodge()) + 
        guides(fill=guide_legend(title=input$Variable2))+ 
        xlab("Question Response") +
        geom_text(aes(y = ((..prop..)),
                      label = scales::percent((..prop..))), 
                  stat = "count", 
                  position = position_dodge(width = 1), vjust = 1.5, size =4) +
        facet_grid(cols = cols) + 
        scale_y_continuous(labels = scales::percent,name = "Percent") +
        ggtitle(stringr::str_wrap(paste("Selected Question: ", input$Variable), 100)) +
        theme(plot.title = element_text(hjust = 0.5,size = 14)) + 
        #scale_fill_paletteer_d("ggsci::category10_d3")
        scale_fill_manual(values = c("#a1a75f","#0097b1","#7c6553","#e0b763","#a6286e"))
    }
  }) 
  
  output$returnedFromFunction <- reactive({
    return(as.numeric(NROW(unique(app_df[,input$Variable]))))
  })
  
  outputOptions(output, "returnedFromFunction", suspendWhenHidden = FALSE)
  
  output$distPlot2 <- renderPlotly({
    if(as.numeric(NROW(unique(app_df[,input$Variable]))) < 10){
      df <- app_df[app_df$`Q2 Are you Native Hawaiian?` == "Yes"& !app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),]
      
      percs <- df %>%
        group_by(df[[input$Variable]]) %>%
        tally() %>%
        mutate(Percentage = n/sum(n))
      df2 <- app_df[app_df$`Q2 Are you Native Hawaiian?` == "No"& !app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),]
      
      percs2 <- df2 %>%
        group_by(df2[[input$Variable]]) %>%
        tally() %>%
        mutate(Percentage = n/sum(n))
      
      fig <- plot_ly(sort = FALSE)
      fig <- fig %>% add_pie(data = percs, labels = ~percs[[1]], values = ~n, domain = list(row = 0, column = 0), marker = list(colors = c("#a1a75f","#0097b1","#7c6553","#e0b763","#a6286e")))
      fig <- fig %>% add_pie(data = percs2, labels = ~percs2[[1]], values = ~n, domain = list(row = 0, column = 1))
      fig <- fig %>% layout(grid=list(rows=1, columns=2),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend=list(y=0,title=list(text='<b> Question Response </b>')),
                            annotations = list(
                              list(
                                x = 0.225, 
                                y = 1.0, 
                                font = list(size = 16), 
                                text = "Native Hawaiian", 
                                xref = "paper", 
                                yref = "paper", 
                                xanchor = "center", 
                                yanchor = "bottom", 
                                showarrow = FALSE
                              ), 
                              list(
                                x = 0.775, 
                                y = 1.0, 
                                font = list(size = 16), 
                                text = "Non-Hawaiian", 
                                xref = "paper", 
                                yref = "paper", 
                                xanchor = "center", 
                                yanchor = "bottom", 
                                showarrow = FALSE
                              )))
      return(fig)
    }
  }) 
  
  output$distPlot3 <- renderPlot({
    if(as.numeric(NROW(unique(app_df[,input$Variable]))) >= 10){
      word_vec <- unlist(stringr::str_split(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),input$Variable],pattern = " ")) 
      word_vec <- stringr::str_replace(word_vec,"\"",replacement = "")
      word_vec <- stringr::str_replace(word_vec,"[.]",replacement = "")
      word_vec <- stringr::str_replace(word_vec,",",replacement = "")
      word_vec <- tolower(word_vec)
      word_vec <- word_vec[!word_vec %in% stopwords]
      df_words <- data.frame(table(word_vec))
      df_words <- df_words[nchar(as.character(df_words$word_vec)) > 3 & df_words$Freq > 3,]
      if(NROW(df_words) > 1){
        ggplot(df_words,aes(label = word_vec, size =Freq)) +
          geom_text_wordcloud(rm_outside = TRUE)+
          scale_radius(range = c(0, 60), limits = c(0, NA)) +
          theme_minimal()
      }
      
      
    }
    
    
    
  }) 
  
  output$freetext <- DT::renderDataTable({ if(as.numeric(NROW(unique(app_df[,input$Variable]))) >= 10){app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),input$Variable]} })  
}

# Run the application 
shinyApp(ui = ui, server = server)
