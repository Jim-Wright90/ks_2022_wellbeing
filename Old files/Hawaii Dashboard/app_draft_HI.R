#
# This is a Shitty Shiny web application. You can run the application by clicking
# the 'Run App' button above.
###--------
library(shiny)
library(ggplot2)
library(ggwordcloud)

load("Output.RData")

cvar <- names(app_df)[-c(1:20,140)]
yvar <- names(app_df)[-c(1:20,140)]
fvar <- append("None",names(app_df)[-c(1:20,140)])

ui <- fluidPage(title = "Survey Data",
                

        fluidRow(column(width = 12,offset = .7,
          selectInput("Variable", label = "Select Question:", choices = yvar,selected = names(app_df)[24],size = 5,selectize = FALSE,width = "95%")
                 )),
        fluidRow(column(width = 6,
                        selectInput("Variable2", 
                                    label = "Select Comparison:",
                                    choices = cvar,names(app_df)[22],width = "90%")
                        ),
                 column(width = 6,
                        selectInput("Variable3", 
                                    label = "Select Facet:",
                                    choices = fvar,selected = "None",width = "90%"))
                 ),
        
        fluidRow(conditionalPanel(condition = "output.returnedFromFunction < 10", plotOutput("distPlot")),style='padding:10px;'),

        fluidRow(conditionalPanel(condition = "output.returnedFromFunction < 10", plotOutput("distPlot2")),style='padding:10px;'),
        
        fluidRow(conditionalPanel(condition = "output.returnedFromFunction >= 10", plotOutput("distPlot3")),style='padding:10px;'),
        
        fluidRow(conditionalPanel(condition = "output.returnedFromFunction >= 10", DT::dataTableOutput("freetext")),style='padding:10px;')
)

server <- function(input, output) {
  
    output$distPlot <- renderPlot({
      if(input$Variable3 == "None"){
        cols <- vars()
      }else {
        cols <- vars(!!sym(input$Variable3))
      }
      
      ggplot(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),], aes(x = !!sym(input$Variable), y=..prop.., fill = factor(!!sym(input$Variable2)),group = factor(!!sym(input$Variable2)))) +
        geom_bar(position = position_dodge()) + 
        guides(fill=guide_legend(title=input$Variable2))+ 
        xlab(NULL) +
        geom_text(aes(y = ((..prop..)),
                      label = scales::percent((..prop..))), 
                  stat = "count", 
                  position = position_dodge(width = 1), vjust = 1.5, size =4) +
        facet_grid(cols = cols) + 
        scale_y_continuous(labels = scales::percent,name = "Percent") +
        ggtitle(stringr::str_wrap(paste("Selected Question: ", input$Variable), 100)) +
        theme(plot.title = element_text(hjust = 0.5,size = 14))
      
    }) 
    
    output$returnedFromFunction <- reactive({
      return(as.numeric(NROW(unique(app_df[,input$Variable]))))
    })
    
    outputOptions(output, "returnedFromFunction", suspendWhenHidden = FALSE)
    
    output$distPlot2 <- renderPlot({
      if(input$Variable3 == "None"){
        cols <- vars()
      }else {
        cols <- vars(!!sym(input$Variable3))
      }
      
      ggplot(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),], aes(!!sym(input$Variable), fill = factor(!!sym(input$Variable2)))) +
        geom_bar() +
        guides(fill=guide_legend(title=input$Variable2))+ 
        xlab(NULL) +
        facet_grid(cols = cols) + 
        scale_y_continuous(name = "Count") +
        geom_text(aes(label = ..count..), 
                  stat = "count",
                  position = position_stack(vjust = 0.5), size =4)

    }) 
    
    output$distPlot3 <- renderPlot({
      if(as.numeric(NROW(unique(app_df[,input$Variable]))) >= 10){
        word_vec <- unlist(stringr::str_split(app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),input$Variable],pattern = " ")) 
        
        word_vec <- stringr::str_replace(word_vec,"\"",replacement = "")
        word_vec <- stringr::str_replace(word_vec,"[.]",replacement = "")
        word_vec <- stringr::str_replace(word_vec,",",replacement = "")
        word_vec <- tolower(word_vec)
        
        df_words <- data.frame(table(word_vec))
        df_words <- df_words[nchar(as.character(df_words$word_vec)) > 3 & df_words$Freq > 3,]
        
        
        ggplot(df_words,aes(label = word_vec, size =Freq)) +
          geom_text_wordcloud(rm_outside = TRUE)+
          scale_radius(range = c(0, 50), limits = c(0, NA)) +
          theme_minimal()
      }
      
      
      
    }) 
    
    output$freetext <- DT::renderDataTable({ if(as.numeric(NROW(unique(app_df[,input$Variable]))) >= 10){app_df[!app_df[[input$Variable]] %in% c(NA,"NA","Don't know"),input$Variable]} })  
}

# Run the application 
shinyApp(ui = ui, server = server)
