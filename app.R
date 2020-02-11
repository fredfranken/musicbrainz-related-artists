#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(dplyr)
library(shinythemes)
library(DT)

# Define UI for application that draws a histogram
ui <-
  navbarPage("Powered by MusicBrainz", collapsible=TRUE, theme = shinytheme("flatly"),
             tabPanel("Artist Search",
                      fluidPage(
                        fluidRow(
                          column(3,
                                 textInput("artist",
                                           "Artist to Search"
                                 ),
                                 submitButton("Search", icon("search"))
                          ),
                          column(9,
                                 DT::dataTableOutput("artists")
                          ))
                      )
             )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  radio <- reactive({
    if(input$artist!=""){
      #artist_to_match <- commandArgs(trailingOnly = TRUE)
      artist_to_match <- input$artist
      print(artist_to_match)
      
      #genre_filter <- read.csv("genres.csv")
      genre_filter <- read.csv("genres.csv")
      url <- paste("http://musicbrainz.org/ws/2/artist/?query=artist:", artist_to_match, sep = "")
      url <- URLencode(url)
      
      df <- httr::GET(
        url, 
        httr::add_headers(Accept = "application/json")
      )
      
      mb_content <- content(df, "parse")
      #location
      mb_content$artists[[1]]$area$name
      #location
      mb_content$artists[[1]]$area$name
      
      artist <- NULL
      tags <- NULL
      count <- NULL
      a_name <- mb_content$artists[[1]]$name
      if(length(mb_content$artists[[1]]$tags) > 0){
        for(j in 1:length(mb_content$artists[[1]]$tags)){
          t_name <- mb_content$artists[[1]]$tags[[j]]$name
          t_count <- mb_content$artists[[1]]$tags[[j]]$count
          artist <- c(artist, a_name)
          tags <- c(tags, t_name)
          count <- c(count, t_count)
        }
      }
      
      final_tags <- data.frame(artist, tags, count)
      print(final_tags)
      sub_tags_all <- subset(final_tags, count > 0)
      sub_tags <- sub_tags_all[sub_tags_all$tags %in% genre_filter$genres ,]
      print(sub_tags)
      sub_tags <- arrange(sub_tags, desc(count))
      sub_tags <- head(sub_tags, n=3)
      sub_tags <- as.character(unique(sub_tags$tags))
      print(sub_tags)
      
      
      tag_query <- paste(sub_tags, sep = ' ', collapse = ' AND ')
      tag_url <- URLencode(tag_query, reserved = TRUE)
      
      radio_url <- paste(
        "http://musicbrainz.org/ws/2/artist/?query=tag:(",
        tag_url,
        ")&fmt=json&limit=100",
        sep = ""
      )
      
      print(radio_url)
      
      df3 <- httr::GET(
        radio_url, 
        httr::add_headers(Accept = "application/json")
      )
      
      mb_content3 <- content(df3, "parse")
      
      ##Other data
      #head(mb_content3$artists[2], n=1)
      #mb_content3$artists[[2]]$area$name
      #mb_content3$artists[[2]]$`life-span`$begin
      #mb_content3$artists[[2]]$`life-span`$end
      
      radio_artist <- NULL
      radio_tags <- NULL
      radio_count <- NULL
      
      for(i in 1:length(mb_content3$artists)){
        a_name <- mb_content3$artists[[i]]$name
        if(length(mb_content3$artists[[i]]$tags) > 0){
          for(j in 1:length(mb_content3$artists[[i]]$tags)){
            t_name <- mb_content3$artists[[i]]$tags[[j]]$name
            t_count <- mb_content3$artists[[i]]$tags[[j]]$count
            radio_artist <- c(radio_artist, a_name)
            radio_tags <- c(radio_tags, t_name)
            radio_count <- c(radio_count, t_count)
          }
        }
      }
      
      radio_final1 <- data.frame(radio_artist, radio_tags, radio_count)
      radio_final <- subset(radio_final1, radio_count > 0)
      rating1 <- merge(radio_final, sub_tags_all, by.x = "radio_tags", by.y = "tags")
      rating2 <- group_by(rating1, radio_artist)
      rating3 <- summarise(rating2, count = n())
      str(rating3)
      rating4 <- arrange(rating3, desc(count))
      
      radio <- select(rating4, "Artist" = radio_artist, "Score" = count)
      radio
    }
  })
  
  output$artists <- DT::renderDataTable({
    DT::datatable(radio(),
                  rownames = FALSE,
                  extensions = list('Buttons'=NULL, 'Responsive'=TRUE),
                  options = list(
                    lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')),
                    pageLength = 25,
                    dom = 'lfBrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), 
                  escape = FALSE
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

