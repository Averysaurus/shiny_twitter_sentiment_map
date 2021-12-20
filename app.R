# https://github.com/PaulC91/crime-watch/blob/master/app.R
# https://rstudio.github.io/leaflet/shiny.html
# https://shiny.rstudio.com/articles/isolation.html
# https://rstudio.github.io/leaflet/popups.html

# tile selections
# https://leaflet-extras.github.io/leaflet-providers/preview/

# jitter coords
# https://stackoverflow.com/questions/36469379/multiple-markers-on-same-coordinate

# if null example
# https://shiny.rstudio.com/articles/req.html

# needs action button input.
library(shiny)
library(leaflet)
library(leaflet.extras)
library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(RColorBrewer)
library(htmltools)
library(shinyjs)

api_key = "QPpbkg7VO78Vetq4ak1iLmNv7"
api_secret_key = "Bklnhk7q6ba2evPJWJmqxF5WeK4IeGoh5QucAYFotyo5z7aMgI"
access_token = "51135752-GYoO7ROO36WetGHfmq4l5ieO2WwkUqhnPDjlaTFyj"
access_token_secret = "kYih6fgJZJA7CliUiZ4ffHTa8jChDAUgOZ0S8DyBbL5j0"

me_token <- create_token(
   app = "pubhealth_tweetmap",
   consumer_key = api_key,
   consumer_secret = api_secret_key,
   access_token = access_token,
   access_secret = access_token_secret)

afinn_dict <- read_csv("afinn_dict.csv")

ui <- bootstrapPage(
   useShinyjs(),
   tags$style(type = "text/css",
              "html, body {width:100%;height:100%}"),
   leafletOutput('mapo', width = "100%", height = "100%"),
   absolutePanel(top = 10, right =10,
                 tags$h2("Twitter Sentiment Map: Bay Area"),
                 span(textOutput("no_data"), style="color:red"),

                 textInput("keyword", "", placeholder = "Type Keyword Here.."),
                 actionButton("button", "Map it!"),
                 actionButton("button_2", "Reset Map"),
                 p(),
                 HTML(paste("This app collects geolocated tweets based on keywords<p> up to 9 characters long and evaluates their <a href = 'https://darenr.github.io/afinn/'>sentiment</a>. <p>Zoom in, click on the shapes to access a tweet's content.<p>
                 Author: <a href='https://www.linkedin.com/in/averysaurus/'>Avery Richards</a> <p>"
                 ))


   ),

   selectInput("colors", "",
               rownames(subset(brewer.pal.info,
                               category %in% c("seq", "div")))
   ),
   checkboxInput("legend", "", TRUE)

)

server <- function(input, output, session){
   output$mapo <- renderLeaflet({
      leaflet() %>%
         addProviderTiles("Esri.WorldGrayCanvas") %>%
         setView(-122.2712, 37.8044, zoom = 10 ) %>%
         addResetMapButton()

   })
   observeEvent(input$button_2, {
      session$reload()
   })
   observeEvent(input$button, {
      withProgress(message = "fetching tweets from API..",
                   value = 2/5, {

                      filteredData <- reactive({

                         tw <- search_tweets(isolate(input$keyword),
                                             n = 3000, token = me_token,
                                             geocode="37.8044,-122.2712,50km",
                                             include_rts = FALSE,
                                             retryonratelimit = FALSE, lang = "en")

                         tw_geo <- lat_lng(tw, coords = c("coords_coords",
                                                          "bbox_coords", "geo_coords"))

          tw_geo_map <- tw_geo %>%
      dplyr::select(screen_name, created_at, text, lat, lng) %>%
                            filter(lat != 'NA' | lng != 'NA')

                         tw_text <- tw_geo_map %>%
                            mutate(text = str_to_lower(text)) %>%
                            mutate(text = str_remove_all(text, "http\\S+")) %>%
                            mutate(text = str_remove_all(text, "@\\w+")) %>%
                            mutate(text = str_remove_all(text, "[[:punct:]]")) %>%
                            mutate(text = str_replace_all(text, "amp", "and"))

                         tw_text <- rowid_to_column(tw_text)

                         tw_act_tweets <- tw_text %>%
                            dplyr::select(rowid, text)

                         tw_text_tokens <- tw_text %>%
                            unnest_tokens(word, text) %>%
                            anti_join(stop_words)

                         afinn_text <- tw_text_tokens %>%
                            inner_join(afinn_dict) %>%
                            unite(coords, lat:lng, sep = ",") %>%
                            group_by(coords) %>%
                            separate(coords, c("lat", "lng"), sep = ",") %>%
                            mutate(lat = as.numeric(lat)) %>%
                            mutate(lng = as.numeric(lng)) %>%
                            group_by(word, lat, lng, rowid) %>%
                            summarise(score = mean(value)) %>%
                            left_join(tw_act_tweets, by = "rowid") %>%
                            distinct(rowid, .keep_all = T) %>%
                            mutate(lag = jitter(lat, factor = .005)) %>%
                            mutate(lng = jitter(lng, factor = .005))

                      })

                      for (i in filteredData()){
                         incProgress(1/5)

                      }

                   })

      colorpal <- reactive({
         colorNumeric(input$colors, domain = c(-5, 5))
      })

      if (dim(filteredData()) == 0) {
         output$no_data <-
            renderText({"No data for that keyword, try again?"})
         delay(3000, session$reload())
      }else{

         observe({
            pal <-  colorpal()
            leafletProxy("mapo", data = filteredData()) %>%
               addCircleMarkers(fillColor= ~pal(score),
                                radius=~(score^2)*3, stroke=FALSE, weight=1,
                                fillOpacity = .2,
                                popup = ~htmlEscape(as.character(isolate(filteredData()$text))),
                                popupOptions = popupOptions(closeButton = F)) %>%
               addLabelOnlyMarkers(label = filteredData()$score,
                                   labelOptions = labelOptions(noHide = F, textOnly = T) )

         })
         observe({
            proxy <- leafletProxy("mapo", data = filteredData())
            if (input$legend) {
               pal <- colorpal()
               proxy %>% addLegend(position = "bottomleft",
                                   title = "Sentiment Score",
                                   pal = pal, values = ~score

               )
            }
         })
      }
   })

}

shinyApp(ui, server)
