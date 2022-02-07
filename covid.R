if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")


####COVID-19 DATA
df = read.csv('D:/AI/github/covid-variants.csv',sep = ",")


### 1.geographical data containing coordinates (longitude and latitude)爬出經緯度

library(magrittr)
library(rvest)
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="content"]') %>%
  html_table()

iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

###2.load in our world data with geographical coordinates directly from the ggplot2 package.

library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)
df['year']<-format(as.Date(df$date),format="%Y") 
df['month']<-format(as.Date(df$date),format="%m")
df['ISO3'] <- iso_codes$ISO3[match(df$location, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]


worldMaps <- function(df, world_data, variant, year, month){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$variant == variant & df$year == year& df$month == month,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['variant'] <- rep(variant, nrow(world_data))
  world_data['year'] <- rep(year, nrow(world_data))
  world_data['month'] <- rep(month, nrow(world_data))
  #world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotdf$num_sequences[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map 幫地圖加名字
  #capt <- paste0("Source: ", ifelse(variant == "Childlessness", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    #labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    labs(fill = variant, color = variant, title = NULL, x = NULL, y = NULL) + 
    my_theme()
  
  return(g)
}

library(shiny)
library(ggiraph)
shinyApp(
  
  # Define the UI
  ui = fluidPage(
    
    # App title
    titlePanel("Covid 19 variant data"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for inputs 
      sidebarPanel(
        
        # First input: Type of data
        selectInput(inputId = "variant",
                    label = "Choose the type of variant you want to see:",
                    choices = unique(df$variant)),
        
        # Second input (choices depend on the choice for the first input)
        uiOutput("secondSelection"),
        
        # Third input (choices depend on the choice for the first and second input)
        uiOutput("thirdSelection")
        
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Hide errors
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        
        # Output: interactive world map
        girafeOutput("distPlot")
        
      )
    )
  ),
  
  # Define the server
  server = function(input, output) {
    
    # Create the interactive world map
    output$distPlot <- renderGirafe({
      ggiraph(code = print(worldMaps(df, world_data, input$variant, input$year, input$month)))
    })
    
    # Change the choices for the second selection on the basis of the input to the first selection
    output$secondSelection <- renderUI({
      choice_second <- as.list(unique(df$year[which(df$variant == input$variant & df$num_sequences>0)]))
      selectInput(inputId = "year", choices = choice_second,
                  label = "Choose the year for which you want to see the data:")
    })
    
    # Change the choices for the third selection on the basis of the input to the first and second selections
    output$thirdSelection <- renderUI({
      choice_second <- as.list(unique(df$month[which(df$variant == input$variant & df$year==input$year & df$num_sequences>0)]))
      selectInput(inputId = "month", choices = choice_second,
                  label = "Choose the month for which you want to see the data:")
    })
  },
  
  options = list(height = 600)
  
)
shinyApp(ui = ui, server = server)
