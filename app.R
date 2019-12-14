# Data Visualization - Final Project
# Group 5 - Matthew Bomberger, James Guymon, and Tien Trinh-Tran

#-------------------#
# Establish the Environment

library(tidyverse)
library(rgeos)
library(DT)
library(shiny)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(sp)
library(scales)
library(maptools)
library(rgdal)

#-------------------#
# Load Data

#Read data sets
parks <- read.csv("Parks_Locations_and_Features.csv", stringsAsFactors = F)
enforcement <- read.csv("Code_Enforcement_Cases.csv", stringsAsFactors = F)
facilities <- read.csv("Public_Facilities.csv", stringsAsFactors = F)
districts <- readOGR(dsn="City_Council_Districts", layer = "City_Council_Districts", stringsAsFactors = FALSE)

#-------------------#
# Prepare Data

# Create variable as sum of features
parks$Volleyball <- as.integer(parks$Volleyball)
parks$Tot_Features <- rowSums(parks[4:47], na.rm=TRUE)

# Add one for park itself
parks$Tot_Features <- parks$Tot_Features + 1

#The below is to convert parks dataset from wide to long for displaying
parks_long <- parks %>% 
  select(Park_Name,Address,Park_Type,Aqua_Feat__Pool:Water_Feature) %>%
  filter(Park_Type %in% c('Block Park','Community Park','Neighborhood Park')) %>%
  gather(Features, Value, Aqua_Feat__Pool:Water_Feature) %>%
  na.omit()
rownames(parks_long) <- c()

# Find centroids of polygons
district_centroids <- as.data.frame(coordinates(districts))
names(district_centroids) <- c("cent_longitude", "cent_latitude")
districts@data <- cbind(districts@data, district_centroids)

# Format district objects for static map display
districts_stat <- fortify(districts, region = 'Dist')
districts_stat <- merge(districts_stat, districts@data, by.x = 'id', by.y = 'Dist')

# Format files as spatial
facilities.spatial <- SpatialPointsDataFrame(coords = facilities[,c("Lon","Lat")],
                                             data = facilities,
                                             proj4string = CRS(proj4string(districts)))
parks.spatial <- SpatialPointsDataFrame(coords = parks[,c("Lon","Lat")],
                                             data = parks,
                                             proj4string = CRS(proj4string(districts)))
enforcement.spatial <- SpatialPointsDataFrame(coords = enforcement[,c("Lon","Lat")],
                                           data = enforcement,
                                           proj4string = CRS(proj4string(districts)))

# Create popups for leaflets
parks.spatial$popup <- paste("<b>",parks.spatial$Park_Name,"</b><br>",
                                  "Type: ",parks.spatial$Park_Type,"<br>",
                                  "Feature Count: ", parks.spatial$Tot_Features, sep ="")
facilities.spatial$popup <- paste("<b>",facilities.spatial$POPL_NAME,"</b><br>",
                                  "Type: ",facilities.spatial$POPL_TYPE,sep ="")

# Create spatial overlays
FacOverDist <- over(facilities.spatial, districts) %>% cbind(facilities.spatial@data)
ParksOverDist <- over(parks.spatial, districts) %>% cbind(parks.spatial@data)
EnfOverDist <- over(enforcement.spatial, districts) %>% cbind(enforcement.spatial@data)

# Set Ordered Factors for Type
FacOverDist$POPL_TYPE <- factor(FacOverDist$POPL_TYPE, ordered = TRUE)
ParksOverDist$Park_Type <- factor(ParksOverDist$Park_Type, ordered = TRUE)

# Color-blind friendly palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
FacPalette <- c("#000000", "#E69F00", "#56B4E9")
facpal <- colorFactor(palette = FacPalette, domain =facilities.spatial$POPL_TYPE)
names(FacPalette) <- levels(factor(levels(FacOverDist$POPL_TYPE)))
ParksPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
parkpal <- colorFactor(palette = ParksPalette, domain =parks.spatial$Park_Type)
names(ParksPalette) <- levels(factor(levels(ParksOverDist$Park_Type)))

# Create District Summary
DistSummary <- FacOverDist %>% 
  full_join(ParksOverDist,by='Dist') %>% 
  full_join(EnfOverDist,by='Dist') %>% 
  group_by(Dist) %>% 
  summarise(Parks=n_distinct(Park_Name),Facilities=n_distinct(POPL_NAME),Cases=n_distinct(Case_Number)) %>% 
  reshape2::melt()

#-------------------#
#BUILDING APP

#-----------------------------------------------Define UI for application-----------------------------------------------#
ui = 
    tagList(
        navbarPage(
            theme = shinytheme("readable"),
            div(strong("City of South Bend | District Services & Facilities"), 
                style = "color:blue"),
            selected = "About",

#~~~~~~~~~~~~~~~BEGIN - Tab: Overview - James Guymon~~~~~~~~~~~~~~~

tabPanel("Overview", 
          mainPanel(
            selectInput("District",
                        "Select District",
                        choices = sort(unique(DistSummary$Dist)),
                        hr()
            ),
            # dataTableOutput("districtOverview"), #dataTable option
            tableOutput("districtOverview"),
            width=3
                       
          ),#end mainPanel
          sidebarPanel(
            selectInput("variable",
                        "Select Resource",
                        choices = sort(unique(DistSummary$variable)),
                        hr()
            ),
            plotOutput("barOverview"),
            width=5
          )#end sidebarPanel
),#end tabPanel

#~~~~~~~~~~~~~~~END - Tab: Overview~~~~~~~~~~~~~~~             


#~~~~~~~~~~~~~~~BEGIN - Tab: Parks & Facilities - Matt Bomberger~~~~~~~~~~~~~~~

  # Side Panel for selection of district and identification of selected district relative to city

        tabPanel("Parks & Facilities",
                 sidebarPanel(
                   # Select district
                     selectInput("city_district", 
                                 "District", 
                                 choices = sort(unique(districts_stat$id)), 
                                 selected = min(districts_stat$id)
                                 ),
                     # Plot selected district relative to other districst in city
                     plotOutput("citymap"),
                     width = 3
                 ),#end sidebarPanel Navbar 1

  # Main panel with two tabs - one for Parks and one for City Facilities
  
                 mainPanel(
                     tabsetPanel(
                       
    # Park panel allows multi select of park types, display map of locations, and 
    # bar chart for comparative number of parks
                       
                         tabPanel("Parks",
                                  # Selection of park type (default all)
                                  selectInput("park_type", 
                                              "Park Types", 
                                              choices = sort(unique(parks$Park_Type)),
                                              selected = sort(unique(parks$Park_Type)),
                                              multiple = TRUE
                                              ),
                                  # Leaflet map for location of parks
                                  leafletOutput("distleafP"),
                                  # Plot of park counts by type for all districts (selected district bold)
                                  plotOutput("parkbar")
                                  ),#end tabPanel - Parks
                         
    # Facilities panel allows multi select of facilities types, display map of locations, and 
    # bar chart for comparative number of facilities
                         
                         tabPanel("Facilities",
                                  # Multi selection for Facility Type
                                  selectInput("fac_type", 
                                              "Facility Types", 
                                              choices = sort(unique(facilities$POPL_TYPE)),
                                              selected = sort(unique(facilities$POPL_TYPE)),
                                              multiple = TRUE
                                              ),
                                  # Leaflet map for facilties location
                                  leafletOutput("distmapF"),
                                  # Plot of facilities counts by type for all districts
                                  plotOutput("facbar")
                                  )#end tabPanel - Facilities
                     ) #end of tabsetPanel
                 )#end mainPanel
        ),#end tabPanel

#~~~~~~~~~~~~~~~END - Tab: Parks & Facilities~~~~~~~~~~~~~~~ 


#~~~~~~~~~~~~~~~BEGIN - Tab: Code Enforcement - Tien Trinh-Tran~~~~~~~~~~~~~~~

tabPanel("Code Enforcement",
         #  Main panel with two tabs - one for Statistics and one for map display of locations 
         mainPanel(
           tabsetPanel(
             tabPanel("Statistics",
                      fluidPage(
                        sidebarLayout(      
                          # bar chart for comparative number of Cases by Violation Types
                          sidebarPanel(
                            selectInput("case_type", "Case Type:", 
                                        choices=unique(enforcement$Case_Type_Code_Description),
                                        selected = "ENVIRONMENTAL MOWING"),
                            hr(),
                            plotOutput("num_cases"),
                            width = 7
                          ),
                          # bar chart for comparative percentage of Closed vs. Active Cases
                          mainPanel(
                            plotOutput("avg_closed_active"),
                            width = 5
                          ),
                          position = c("left", "right"),
                          fluid = TRUE
                        )
                      )
             ),#end tabPanel - Statistics
             # This panel display map of locations, either by Closed or Active Cases
             tabPanel("Violations GIS Representations", 
                      fluidPage(selectInput(inputId = "case_type_map", 
                                            label = "Case Type:",
                                            choices = unique(enforcement$Case_Type_Code_Description)),
                                radioButtons("status", "Case Status:",
                                             c("Closed" = "Closed",
                                               "Active" = "Active")),
                                # Leaflet map for location of Cases
                                tabsetPanel(leafletOutput(outputId = "map")
                                )#end TabsetPanel          
                      )#end fluidPage
             )#end tabPanel - Map                              
           )#end tabsetPanel
         )#end mainPanel
),#end tabPanel

#~~~~~~~~~~~~~~~END - Tab: Code Enforcement~~~~~~~~~~~~~~~ 


#~~~~~~~~~~~~~~~BEGIN - Tab: Data - Tien Trinh-Tran~~~~~~~~~~~~~~~

tabPanel("Data", width = 3,
         sidebarLayout(
           # dropdown bar for selection of the dataset want to see
           sidebarPanel(selectInput("dataset", "Choose a dataset:",
                                    choices = c("Public Facilites",
                                                "Enforcement Cases",
                                                "Parks"),
                                    selected = "Parks"),
                        # option to download the dataset to local
                        downloadButton("downloadData", "Download")),
           mainPanel(dataTableOutput("tablex"), width = 9)
         )#end sidebarLayout
),#end tabPanel        

#~~~~~~~~~~~~~~~END - Tab: Data - Tien Trinh-Tran~~~~~~~~~~~~~~~ 


#~~~~~~~~~~~~~~~BEGIN - Tab: About - Tien Trinh-Tran~~~~~~~~~~~~~~~  

tabPanel("About", 
         # sidePanel to show the navigating instruction
         sidebarPanel(strong("Navigation Guide:"), br(), br(), 
                      div(strong(em("Overview")),("displays summary statistics of various amenities and services by district, in the 
                          form of spatial maps and various statistical visualizations")),br(),
                      div(strong(em("Parks & Facilities")), ("displays detailed statistics for the selected City District and 
                          allow the audience to explore the location of parks and public facilities within these districts")),br(),
                      div(strong(em("Code Enforcement")), ("displays summary statistics of Code violation types,
                          Inspection status and spatial map")),br(),
                      div(strong(em("Data")), ("shows raw data sets used."))                      
         ),#end sidebarPanel 1 
         
         # provide the goal of building this web app and list of team members 
         mainPanel(strong("Background:"), 
                   br(),
                   div("This Dashboard is to provide an interactive tool for our audience to explore 
                       the availability and utilization of City of South Bend maintained facilities and services, 
                       specifically in the form of location and type of Parks and Public Facilities and utilization 
                       of Code Enforcement services, broken out by City Council District.  
                       This helps understand what is available in each District, answer 
                       questions about service utilization, potentially identify under and over-served areas.")
         ), # end mainPanel
         
         mainPanel(br(),strong("Team members:"), br(),
                   div("James Guymon"),
                   div("Matthew Bomberger"),
                   div("Tien Thuy Trinh-Tran")
         )#end mainPanel 2
)#end tabPanel

#~~~~~~~~~~~~~~~END - Tab: About~~~~~~~~~~~~~~~

    )
)

#-----------------------------------------------Define server logic-----------------------------------------------#
server = function(input, output) {

#~~~~~~~~~~~~~~~BEGIN Output - Tab: Overview - James Guymon~~~~~~~~~~~~~~~   
  #Create a graph filtered to just the feature type on the overview tsb.
    output$barOverview <- renderPlot(
      {
        ggplot(DistSummary %>% filter(variable==input$variable), aes(x=Dist, y=value)) + stat_summary(fun.y="mean", geom="bar")+labs(x="District",y="Resource Count") +
          theme_bw()+ggtitle("Resource Allocation By District")
        
      })
    #Create a dynamic data set for the output table based off of what is selected.
    overviewInput <- reactive({
      DistSummary %>% filter(Dist==input$District) %>% select(variable,value) %>% rename(Resource=variable, Quantity=value)
    })
    
  #output the dynamic table created above.
    
    output$districtOverview<-renderTable({
      overviewInput()
    })  
    
#~~~~~~~~~~~~~~~END Output - Tab: Overview~~~~~~~~~~~~~~~    

      
#~~~~~~~~~~~~~~~BEGIN Output - Tab: Parks & Facilities - Matt Bomberger~~~~~~~~~~~~~~~
    
    
    # Leaflet map of parks within the selected district
    
    output$distleafP <- renderLeaflet(
        {
            leaflet(subset(districts, districts$Dist == input$city_district))  %>%
            addTiles()  %>%
            # Display district boundaries and highlight dirtsict area
            addPolygons(color = "#18659B", 
                        weight = 1, 
                        smoothFactor = 0.5,
                        opacity = 1.0, 
                        fillOpacity = 0.1) %>% 
            # Use circle markers for parks with color palette for park type matching bar chart
            addCircleMarkers(data = subset(ParksOverDist,
                                ParksOverDist$Dist == input$city_district & ParksOverDist$Park_Type %in% input$park_type),
                             color = ~parkpal(Park_Type),
                             stroke = 0, 
                             fillOpacity = 1, 
                             radius = 8, 
                             popup = ~popup)

            }
        )
    
    
    # Bar chart of parks across district with stacked bar of park type

    output$parkbar <- renderPlot(
      {
        ggplot() + 
          geom_bar(data = subset(ParksOverDist, 
                          ParksOverDist$Park_Type %in% input$park_type & !is.na(ParksOverDist$Dist)), 
                       aes(x= Dist, 
                           y = Tot_Features,
                           alpha = ifelse(Dist == input$city_district, NA, 1),
                           fill = Park_Type),
                   stat = "sum") + 
          scale_fill_manual(values=ParksPalette) +
          guides(alpha=FALSE, size=FALSE) +
          theme_minimal() +
          labs(title = "Park Features by City District", 
               x = "City Distict", 
               y = "Count of Park Features (incl. park)")
      }
    )
    
    
    # Leaflet map of facilities within the selected district
    
      output$distmapF <- renderLeaflet(
      {
        leaflet(subset(districts, districts$Dist == input$city_district))  %>%
          addTiles()  %>%
          # Display district boundaries and highlight dirtsict area
          addPolygons(color = "#18659B", 
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 1.0, 
                      fillOpacity = 0.1) %>% 
          # Use circle markers with color for facility type matching stacked bar chart
          addCircleMarkers(data = subset(FacOverDist, 
                                FacOverDist$Dist == input$city_district & FacOverDist$POPL_TYPE %in% input$fac_type),
                      color = ~facpal(POPL_TYPE),
                      stroke = 0, 
                      fillOpacity = 1, 
                      radius = 8, 
                      popup = ~popup)
        }
      )


      # Bar chart of facilities across district with stacked bar of facility type
      
      output$facbar <- renderPlot(
        {
          ggplot() + 
            geom_bar(data = subset(FacOverDist, 
                                   FacOverDist$POPL_TYPE %in% input$fac_type & !is.na(FacOverDist$Dist)), 
                     aes(x= Dist, 
                         alpha = ifelse(Dist == input$city_district, NA, 1),
                         fill = POPL_TYPE),
                     stat = "count") + 
            scale_fill_manual(values=FacPalette) +
            guides(alpha=FALSE) +
            theme_minimal() +
            labs(title = "Facilities by City Districts", x = "City District", y = "Number of Facilities")
        }
      )
      
      
      # Map highlighting which district is selected in city for easy reference

      output$citymap <- renderPlot(
        {
            ggplot() + 
            geom_polygon(data = districts_stat, 
                         aes(x=long, 
                             y = lat,
                             fill = ifelse(id == input$city_district, 1, NA),
                             group = group),
                         color = "black") + 
            coord_quickmap() +
            geom_text(data = districts_stat, aes(label = id, x = cent_longitude, y = cent_latitude)) + 
            theme_minimal() +
            labs(title = "City of South Bend Districts", x = "", y = "") +
            theme(legend.position = "none", 
                  axis.title.x = element_blank(), 
                  axis.text.x = element_blank(), 
                  axis.ticks.x = element_blank(), 
                  axis.title.y = element_blank(), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(), 
                  panel.border = element_rect(colour = "black", 
                                              fill=NA, 
                                              size=1))
            }
        )
      
#~~~~~~~~~~~~~~~END Output - Tab: Parks & Facilities~~~~~~~~~~~~~~~

  
#~~~~~~~~~~~~~~~BEGIN Output - Tab: Code Enforcement - Tien Trinh-Tran~~~~~~~~~~~~~~~        
      
      output$num_cases <- renderPlot(
        {
          # Bar chart for comparative number of Cases by Violation Types
          ggplot(enforcement, aes(x=Case_Type_Code_Description,
                                  fill = ifelse(Case_Type_Code_Description == input$case_type, "#0072B2", "grey"),
                                  stat = "count")) +
            geom_bar() +
            scale_fill_identity(guide = 'none') +
            theme_bw() +
            labs(x="Case Type",y="Number of cases") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
      )
      
      output$avg_closed_active <- renderPlot(
        {
          # Bar chart for comparative percentage of Closed vs. Active Cases
          ggplot(enforcement %>% filter(Case_Type_Code_Description==input$case_type), aes(x=Case_Status_Code_Description, 
                                                                                          fill=Case_Status_Code_Description)) +
            geom_bar(aes(y = (..count..)/sum(..count..))) +
            geom_text(aes(y = ((..count..)/sum(..count..)), 
                          label = percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
            scale_y_continuous(labels = percent) +
            theme_bw() +
            scale_fill_manual(values=cbbPalette, guide = 'none') +
            labs(x="Case Status",y="Percentage") +
            theme(axis.text.y=element_blank(), 
                  axis.ticks=element_blank(),
                  axis.title.y=element_blank()
            )
        })
      
      output$map <-  renderLeaflet({
        # Leaflet map of cases 
       leaflet(data= enforcement %>% 
                filter(Case_Type_Code_Description==input$case_type_map,
                         Case_Status_Code_Description==input$status)) %>%
          addTiles()%>%
          addMarkers(~Lon, ~Lat, label = ~paste("Zip Code: ",as.character(Zip_Code)), 
                     popup = ~paste("Status: ", Case_Status_Code, "/ Address: ",Street_Address),
                     clusterOptions = markerClusterOptions()) 
      })    
      
#~~~~~~~~~~~~~~~END Output - Tab: Code Enforcement~~~~~~~~~~~~~~~        
      

#~~~~~~~~~~~~~~~BEGIN Output - Tab: Data - Tien Trinh-Tran~~~~~~~~~~~~~~~        
      
      datasetInput <- reactive({
        switch(input$dataset,
               "Public Facilites" = facilities %>% select(-c(Lat, Lon)),
               "Enforcement Cases" = enforcement %>% select(-c(Lat, Lon)),
               "Parks" = parks_long)
      })
      
      output$tablex <- renderDataTable({
        datatable(datasetInput(), options = list(lengthMenu = c(5, 10), pageLength = 5))
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
      
#~~~~~~~~~~~~~~~END Output - Tab: Data~~~~~~~~~~~~~~~  
    
}

#-----------------------------------------------Run the application-----------------------------------------------# 

shinyApp(ui = ui, server = server)
