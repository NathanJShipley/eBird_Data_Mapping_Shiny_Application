#==============================================================================
#           Mapping the Birding World Shiny World Data & Maps                  #
#==============================================================================
# 
# Author: Nathan J. Shipley
# 
# Version 2.0
#
# Last Updated March 28th, 2024

############## Current working version  next version 

library(shiny)
library(tmap)
library(sf)
library(leaflet) # Need for the leafletoutput for shiny 
library(dplyr)
library(stringr)
library(classInt)


## library(DT) #Will need for creating the desktop application container 

ui <- fluidPage(
  
  # JavaScript code to send message to server when browser window is closed
  tags$head(
    tags$script(
      HTML('
        $(window).on("beforeunload", function() {
          Shiny.onInputChange("window_closed", true);
        });
      ')
    )
  ),
  
  titlePanel("Birding Mapping Project"), # Obviously the title
  sidebarLayout( # Adds a sidebar panel to stick the buttons below
    sidebarPanel(
      style = "width: 75%; font-size: 12px;",
      tags$h4("Step 1.) Upload the \"MyEBirdData.csv\" File"),
      fileInput("file", "upload file",
                accept = c(".csv")),
      ##tags$hr(), ## This adds a break in between the sections 
      tags$h4("Step 2.) Choose Interactive Mapping Mode"),
      selectInput("mapping_mode_option", "Mapping Mode", 
                  choices = c("World Density Map", "Lower 48 County Map"), 
                  selected = "World Density Map"),
      ##tags$hr(), ## This adds a break in between the sections 
      tags$h4("Step 3.) Select Mapping Options"),
      tags$h6("note these options also affect the downloadable map"),
      selectInput("map_breaks_option", "A.) Map Breaks", 
                  choices = c("Discrete Categories", "Continuous", "Natural Breaks"), 
                  selected = "Discrete Categories"), # Dropdown menu for selecting map options
      
      selectInput("checklist_point_option", "B.) Include Checklist Points",
                  choices = c("No", "Yes"),
                  selected = "No"), # Dropdown menu for selecting map options
      
      selectInput("checklist_point_colors", "C.) Checklist Point Color",
                  choices = c("green", "black"),
                  selected = "green"), # Dropdown menu for selecting map options    
      
      ##tags$hr(), ## This adds a break in between the sections 
      tags$h4("Step 4.) Press the Button to Download the Map"),
      tags$h6("This step can take a few minutes"),
      selectInput("mapping_download_option", "Download Type", 
                  choices = c("World Density", "Lower 48", "Alabama", "Arizona", "Arkansas", "California", "Colorado", 
                              "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                              "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                              "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                              "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                              "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"), 
                  selected = "World Density"), # Dropdown menu for selecting map options
      downloadButton("download_map_btn", "Download Map") ## Adds a button the export the map
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.mapping_mode_option == 'World Density Map'",
        leafletOutput("interactive_world_map", height = "90vh")
      ),
      conditionalPanel(
        condition = "input.mapping_mode_option == 'Lower 48 County Map'",
        leafletOutput("interactive_usa_map", height = "90vh")
      )
    )
  )
)

####options(shiny.maxRequestSize=1000*1024^2) #30 MB is the size now, default is 5MB

# Define server logic
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2) #30 MB is the size now, default is 5MB

  # Initialize reactive values to store the selected map break option
  map_options <- reactiveValues(breaks_option = "Discrete Categories")
  checklist_point_options <- reactiveValues(breaks_option = "No")
  range_map_options <- reactiveValues(breaks_option = "World Density Map")
  checklist_point_color <- reactiveValues(breaks_option = "green")
  mapping_download_option <- reactiveValues(breaks_option = "World Density")
  
  
  # Reactive value to trigger map refresh
  refresh_map <- reactiveVal(FALSE)
  
  ############ Read in the inital data
  # Load world map data and then set STs, needs two lines becasue R is picky 
  # Set some other default settings, load some base layer data, and set up some other data
  sf_use_s2(FALSE)
  Bird_PALLETE <- c('#EDD577','#E54353') # Add up hear casue why not
  World_Map_Data <- sf::st_read("Base_Data/World_Map_Data_Shapefiles/World_Map_Data.shp")
  World_Map_Data <- World_Map_Data %>% sf::st_transform(4326)
  hexagon_grid_raster <- st_make_grid(World_Map_Data, what = "polygons", square = FALSE, cellsize = 2)
  fishnet_layer <- hexagon_grid_raster %>% st_as_sf() %>% mutate(id = row_number())
  # Read in the new USA County Level Data
  County_Map_Data <- sf::st_read("Base_Data/USA_Shape_File_v5/USA_Shape_File_v5.shp")
  County_Map_Data <- County_Map_Data %>% sf::st_transform(4326)
  County_Map_Data$GEOID <- as.integer(County_Map_Data$GEOID)
  # Also load in the new GEOID lookup to join data
  GEOID_Lookup <- read.csv("Base_Data/USA_GEOID_LOOKUP.csv")
  


  # Read CSV file
  data <- reactive({          ##  Reactive expressions in Shiny are functions that re-execute whenever their input changes.
    req(input$file)           ##  function to check if the input$csv_file object (which represents the uploaded file) is available. If it is not available (i.e., if the user has not uploaded a file), the reactive expression will not proceed further.
    read.csv(input$file$datapath)
  })
  
  ########### Do the inital data manipulations   
  # Perform data manipulations to get the main checklist data
  manipulated_dat <- reactive({
    req(data())
    # Do a bunch of sorting and filtering all at once
    manipulated <- data() %>% 
      select(-c(Taxonomic.Order, Location.ID,
                Location, Protocol, Duration..Min., All.Obs.Reported,
                Distance.Traveled..km., Area.Covered..ha., Number.of.Observers,
                Breeding.Code, Observation.Details, Checklist.Comments, ML.Catalog.Numbers,Time)) %>%
      filter(Count != 0) %>%
      mutate(Count = as.numeric(ifelse(Count == "X", 1, Count))) %>% 
      filter(!stringr::str_detect(Common.Name, ' x ')) %>%
      mutate(Bird_Name = paste(stringr::word(Scientific.Name,1),stringr::word(Scientific.Name,2))) %>%
      filter(!stringr::str_detect(Bird_Name, '/')) %>%
      filter(!stringr::str_detect(Bird_Name, '[.]')) %>%
      filter(!stringr::str_detect(Bird_Name, '[()]'))
    
    manipulated
    
    # # Do the seperate st as sf transformation here
    # manipulated_sf <- st_as_sf(manipulated, 
    #                            coords = c("Longitude", "Latitude"), 
    #                            crs = 4326, remove = F)
    # 
    # 
    # manipulated_sf
  })
  
  # Break up the above code to create two interactable objects, one that is data above and this one creating the SF object
  manipulated_sf <- reactive({
    req(manipulated_dat())
    # Do the seperate st as sf transformation here
    manipulated_sf <- st_as_sf(manipulated_dat(), 
                               coords = c("Longitude", "Latitude"), 
                               crs = 4326, remove = F)
    manipulated_sf
  })
  
  # Perform data manipulations to generate the fishnet data layer used in the world map
  fnet_data <- reactive({
    req(manipulated_sf())
    unique_species_per_raster <- st_join(manipulated_sf(), fishnet_layer) %>%
      st_drop_geometry() %>%
      group_by(id) %>%
      summarise(Species_Count = n_distinct(Bird_Name))
    # now select the layers using the previous data
    fnet_count <- left_join(fishnet_layer, unique_species_per_raster) %>%
      filter(Species_Count > 0, na.rm = TRUE)

    fnet_count
  })
  
  # Grab the raw lat and long
  global_checklist_points <- reactive({
    req(manipulated_dat())
    unique.checklists <- manipulated_dat() %>% select(c("Latitude","Longitude")) %>% distinct()
    
    unique.checklists
  })  
  
  # Now take above and convert to SF object to MAP in the very below code
  global_checklist_points_sf <- reactive({
    req(global_checklist_points())
    unique.checklists_sf <- st_as_sf(global_checklist_points(), 
                                     coords = c("Longitude", "Latitude"), 
                                     crs = 4326, remove = F)
    unique.checklists_sf
  })    
  
  
  # Get number of species per GEOID mapped
  usa_county_fill <- reactive({
    req(manipulated_sf())
    usa_county_species_number <- manipulated_sf() %>%
      st_drop_geometry() %>%
      filter(substr(State.Province,1,2) == "US") %>% # Filter to just USA
      filter(!State.Province %in% c("US-AK","US-HI")) %>% # Remove Alaksa and Hawaii
      dplyr::left_join(GEOID_Lookup, by = c("State.Province","County")) %>%
      group_by(GEOID) %>%
      summarise(Species_Count =as.numeric(n_distinct(Bird_Name)))
    # Now lets join the species count back to the county map data
    usa_county_species_map_fill <- dplyr::left_join(County_Map_Data, usa_county_species_number)
    # Return the object
    usa_county_species_map_fill
  })
  
  
  # Perform data manipulations to generate the fishnet data layer used in the world map
  usa_checklist_points <- reactive({
    req(manipulated_sf())
    
    usa_county_checklists <- manipulated_dat() %>%
      filter(substr(State.Province,1,2) == "US") %>% # Filter to just USA
      filter(!State.Province %in% c("US-AK","US-HI")) %>% # Remove Alaksa and Hawaii
      select(c("Latitude","Longitude","State.Province")) %>% distinct()
    
    usa_county_checklists
  })
  
  
  # Perform data manipulations to generate the fishnet data layer used in the world map
  usa_checklist_points_sf <- reactive({
    req(manipulated_sf())

    usa_checklist_points_sf <- st_as_sf(usa_checklist_points(), 
                                     coords = c("Longitude", "Latitude"), 
                                     crs = 4326, remove = F)

    usa_checklist_points_sf
    
  })
  
    ######################## Triggers to refresh settings 
  # Create a trigger to change map settings as the settings are changed 
  observeEvent(input$mapping_mode_option, {
    range_map_options$breaks_option <- input$mapping_mode_option
    # Set refresh_map to TRUE to trigger map refresh
    refresh_map(TRUE)
  })
  
  # Create a trigger to change map settings as the settings are changed 
  observeEvent(input$map_breaks_option, {
    map_options$breaks_option <- input$map_breaks_option
    # Set refresh_map to TRUE to trigger map refresh
    refresh_map(TRUE)
  })
  
  # Create a trigger to change map settings as the settings are changed 
  observeEvent(input$checklist_point_option, {
    checklist_point_options$breaks_option <- input$checklist_point_option
    # Set refresh_map to TRUE to trigger map refresh
    refresh_map(TRUE)
  })
  
  # Create a trigger to change map settings as the settings are changed 
  observeEvent(input$checklist_point_colors, {
    checklist_point_color$breaks_option <- input$checklist_point_colors
    # Set refresh_map to TRUE to trigger map refresh
    refresh_map(TRUE)
  })  
  
  # Create a trigger to change map settings as the settings are changed 
  observeEvent(input$mapping_download_option, {
    mapping_download_option$breaks_option <- input$mapping_download_option
    # Set refresh_map to TRUE to trigger map refresh
    refresh_map(TRUE)
  })  
  
  ##################### Render interactive maps   
  # Render interactive world map
  output$interactive_world_map <- renderLeaflet({
    if(input$mapping_mode_option == "World Density Map") {
      
      # world_map <- tm_shape(fnet_data(), crs = 4326) +
      #   tm_borders(col = "gray", lwd = .5) +
      #   tm_legend(legend.stack = "horizontal")
      # 
      # # Set up break options logic
      # if (map_options$breaks_option == "Discrete Categories") {
      #   world_map <- world_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, breaks = c(1,25,50,75,100,150,200,300) )
      # }
      # if (map_options$breaks_option == "Continuous") {
      #   world_map <- world_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, style = "cont" )
      # }
      # if (map_options$breaks_option == "Natural Breaks") {
      #   world_map <- world_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, style = "jenks", n = 7 )
      # }
      # 
      # if (checklist_point_options$breaks_option == "Yes") {
      #   world_map <- world_map + tm_shape(global_checklist_points()) + tm_dots(col = checklist_point_color$breaks_option, size = .001, legend.shape.show = FALSE)
      # }
      #
      # Set up the plotting
      ##tmap_leaflet(world_map, in.shiny = TRUE) %>% setView(lng = 0, lat = 0, zoom = 2)
      
      # So let's take this a different way I suppose
      leaflet_map <- leaflet(fnet_data()) %>%
        addTiles() %>%
        addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap') %>%  # Add desired tile provider
        addProviderTiles(providers$CartoDB.Positron, group='OpenStreetCARTO') %>%  # Add desired tile provider
        addProviderTiles(providers$Esri.NatGeoWorldMap, group='ESRI_Nat_Geo') %>%  # Add desired tile provider        
        setView(lng = 0, lat = 0, zoom = 2)  # Set initial view

      # Set up break options logic
      if (map_options$breaks_option == "Discrete Categories") {
        # Set up breaks
        custom_leaflet_breaks <- c(1,25,50,75,100,150,200,300,500)
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorBin(palette = c('#EDD577','#E54353'), domain = fnet_data()$Species_Count, bins = custom_leaflet_breaks)

        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = fnet_data(),
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer") %>%
            addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )

        }
      if (map_options$breaks_option == "Continuous") {
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorNumeric(palette = c('#EDD577','#E54353'), domain = fnet_data()$Species_Count)
        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = fnet_data(),
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer" )  %>%
          addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )
      }
      if (map_options$breaks_option == "Natural Breaks") {
        # Set up breaks
        custom_leaflet_breaks <- classInt::classIntervals(fnet_data()$Species_Count, n = 7, style = "jenks")$brks
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorBin(palette = c('#EDD577','#E54353'), domain = fnet_data()$Species_Count, bins = custom_leaflet_breaks)
        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = fnet_data(),
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer" ) %>%
          addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )
      }

      if (checklist_point_options$breaks_option == "Yes") {
        
        ### leaflet(options = leafletOptions(preferCanvas = TRUE))
        
        # Get points since I have the points as a SF object for easier mapping later
        # global_checklist_data_points <- st_as_sf(global_checklist_points()) %>%
        #   st_centroid() %>%
        #   st_coordinates() %>%
        #   as.data.frame() 
        
        ##global_checklist_data_points <- global_checklist_data_points[sample(nrow(global_checklist_data_points), 3000)]

        # global_checklist_points()
        # str(global_checklist_points())
        
        # Now lets add some clusters of points!
        leaflet_map <- leaflet_map %>%
          addCircleMarkers(
          data = global_checklist_points(),
          lng = ~Longitude,
          lat = ~Latitude,
          color = checklist_point_color$breaks_option,
          fillOpacity = 0.7,
          radius = 4,
          clusterOptions = markerClusterOptions(maxClusterRadius = 10, disableClusteringAtZoom = 5)
        )
      }
      
      # Return the map and also enable the settings to select and deselect
      leaflet_map %>% 
        addLayersControl(
          baseGroups = c('OpenStreetMap', 'OpenStreetCARTO', 'ESRI_Nat_Geo'),
          overlayGroups = "Density Layer",
          options = layersControlOptions(collapsed = FALSE)
      )
      
    }  
  })
  

  
  # Render interactive usa map
  output$interactive_usa_map <- renderLeaflet({
    if(input$mapping_mode_option == "Lower 48 County Map") {
      
      usa_county_fill_data_map <- usa_county_fill() %>%
        filter(!is.na(Species_Count))
      
      usa_map <- tm_shape(usa_county_fill_data_map, crs = 4326) +
        tm_legend(legend.stack = "horizontal")
      
      # # Set up break options logic
      # if (map_options$breaks_option == "Discrete Categories") {
      #   usa_map <- usa_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, breaks = c(1,25,50,75,100,150,200,300) )
      # }
      # if (map_options$breaks_option == "Continuous") {
      #   usa_map <- usa_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, style = "cont" )
      # }
      # if (map_options$breaks_option == "Natural Breaks") {
      #   usa_map <- usa_map + tm_fill("Species_Count", palette = PALLETE, title = "Total Species", alpha = .80, style = "jenks", n = 7 )
      # }
      # 
      # if (checklist_point_options$breaks_option == "Yes") {
      #   usa_map <- usa_map + tm_shape(usa_checklist_points()) + tm_dots(col = checklist_point_color$breaks_option, size = .01, legend.shape.show = FALSE)
      # }
      # 
      # # Set up the plotting
      # tmap_leaflet(usa_map, in.shiny = TRUE) %>% setView(lng = 0, lat = 0, zoom = 2)
      
      
      # So let's take this a different way I suppose
      leaflet_map <- leaflet(fnet_data()) %>%
        addTiles() %>%
        addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap') %>%  # Add desired tile provider
        addProviderTiles(providers$CartoDB.Positron, group='OpenStreetCARTO') %>%  # Add desired tile provider
        addProviderTiles(providers$Esri.NatGeoWorldMap, group='ESRI_Nat_Geo') %>%  # Add desired tile provider        
        setView(lng = 0, lat = 0, zoom = 2)  # Set initial view
      
      # Set up break options logic
      if (map_options$breaks_option == "Discrete Categories") {
        # Set up breaks
        custom_leaflet_breaks <- c(1,25,50,75,100,150,200,300,500)
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorBin(palette = c('#EDD577','#E54353'), domain = usa_county_fill_data_map$Species_Count, bins = custom_leaflet_breaks)
        
        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = usa_county_fill_data_map,
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer") %>%
          addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )
        
      }
      if (map_options$breaks_option == "Continuous") {
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorNumeric(palette = c('#EDD577','#E54353'), domain = usa_county_fill_data_map$Species_Count)
        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = usa_county_fill_data_map,
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer" )  %>%
          addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )
      }
      if (map_options$breaks_option == "Natural Breaks") {
        # Set up breaks
        custom_leaflet_breaks <- classInt::classIntervals(usa_county_fill_data_map$Species_Count, n = 7, style = "jenks")$brks
        # Set up Pallet 
        Bird_Leaflet_PALLETE <- colorBin(palette = c('#EDD577','#E54353'), domain = usa_county_fill_data_map$Species_Count, bins = custom_leaflet_breaks)
        # Now add the fnet data to the leafletmap
        leaflet_map <- leaflet_map %>% addPolygons(
          data = usa_county_fill_data_map,
          stroke = TRUE,
          color = "gray",
          weight = 0.5,
          fillOpacity = 0.8,
          fillColor = ~Bird_Leaflet_PALLETE(Species_Count),
          group = "Density Layer" ) %>%
          addLegend("bottomright", pal = Bird_Leaflet_PALLETE, values = ~Species_Count, title = "Species Count", labFormat = labelFormat(suffix = ""), opacity = 1 )
      }
      
      if (checklist_point_options$breaks_option == "Yes") {
        
        # Now lets add some clusters of points!
        leaflet_map <- leaflet_map %>%
          addCircleMarkers(
            data = usa_checklist_points(),
            lng = ~Longitude,
            lat = ~Latitude,
            color = checklist_point_color$breaks_option,
            fillOpacity = 0.7,
            radius = 4,
            clusterOptions = markerClusterOptions(maxClusterRadius = 10, disableClusteringAtZoom = 5)
          )
      }
      
      # Return the map and also enable the settings to select and deselect
      leaflet_map %>% 
        addLayersControl(
          baseGroups = c('OpenStreetMap', 'OpenStreetCARTO', 'ESRI_Nat_Geo'),
          overlayGroups = "Density Layer",
          options = layersControlOptions(collapsed = FALSE)
        )
      
      
    } 
  })
  
  ################# Render exportable maps 
  # Export world AND usa map as image file
  output$download_map_btn <- downloadHandler(
    filename = function() {
      if (mapping_download_option$breaks_option == "Lower 48") {
        paste("my_usa_county_birding_map_", Sys.Date(), ".png", sep = "")
      } 
      else if (mapping_download_option$breaks_option == "World Density") {
        paste("my_world_bird_density_map_", Sys.Date(), ".png", sep = "")
      } 
      else {
        paste("my_",{mapping_download_option$breaks_option}, "_county_birding_map_", Sys.Date(), ".png", sep = "")
      }
    }
    ,
    content = function(file) {
      ##refresh_map(FALSE) # Reset refresh_map to FALSE
      ##print("Content function executed.")  # Debugging statement
      ##req(refresh_map()) # Ensure that this download handler is triggered only when refresh_map is TRUE
      ##refresh_map(FALSE) # Reset refresh_map to FALSE
      
      if (mapping_download_option$breaks_option == "Lower 48") {
        
        map <- tm_shape(st_transform(usa_county_fill(), crs = 5070)) +
          tm_borders(col = "#FFFFFF", lwd = .5) + 
          tm_layout(main.title = "My United States County Birding Map", 
                    main.title.position = "center",
                    legend.position = c("right","bottom"))
        # Set up break options logic
        if (map_options$breaks_option == "Discrete Categories") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, breaks = c(1,25,50,75,100,150,200,300) )
        }
        else if (map_options$breaks_option == "Continuous") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, style = "cont" )
        }
        else if (map_options$breaks_option == "Natural Breaks") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, style = "jenks", n = 7 )
        }
        
        if (checklist_point_options$breaks_option == "Yes") {
          map <- map + tm_shape(usa_checklist_points_sf()) + tm_dots(col = checklist_point_color$breaks_option, size = .01, legend.shape.show = FALSE)
        }
        
        tmap_save(map, filename = file, width = 3400, height = 1950, asp=0)  # Adjust width and height as needed
      } 
      
      if (mapping_download_option$breaks_option == "World Density") {
        
        map <- tm_shape(World_Map_Data, crs = 4326) + tm_fill("#E6E6E6") + tm_borders(col = "darkgray", lwd = .5) +
          tm_shape(fnet_data(), crs = 4326) +
          tm_borders(col = "gray", lwd = .5) +
          tm_legend(legend.stack = "horizontal") +
          tm_layout(main.title = "My World Birding Map",
                    main.title.position = "center",
                    legend.position = c("left","bottom"),
                    legend.text.size = 1.3)
        
        # Set up break options logic
        if (map_options$breaks_option == "Discrete Categories") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, title = "Total Species", alpha = .80, breaks = c(1,25,50,75,100,150,200,300) )
        }
        if (map_options$breaks_option == "Continuous") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, title = "Total Species", alpha = .80, style = "cont" )
        }
        if (map_options$breaks_option == "Natural Breaks") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, title = "Total Species", alpha = .80, style = "jenks", n = 7 )
        }
        
        if (checklist_point_options$breaks_option == "Yes") {
          map <- map + tm_shape(global_checklist_points_sf()) + tm_dots(col = checklist_point_color$breaks_option, size = .001, legend.shape.show = FALSE)
        }
        
        tmap_save(map, filename = file, width = 4800, height = 2000, asp=0)  # Adjust width and height as needed
      }
      
      else {
        # grab the usa_county_fill() for the selected state
        usa_county_fill_state <- usa_county_fill() %>% filter(State == {mapping_download_option$breaks_option})

        # Also go and grab the checklist data for the selected state
        State_list <- read.csv("Base_Data/USA_State_List.csv")
        State_List_Selection <- State_list %>% filter(State == {mapping_download_option$breaks_option}) %>% select(Short_Text) %>% pull()
        usa_checklist_points_print <- usa_checklist_points_sf() %>% filter(State.Province == State_List_Selection)
        
        # Okay, now lets MAP
        map <- tm_shape(st_transform(usa_county_fill_state, crs = 4326)) +
          tm_borders(col = "#FFFFFF", lwd = .5) + 
          tm_layout(main.title = "My United States County Birding Map", 
                    main.title.position = "center",
                    legend.position = c("right","bottom"))
        # Set up break options logic
        if (map_options$breaks_option == "Discrete Categories") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, breaks = c(1,25,50,75,100,150,200,300) )
        }
        else if (map_options$breaks_option == "Continuous") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, style = "cont" )
        }
        else if (map_options$breaks_option == "Natural Breaks") {
          map <- map + tm_fill("Species_Count", palette = Bird_PALLETE, colorNA = "#E6E6E6", textNA = "0", title = "Total Species", alpha = .80, style = "jenks", n = 7 )
        }
        
        if (checklist_point_options$breaks_option == "Yes") {
          map <- map + tm_shape(usa_checklist_points_print) + tm_dots(col = checklist_point_color$breaks_option, size = .04, legend.shape.show = FALSE)
        }
        
        tmap_save(map, filename = file, width = 3400, height = 1950, asp=0)  # Adjust width and height as needed
        
      }
    }
  )
  
  
  
  observeEvent(input$window_closed, {
    if (input$window_closed) {
      q("no", status = 0)  # Terminate R process
    }
  }, ignoreInit = TRUE)
  
  
  
}

shinyApp(ui = ui, server = server)
