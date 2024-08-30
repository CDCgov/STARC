
# Load/install libraries
source("R/Libraries_Dependencies.R")



# Parameter to signify R shiny app is in use
app_run=TRUE

# Read in list of country names (manually specified)
country_names = read.csv("Inputs/countries_regions_names.csv") %>% filter(Notes != "omit")
country_names=country_names %>% dplyr::mutate(OSM_country2 = dplyr::if_else(OSM_country!="", OSM_country, tolower(Aesthetic_Names) ) )

# Function to source in RMD files
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  tempR <- tempfile( fileext = ".R")
  tempR= gsub("\\\\", "/", tempR)
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  envir <- globalenv()
  source(tempR, local = envir, verbose=T, echo = T, max.deparse.length=1e1000)
}


############
#1) Title of the application 
############
title_content<-   dashboardHeader(title="STARC Application",
                                  titleWidth = 400)

############
#2) Setting up the contents and menu items within the sidebar
############
sidebar_content<- dashboardSidebar(width="400px",
                                   skin = "light",
                                   expandOnHover=FALSE, # this makes sidebar minimized but expand upon hover, I found this annoying
                                   minified = FALSE,
                                   sidebarMenu(id = "sidebarid",
                                               menuItem("Dashboard",               # this menuItem() creates the "dashboard" space. Only operable space now but you can expand to the "Additional" space in the future if you can think of more things to add
                                                        tabName = "Dashboard", 
                                                        icon = icon("dashboard")   # the icon used to indicate the Dashboard space in side menu
                                               ),  
                                               menuItem("Additional",              # this menuItem() creates the "Additional" space. Blank for now
                                                        tabName = "Additional", 
                                                        icon=icon("cog", lib = "glyphicon") # see here for additional glyphicon icon options https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
                                               ),
                                               conditionalPanel( 'input.sidebarid == "Dashboard"',# ConditionalPanel allows for different inputs on sidebar for each menu item. Important if you want very different content in different locations within the same application  https://stackoverflow.com/questions/61299368/shiny-and-shinydashboard-how-to-display-inputs-in-sidebar-only-on-certain-tabs
                                                                 
                                                                 h3("Select Region/Country"),
                                                                 
                                                                 pickerInput( inputId="Region",#  pickerinput builds an input button. The following are options
                                                                              label="Region",
                                                                              choices=unique(country_names$OSM_Region), 
                                                                              multiple=F,
                                                                              options = list(`actions-box` = TRUE),  
                                                                              selected="africa"
                                                                 ),
                                                                 htmlOutput("Country_filt"),
                                                                 
                                                                 h3("Input Options"),
                                                                 
                                                                 selectInput("repeat_input", "Regnerate Inputs?", c("True", "False"),
                                                                             selected ="True"),
                                                                 
                                                                 pickerInput( inputId="input_runs",#  pickerinput builds an input button. The following are options
                                                                              label="What Inputs to Generate",
                                                                              choices=c("OSM Roads", "Pop Raster", "Hex Grids"), 
                                                                              multiple=T,
                                                                              options = list(`actions-box` = TRUE),  
                                                                              selected=c("OSM Roads", "Pop Raster", "Hex Grids") 
                                                                 ),
                                                                 
                                                                 selectInput("Low_rest", "Lower Raster Resolution?", c("True", "False"),
                                                                             selected ="False"),
                                                                 
                                                                 h3("Hot Spot Options (GI*)"),
                                                                 numericInput(inputId="neighbor_distance",
                                                                              label = "GI* Neighbor Distance (M)",
                                                                              value=2500),
                                                                 
                                                                 h3("Cluster Options"),
                                                                 pickerInput( inputId="strc_include",#  pickerinput builds an input button. The following are options
                                                                              label="STARC Codes to Include in Cluster",
                                                                              choices=c("S1.1","S1.2",  "S1.3",  "S1.4",  "S2.1",  "S2.2",  "S2.3",  "S2.4",  "S3.1",  "S3.2",  "S3.3", "S3.4"), 
                                                                              multiple=T,
                                                                              options = list(`actions-box` = TRUE),  
                                                                              selected=c("S1.1","S1.2",  "S1.3",  "S1.4",  "S2.1",  "S2.2",  "S2.3",  "S2.4",  "S3.1",  "S3.2",  "S3.3", "S3.4")
                                                                 ),
                                                                 numericInput(inputId="buffer",
                                                                              label = "Cluster Buffer (M)",
                                                                              value=500)
                                                                 
                                               ) # end Conditional Panel
                                   ) # end sidebarmenu
) #end dashboardSidebar


############
#3) Setting up the contents in the main body (that which is not in the sidebar)
############
main_body_content <-  dashboardBody(
  tabItem(tabName = "Dashboard",
          fluidPage(
            actionButton("run_input", "1. Generate Input Files"),
            actionButton("run_output", "2. Generate Output Files"),
            actionButton("run_maps", "3. Generate STARC Maps"),
            actionButton("run_hots_spots", "4. Generate Hot Spots"),
            actionButton("run_cluster", "5. Generate Clusters"),
            actionButton("run_cluster_maps", "6. Generate Cluster Maps")
            
          )
  )
)# end dashboardbody (this officially ends code that relates to setting up contents in the dashboard body)



####################################################################################################################################################################
#B) Setting up the UI. Incorporates the title, sidebar, and main body contents defined in ***
####################################################################################################################################################################
ui <- dashboardPage(title_content, 
                    sidebar_content, 
                    main_body_content,
                    title = "STARC Application"
)



server <- function(input, output) {
  countries_filt_df <- reactive({   
    country_names%>% 
      filter(`OSM_Region` %in% input$Region) 
  })
  
  ############  
  #####  Make the reactive system df into selectable input (this creates a selector/button in the sidebarpanel for "systems"; Ctrl+F "htmlOutput("System_filt")" so see where this button is slated to appear in the app)
  ############
  output$Country_filt <- renderUI({
    
    ctrys <- countries_filt_df()%>%
      arrange(`Aesthetic_Names`) %>%
      dplyr::select(`Aesthetic_Names`)
    Countries <- unique(ctrys)
    
    ##### Creating the Name ID filter feature. 
    pickerInput( inputId="Country",
                 label="Countries",
                 choices=Countries,
                 multiple=FALSE,
                 options = list(`actions-box` = TRUE),   # build buttons for collective selection
                 selected="Burundi"
    )
  }) 
  
  observeEvent(
    input$run_input, {
      
      
      insertUI(selector = "#run_input",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_inputs.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      
      
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        continent<<-sort(as.character(input$Region))
        osm_layer_name <<- country_names[which(country_names$Aesthetic_Names==countries),]$OSM_country2
        regen_inputs <<- as.character(input$repeat_input)
        input_runs <<- input$input_runs
        Low_rast_rest <<-input$Low_rest
        function (){
          source_rmd("R/A_Set_up_Input_Files.Rmd")
        }
      }
      
      return(source_function()())
      
      
    }
    
  ) 
  
  observeEvent(
    input$run_output, {
      insertUI(selector = "#run_output",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_outputs.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        function (){
          source_rmd("R/B_Create_STARC_Map_SHP.Rmd")
        }
        
      }
      return(source_function()())
    }
  )
  
  observeEvent(
    input$run_maps, {
      
      
      insertUI(selector = "#run_maps",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_maps.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        function (){
          source_rmd("R/C_Create_STARC_Map.Rmd")
        }
        
      }
      return(source_function()())
    }
  )
  
  
  observeEvent(
    input$run_hots_spots, {
      insertUI(selector = "#run_hots_spots",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_outputs.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        neighbor_distance<<-input$neighbor_distance
        function (){
          source_rmd("R/D_Hot_Spot_Analysis.Rmd")
        }
        
      }
      return(source_function()())
    }
  )
  
  observeEvent(
    input$run_cluster, {
      insertUI(selector = "#run_cluster",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_outputs.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        strc_include <<- input$strc_include
        buffer_size<<- input$buffer
        function (){
          source_rmd("R/E_Cluster_Analysis.Rmd")
        }
        
      }
      return(source_function()())
    }
  )
  
  
  observeEvent(
    input$run_cluster_maps, {
      insertUI(selector = "#run_cluster_maps",where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "generate_outputs.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )
      source_function <- function (){      
        countries<<- sort(as.character(input$Country))
        function (){
          source_rmd("R/F_Cluster_maps.Rmd")
        }
        
      }
      return(source_function()())
    }
  )
  
}


################################################################################################################################################################
#D) Finally: call the application 
####################################################################################################################################################################
shinyApp(ui, server)









