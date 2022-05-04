# Clean workspace
rm(list =ls())

#Set up packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(highcharter)
library(leaflet)
library(sp)
library(rgdal)
library(exactextractr)
library(readxl)
library(rgeos)
library(stringr)


## Laterite colors for graphs
laterite_1 <- c("#D6EDD8", "#7DD9BA", "#ABE1B9") # to be used for single-select graphs
laterite_2 <- c("#7DD9BA") # to be used for multiple-select graphs
laterite_3 <- c("#DA302C", "#F27317", "#241F21") # to be used for multiple-select LINE graphs (over time)
laterite_maps <- c("#7DD9BA") # to be used for bubbles in maps


#-------------------------------------------------------------------------------
# User Interface Elements-------------------------------------------------------
#-------------------------------------------------------------------------------

ui <- fluidPage(
  
#Title
tags$head(tags$title("Laterite - Geospatial Data")),
              
#Social Media Icon Colors
tags$style(".fa-facebook{color:#000000}"),
tags$style(".fa-linkedin{color:#000000}"),
tags$style(".fa-twitter{color:#000000}"),
tags$style("a{color:black; font-size:13px;}"),
tags$style(".textbox{font-size:14px; text-align:justify; border: 2px solid #DA302C; padding: 5px; box-shadow: 4px 4px 10px 2px #E8C4AA;}"),

#adding javascript code to manually unbind Shiny's inputs to the datatable's inputs (see inside the data_filtered reactive)
#https://groups.google.com/g/shiny-discuss/c/ZUMBGGl1sss/m/7sdRQecLBAAJ
tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                            Shiny.unbindAll($(document.getElementById(id)).find('.dataTable'))
                            })")),

setBackgroundColor(color = "#C5E3C6"), #background color
theme = "bootstrap_flatly.css", #css theme used
useShinydashboard(), #"activating" the shinydashboard package


## Header ----------------------------------------------------------------------

titlePanel(fluidRow(column(width=3, 
                           img(src ="laterite-logo-dark.svg", 
                               height = 70, width = 130)))),

## Body ------------------------------------------------------------------------

tabsetPanel(type = c("tabs"),
               
                  
### Tab 1 : Introduction #######################################################
tabPanel(h5(strong("Introduction")),
         
    #code for a line of whitespace
    fluidRow(style = "background-color:#FFFFFF;", br()), 

    fluidRow(style = "background-color:#FFFFFF;", 
             column(width = 6, offset = 3,
                    
                   #Welcome to the dashboard
                   h2(style = "text-align: center;", "Welcome to the Laterite geospatial data dashboard (beta)!"), 
                   
                   #horizontal line
                   hr(), 
                   
                   #Quick description of the dashboard (to remove ?)
                   tags$p(style = "font-size:16px; text-align:justify; border: 2px solid #DA302C; border-radius: 25px; padding: 20px;", 
                          "This tool will allow you to obtain data on many different geospatial variables for the four Laterite african countries. This data can be used to gain more insights on most research projects, so feel free to take a look. Geospatial data can be accessed in two ways : First, it can be accessed for specific GPS coordinates, for example ones you would get with survey data. Then, it can also be accessed for specific administrative units (e.g. regions, districts, or below) for which the resulting data is some descriptive statistics about how the geospatial variables are distributed in that unit.")))
), 



### Tab 2 : Variables ##########################################################
tabPanel(h5(strong("Variables")),
         
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  #Title : Variables description
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Variables description")))),
  
  #Instructions : Choose a country to get a description of ...
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                       tags$p(class = "textbox", "Choose a country to get a description of all the geospatial variables that are available for that country. For each variable, you will get information such as its source, its theme and at what resolution it is available."))),
  
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  #Selecting the input country
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, 
                                                       selectInput("countrySelectVar", h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda")))),
  
  #outputing the corresponding datatable
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, 
                                                       DT::dataTableOutput("vardesc")))
  ),



### Tab 3 : From GPS coordinates ###############################################
tabPanel(h5(strong("From GPS coordinates")),
         
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  #Title : Loading GPS coordinates
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                       br(h1("Loading GPS coordinates")))),
  
  #Instructions : Select a CSV file containing GPS coordinates for which ...
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                       tags$p(class = "textbox", "Select a CSV file containing GPS coordinates for which you want to extract geospatial data, and the country they're situated in. Then, select the variables that represent the longitude and the latitude in your data."))),
  
  fluidRow(style = "background-color:#FFFFFF;", br()), 

  #Selecting the input country
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, 
                                                       selectInput("countryselect", h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda")))),
  
  #Row containing 4 columns :
  fluidRow(style = "background-color:#FFFFFF;", 
           
           #File choice
           column(width = 3, offset = 1,
                  fileInput("surveydata", label = h4("Enter survey data CSV"), buttonLabel = h4("Browse"), accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))), 
           
           #Selecting the longitude variable of the file
           column(width = 3, uiOutput("longitudevarSelect")), 
    
           #Selecting the latitude variable of the file
           column(width = 3, uiOutput("latitudevarSelect")), 
        
           #Go button
           column(width = 1, uiOutput("gobutton"))),
  
  #Leaflet map output
  fluidRow(style = "background-color:#FFFFFF;", 
           column(width = 10, offset = 1, conditionalPanel("input.gobtton", leafletOutput("map")))),
  
  
  uiOutput("varSelectCoord"),
  
  uiOutput("varrep"),
  
  fluidRow(style = "background-color:#FFFFFF;", 
           column(width = 10, offset = 1, DT::dataTableOutput("summary"))),
  
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  fluidRow(style = "background-color:#FFFFFF;", 
           column(width = 10, offset = 1, conditionalPanel("input.plot", highchartOutput("varplot")))),
  
  #download button
  uiOutput("downloadCoord"),
  ),



### Tab 4 : By administrative units ############################################
tabPanel(h5(strong("By administrative units")), id = "unit_tab",

  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  #Title : Administrative Unit Selection
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                       br(h1("Administrative Unit Selection")))),
  
  #Instructions : Select the country you're interested in and click ...
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                       tags$p(class = "textbox", "Select the country you're interested in, and click on the 'Loads units' button. Loading shapefiles may take some time."))),
  
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  #Row containing 2 columns
  fluidRow(style = "background-color:#FFFFFF;", 
           
           #Selecting input country
           column(width = 3, offset = 1, selectInput("countrySelectUnit",h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda"))), 
           
           #Go button
           actionButton("unitsbutton", h3("Load units"), icon("layer-group"), class = "btn-outline-success btn-lg")),
  
  uiOutput("unitchoice"),
  
  fluidRow(style = "background-color:#FFFFFF;", 
           column(width = 10, offset = 1, conditionalPanel("input.addUnitButton", leafletOutput("unitsMap")))),
  
  uiOutput("varSelectUnit"),
  
  fluidRow(style = "background-color:#FFFFFF;", br()),
  
  fluidRow(style = "background-color:#FFFFFF;", column(width = 9, offset = 1, 
                                                       DT::dataTableOutput("unitStats"))),
  
  #download button
  uiOutput("downloadUnit")
  )


), 

## Footer ----------------------------------------------------------------------
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),

#Row containing 3 columns
fluidRow(style = "background-color:#C5E3C6;",
         
         #Laterite logo and social media
         column(width=2, offset = 1, 
                tags$a(href= "https://www.laterite.com/", img(src ="laterite-logo-dark.svg", height = 80, width = 150)), 
                br(), 
                tags$a(href="https://web.facebook.com/Laterite/?_rdc=1&_rdr",icon("facebook", lib = "font-awesome")), 
                tags$a(href="https://twitter.com/LateriteAfrica", icon("twitter", lib = "font-awesome")), 
                tags$a(href="https://www.linkedin.com/company/laterite", icon("linkedin",lib = "font-awesome"))),
         
         #Contact us
         column(width=2, offset = 1, 
                br(),br(), 
                h3(strong("Get in touch")), 
                tags$a(href = "https://www.laterite.com/contact/", "Contact us")), 
         
         #Laterite's motto
         column(width = 2, offset = 2, 
                br(), br(), 
                h3(strong("From data to policy")), 
                tags$a(href="https://www.laterite.com/services/data-collection/", "Data |"), 
                tags$a(href="https://www.laterite.com/services/research-services/", "Research |"), 
                tags$a(href = "https://www.laterite.com/services/advisory-services/", "Advisory "))),

fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;")
)


#-------------------------------------------------------------------------------
#Server-------------------------------------------------------------------------
#-------------------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
#Introduction Page--------------------------------------------------------------
  
#Variables Page-----------------------------------------------------------------

#Computes the variable's table for the selected country
output$vardesc <- DT::renderDataTable({
  
  #Call to the datatable function (DT package)
  datatable(
    
    #fetching the data according to the selected country
    data = read.csv(paste0("Data/", input$countrySelectVar, "/", input$countrySelectVar, "_variables.csv")), 
    
    #other parameters
    class = "hover cell-border",
    selection = 'none',
    rownames = FALSE,
    options = list(pageLength = 50,
                   
                   searchHighlight = TRUE, 
                   orderClasses = TRUE, 
                   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});","}"))
  )
})



#From GPS coordinates Page------------------------------------------------------

#Loads the csv file chosen by the user
surveydata <- reactive({
  req(input$surveydata) #req() checks for required values
  filepath <- input$surveydata$datapath
  
  #if the file is a .csv
  if (grepl("\\.csv$", filepath)){
    read.csv(filepath)
  } 
  
  #if the file is a .xlsx
  else if (grepl("\\.xlsx$", filepath)){
    read_excel(filepath)
  }
})


#Chooses which is the Latitude Variable
output$latitudevarSelect = renderUI({
  req(surveydata())
  selectInput("lat", h4("Select Latitude Variable"), names(surveydata()))
})


#Chooses which is the Longitude Variable
output$longitudevarSelect = renderUI({
  req(surveydata())
  selectInput("lon", h4("Select Longitude Variable"), names(surveydata()))
})


#Go button
output$gobutton <- renderUI({
  req(input$surveydata)
  actionButton("gobtton", h3("Go!"), icon("globe-africa"), class = "btn-outline-success btn-lg")
})


#Upon activation of the Go button : modifies the csv table
surveydata_coord <- eventReactive(input$gobtton, {
  req(input$surveydata, input$lat, input$lon)
  
  #if the lat / long chosen variables are different : 
  if(input$lat != input$lon){
    
    #renames the lat/long columns
    rename(surveydata(), lat = input$lat, lon = input$lon) %>% 
      
      #removes any other column of the table other than those 2
      select(lon, lat) %>% 
      
      #convert them to numeric if they are character (from excel import)
      mutate_if(is.character,as.numeric)
  }
})


#Computes the leaflet map
output$map <- renderLeaflet({
  coord <- req(surveydata_coord())
  
  leaflet(coord) %>% 
    addTiles() %>% 
    addCircleMarkers(color = "#DA302C", radius = 1, opacity = 0.5)
})


#The chosen country is made into a new variable 
country <- eventReactive(input$gobtton, {
  req(input$countryselect)
})

###### Giving the choice of variables to the user
varSelectionSection <- function(tab, instructions, country){
  # Function that returns the section for variable selection (either in the coord or unit tab).
  # It handles the dynamic datatable and the selection of variables
  # Note that any widget in this section will have an ID depending on "tab"
  # Inputs : instructions : instructions specific for the section (select coord or unit tab)
  #          country : character indicating the country
  #          tab : character indicating the tab
  # Output : section : the UI section
  #          rendered_table : the rendered filtered dynamic datatable
  
  
  processTimeColumn <- function(country){
    # Function that processes the Time column for the COUNTRY_variables.csv file. From it, 
    # it extracts some information (see outputs)
    # Inputs : country : character indicating the chosen country
    # Outputs : time_list : a list associating to each variable its "time information" : the months/years on which it spans
    #           levels_years : vector of all possible year choices for the variables
    #           levels_months : vector of all possible month choices for the variables
    #           data : the data of the csv file

    #reading data from the COUNTRY_variables.csv file
    data <- read.csv(paste0("Data/", country, "/", country, "_variables.csv"), stringsAsFactors=T)
    
    #listing the available sources
    src <- list.dirs(paste0("Data/", country), recursive = FALSE, full.names = FALSE)
    
    #listing all resolution folders (in the Country-Src-Res-Variable architecture)
    res <- list.dirs(paste0("Data/", country, "/", src), full.names = FALSE)[-1]
    res <- res[!grepl("/", res)]
    res <- res[res != ""]
    
    #initializing with empty vectors
    levels_years <- c()
    levels_months <- c()
    time_list <- list()
    
    list_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November", "December")
    
    list_16 <- c()
    for (year in 2000:3000){
      timeseq <- paste0(as.character(year), str_pad(seq(1,353,16), 3, pad = "0"))
      list_16 <- append(list_16, timeseq)
    }
    
    #looping over rows in the dataframe
    for (i in 1:nrow(data)){
      
      #content of the Time field for the current row: what we will be looking into
      content <- as.character(data$Time[i])
      
      #variable name
      varname <- as.character(data$Variable.Name[i])
      
      tmp <- c()
      
      #if the variable contains time information
      if (content != ""){
        
        #a character vector is made from the content, by splitting by commas.
        #that supposes a rigorous formatting of the csv file. Example (for years):
        # 2002, 2005, 2007-2011
        vector <- unlist(strsplit(content, ","))
        
        #for each time (year, month, year-range, month-range etc.) in the vector
        for (time in vector){
          
          #if the current element is a time range (example: 2007-2011), it is converted
          #to individual time units (2007 2008 2009 2010 2011)
          if (grepl('-', time)){
            
            #splitting by '-' to get the start and stop times
            split <- unlist(base::strsplit(time, '-'))
            start <- trimws(split[1]) #trimws removes any whitespace
            stop <- trimws(split[2])
            
            #if the current element is a year (not a month)
            if (grepl("[0-9]", start)){
              
              #if the current element is a "each 16 days" element (for ndvi, ndviano)
              if (nchar(start) > 4){
                ind_start <- match(start, list_16)
                ind_stop <- match(stop, list_16)
                
                range_16_days <- list_16[ind_start:ind_stop]
                
                tmp <- append(tmp, range_16_days)
              
              #if the current element is a 4 digit year
              } else {
                
                #the result is appended to the lists
                levels_years <- append(levels_years, as.numeric(start):as.numeric(stop))
                tmp <- append(tmp, as.character(as.numeric(start):as.numeric(stop)))
              }
            
            #if the current element is a month
            } else {
              
              #the month range is processed and a list of individual months is appended
              ind_start <- match(start, list_months)
              ind_stop <- match(stop, list_months)
              
              levels_months <- append(levels_months, list_months[ind_start:ind_stop])
              tmp <- append(tmp, list_months[ind_start:ind_stop])
            }
          
          #if the current element is not a time range, but an individual year/month    
          } else {
            levels_years <- append(levels_years, as.numeric(time))
            tmp <- append(tmp, as.character(as.numeric(time)))
          }
        }
      }
      
      #in this list, for each variable, the time information is stored
      time_list[[varname]] <- tmp
    }
    return(list(time_list=time_list, levels_years=levels_years, levels_months=levels_months, data=data))
  }
  
  #Renders the UI for the section
  section <- renderUI({
    
    #checking required values
    if (tab == "unit") req(units())
    else req(surveydata_coord())
    
    #call to the processTimeColumn, to extract time information from variables
    res <- processTimeColumn(country)
    
    #data stored in the COUNTRY_variables.csv file
    data <- res$data
    
    #years/month choices available for the variables
    levels_years <- res$levels_years
    levels_months <- res$levels_months
    
    #the output here is rendered as a tag list
    tags <- tagList()
    
    #Title and instructions for the selection of variables
    tags[[1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
    tags[[2]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1,  br(h1("Geospatial variables selection"))))
    tags[[3]] <- fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML(instructions)))
    tags[[4]] <- fluidRow(style = "background-color:#FFFFFF;", br())
    
    #levels_years is processed again to separate years from "each 16 days" elements
    choices_levels_years <- c(sort(unique(levels_years[nchar(levels_years)==4])),
                              sort(unique(levels_years[nchar(levels_years)>=4])))
    
    #first row of filters
    tags[[5]] <- fluidRow(style = "background-color:#FFFFFF;",
               column(width = 3, offset = 1, selectInput(paste0("filterYear_",tab), "Filter by Year", choices=choices_levels_years, multiple=T)),
               column(width = 3, selectInput(paste0("filterMonth_",tab), "Filter by Month (average over years)", choices=unique(levels_months), multiple=T)),
               column(width = 3, selectInput(paste0("filterTheme_",tab), "Filter by Theme", choices=levels(data$Theme), multiple=T)))
    
    #second row of filters
    tags[[6]] <- fluidRow(style = "background-color:#FFFFFF;",
               column(width = 3, offset = 1, selectInput(paste0("filterSource_",tab), "Filter by Source", choices=levels(data$Source.Name), multiple=T)),
               column(width = 3, selectInput(paste0("filterRes_",tab), "Filter by Resolution", choices=levels(data$Resolution), multiple=T)),
               column(width = 3, actionButton(paste0("filterGo_",tab), h4("Filter"), icon=icon("refresh"))))
    
    #selected variables (empty at first)
    tags[[7]] <- fluidRow(style = "background-color:#FFFFFF;", 
                          column(width = 6, offset = 1, selectInput(paste0("input_selected_vars_", tab),
                                                                    label = h4("Selected variables :"),
                                                                    choices = c(),
                                                                    selected = c(),
                                                                    multiple=T,
                                                                    width = '650px')),
                          #clear selected variables button
                          div(style = "margin-top:3em;", column(width = 2, actionButton(paste0('clear_selected_vars_', tab), label = "Clear"))))
    
    #dynamic datatable generated from the filters' inputs
    tags[[8]] <- fluidRow(style = "background-color:#FFFFFF;",
                          column(width = 10, offset = 1, DT::dataTableOutput(paste0("dynamic_datatable_", tab))))
    
                        
    #statistical summary only for the unit section
    if (tab == 'unit'){
      
      tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
      
      #choice of the statistics
      tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 11, offset = 1, checkboxGroupInput("stats", label = h3("Choose statistics"), inline = TRUE, choiceValues = list("min", "max", "mean", "quantile", "median", "variance", "stdev"), choiceNames = list(h4("Min"), h4("Max"), h4("Mean"), h4("First and third quartile"), h4("Median"), h4("Variance"), h4("Standard Deviation")) )))
    }
    
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
    
    #Extract action button
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, actionButton(paste0(tab,"extractbutton"), h3("Extract"), icon("table"), class = "btn-outline-success btn-lg")))
    
    tags #"returning" the rendered tag list
  })
  
  #Upon activation of the filterGo button, the data are filtered
  data_filtered <- eventReactive(list(input[[paste0("filterGo_",tab)]], input$unit_tab),{
    
    #each time the data is filtered, shiny's inputs are unbound from those nested
    # inside the datatable (the multiple select inputs) (see line 45)
    session$sendCustomMessage('unbind-DT', paste0('dynamic_datatable_',tab))
    
    #call to the processTimeColumn function
    res <- processTimeColumn(country)
    
    #the time_list is computed from the COUNTRY_variables.csv file
    time_list <- res$time_list
    
    #we also get the data
    data <- res$data
    
    ### formatting the dataframe
    #initial column that will contain checkboxes
    data$Check <- rep(" ", times=nrow(data))
    
    #reordering columns
    col_order <- c("Check", "Variable.Name", "Description", "Time", "Theme", "Source.Name", "Resolution")
    data_filtered <- data[, col_order]
    
    #renaming columns
    names(data_filtered) <- c("Check", "Variable", "Description", "Time", "Theme", "Source", "Resolution")
    
    #the Variable/Time column is converted to character
    data_filtered$Variable <- as.character(data_filtered$Variable)
    data_filtered$Time <- as.character(data_filtered$Time)
    
    ### getting the inputs from the filters
    input_year <- input[[paste0("filterYear_",tab)]]
    input_month <- input[[paste0("filterMonth_",tab)]]
    input_theme <- input[[paste0("filterTheme_",tab)]]
    input_source <- input[[paste0("filterSource_",tab)]]
    input_res <- input[[paste0("filterRes_",tab)]]
    
    ### filtering data (if the corresponding filter input is not empty)
    if ((!is.null(input_year)) || (!is.null(input_month))){
      
      #initializing with an empty list of indexes
      ind <- c() 
      
      #iterating over rows / variables
      for (i in 1:nrow(data_filtered)){
        varname <- data_filtered$Variable[i]
        
        #if the variable is present in time_list (=> a variable containing time info)
        if (varname %in% names(time_list)){
          
          #if any of the time elements related to the variable (time_list[[varname]])
          #are present in input_year OR input_month
          cond_year <- any(input_year %in% time_list[[varname]], na.rm=T)
          cond_month <- any(input_month %in% time_list[[varname]], na.rm=T)

          if ((cond_year) || (cond_month)){
            
            #then the index is registered
            ind <- append(ind, i)
          }
        }
      }
      
      #the data are filtered based on that index
      data_filtered <- data_filtered[ind,]
    }
    if (!is.null(input_theme)) data_filtered <- data_filtered %>% filter(Theme %in% input_theme)
    if (!is.null(input_source)) data_filtered <- data_filtered %>% filter(Source %in% input_source)
    if (!is.null(input_res)) data_filtered <- data_filtered %>% filter(Resolution %in% input_res)
    
    ### this section creates multiple select inputs nested INSIDE the dataframe
    #iterating over rows
    for (i in 1:nrow(data_filtered)){
      
      #getting the variable name
      varname <- data_filtered$Variable[i]
      
      #creating the ID of the future mselect input
      input_name <- paste0("mselect_",tab,"_",varname)
      
      #the choices of those inputs are directly found in time_list
      choices <- time_list[[varname]]
      
      #if the variable has no time info, NA will replace the mselect input
      if (is.null(choices)) data_filtered$Time[i] <- NA
      
      #if the variable has time info, the mselect input widget fills data_filtered$Time[i]
      else { 
        
        #the time range is extracted from the data (csv file)
        range <- data_filtered$Time[i]
        
        #initializing the widget with a vector
        widget <- c('<select id="',input_name,'" class="form-control" multiple="multiple" style="width:150px">')
        
        #(if the current element is a month and if there is no input for the month filter) OR
        #(if the current element is a year and if there is no input for the year filter)
        if (((substr(choices[1],1,1) %in% LETTERS) && (is.null(input_month))) ||
            (!(substr(choices[1],1,1) %in% LETTERS) && (is.null(input_year)))){
          
          #then the time range is put as the default selected option
          widget <- append(widget, c('<option value="',range,'"selected>',range,'</option>'))
        } else {
          
          #otherwise no. The default selected option will correspond to the filter value
          widget <- append(widget, c('<option value="',range,'">',range,'</option>'))
        }
        
        #looping over choices / options
        for (choice in choices){
          
          #if the particular year/month had been selected has a filter, then it becomes the default option (in the mselect input)
          if (choice %in% c(input_year, input_month)) widget <- append(widget, c('<option value="',choice,'"selected>',choice,'</option>'))
          else widget <- append(widget, c('<option value="',choice,'">',choice,'</option>'))
        }
        
        widget <- append(widget, c('</select>'))
        
        #converting the vector to a single character chain, and let it fill the time column
        data_filtered$Time[i] <- paste(widget, collapse='')
      }
    }
  
    data_filtered
  })
  
  #rendering the datatable, from data_filtered()
  rendered_table <- DT::renderDataTable({
    
    data <- data_filtered() #getting the reactive data_filtered()

    dtable <- datatable(
      
      data = data, 
      escape = FALSE, #allows to create the mselect inputs 
      selection = 'none', #skipping DT's original selection system to use the extension
      extension =c('Select', 'Buttons'), #using the Select and Buttons extensions of DT
      class = "hover cell-border",
      rownames = FALSE,
      
      #allows to make checkboxes work
      callback = JS(c(
        "table.on('select', function(e, dt, type, indexes){",
        "  if(type === 'row'){",
        "    Shiny.setInputValue('selectedRow', indexes);",
        "  }",
        "});"
      )),
      
      options = list(
        pageLength = nrow(data), #several pages would lead to problems with the mselect inputs
        
        #creates checkboxes for variable selection
        columnDefs = list(
          list(targets = 0, orderable = FALSE, className = "select-checkbox")
          ),
        
        #selection options
        select = list(
          style = "os", selector = "td:first-child"),

        dom = 'Blfrtip',
        rowId = 0,
        buttons = c('selectAll', 'selectNone'), #adding selectAll and deselectAll buttons
        searchHighlight = TRUE,
        orderClasses = TRUE,
       
        initComplete = JS(c("function(settings, json) {",
                            #turns the select inputs to mselect inputs (for multiple selection)
                           "  $('[id^=mselect]').selectize();", 
                           #color for the datatable header
                           "  $(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});","}")),
        
        #managing the binding between shiny's inputs and javascript's
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))

    #dtable is the rendered object
    dtable
    
    }, server = FALSE 
    )
  

  determine_time_tag <- function(selected_time, timeunit){
    # Function that creates the time tag for the selected time (year, month, each 16 days) in
    # case the selected_time vector contains several elements, formatting them properly
    # Inputs : selected_time : character vector indicating the selected time (months, years)
    #                          from the nested mselect inputs
    #          timeunit : character indicating the nature of the time (year, month, each 16 days)
    # Outputs : time_tag 
    
    #based on timeunit, a chronological list is computed
    if (timeunit=='month') list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    else if (timeunit=='year') list <- as.character(2000:3000)
    else { 
      list <- c()
      for (year in 2000:3000){
        timeseq <- paste0(as.character(year), str_pad(seq(1,353,16), 3, pad = "0"))
        list <- append(list, timeseq)
      }
    }
    
    #getting the indices of the selected_time vector present in the list
    match <- match(selected_time, list)
    
    #sorting the times (according to the list)
    selected_time <- list[sort(match)] 
    match <- sort(match)
    
    #this logical vector will indicate whether time elements in selected_unit are
    #consecutive or not
    difference <- diff(match) == 1
    
    #initializing with empty vector
    time_tag <- c()
    
    #To make it simple, the following section will iterate over elements of selected_time,
    #appending them to time_tag. However, whenever some elements are consecutive, they will
    #be appended with a more compact form -> instead of 2012_2013_2014_2015, it will be 2012-2015
    
    i <- 1
    while (i <= length(selected_time)){ #iterating over selected time elements
      
      time_tag <- append(time_tag, selected_time[i]) #appending the time to time_tag
      sig <- F #sig = signal whether elements are consecutive, initialized to F
      
      #testing if the current element is consecutive to the next
      while((difference[i]) && (i<=length(difference))){
        #if so, iterating continues (i increases), until the condition is not met
        #basically, it's skipping the in-between years (between start and stop)
        i <- i + 1
        sig <- T #sig becomes TRUE
      }
      
      #if the program went into the while loop above
      if (sig){
        time_tag <- append(time_tag, '-')
        time_tag <- append(time_tag, selected_time[i])
        #the time_tag will get something like 2010-2015 for example
      }
      
      i <- i + 1
    }
    
    #converting the vector to a single character
    time_tag <- paste0(time_tag, collapse='_')
    
    #removing those '_-_' introduced to the tag during the while loops
    time_tag <- gsub('_-_', '-', time_tag)

    return(time_tag)
  }
  
  conditional_time_tag <- function(selected_time){
    # Function that determines the time tag for the selected time
    # Inputs : selected_time : character vector indicating the selected time(s) (months, years)
    #                          from the nested mselect inputs
    # Outputs : time_tag 
    
    #if the mselect input was empty
    if (is.null(selected_time)){
      time_tag <- '_all' #this 'all" tag will indicate that all possible times will later be extracted for the variable
      
    #"no_time" indicates that the variable as no time information
    } else if (selected_time == 'no_time'){
      time_tag <- ''
      
    #if only one element was selected
    } else if (length(selected_time)==1){
      time_tag <- selected_time
      time_tag <- paste0('_',time_tag) #the time tag will consist in just that element
      
    #if several elements were selected (calls to the determine_time_tag function)
    } else if (length(selected_time)>1) {
      
      #if they are months
      if (substr(selected_time[1],1,1) %in% LETTERS){
        time_tag <- determine_time_tag(selected_time, timeunit='month')
        
      #if they are years
      } else if (nchar(selected_time[1])==4){
        time_tag <- determine_time_tag(selected_time, timeunit='year')
        
      #if they are "each 16 days" elements
      } else {
        time_tag <- determine_time_tag(selected_time, timeunit='each_16_days')
      }
      time_tag <- paste0('_',time_tag)
    }
    
    time_tag <- gsub(', ', '_', time_tag) #removing potential commas
    return(time_tag)
  }
  
  #Upon checking any row in the datatable, the selected variable go to the
  #"selected variables" select input (note: reacts to the select all button)
  observeEvent(input[[paste0("dynamic_datatable_", tab, "_rows_selected")]], {
    
    #index(es) of the selected row(s)
    row_ind <- input[[paste0("dynamic_datatable_", tab, "_rows_selected")]]
    
    #name(s) of the selected row(s)
    varname <- data_filtered()[row_ind, "Variable"]

    #time selection for the selected variable(s)
    selected_time <- c()
    reactive_list <- reactiveValuesToList(input)
    
    #for each selected variable
    for (v in varname){
      
      #if the variable has no time information (no associated mselect input widget)
      if(!(paste0("mselect_",tab,"_",v) %in% names(reactive_list))) s <- 'no_time'
      else s <- input[[paste0("mselect_",tab,"_",v)]]
      selected_time <- append(selected_time, list(s))
    }

    #computing a time tag depending on time selection
    time_tag <- sapply(selected_time, conditional_time_tag)

    #paste the variable name and time tag together
    varname_and_time <- paste0(varname, time_tag)
    
    #varname_and_time is appended to the global list of selected variables
    selected_vars_list <<- append(selected_vars_list, varname_and_time)

    #the "selected variables" select input is updates with the new list
    updateSelectInput(session, inputId = paste0("input_selected_vars_", tab), 
                      choices = selected_vars_list, 
                      selected = selected_vars_list)
  })
  
  #Whenever a selected variable is manually removed by the user (in the select input),
  #selected_vars_list is also updated
  observe({
    select_input_content <- input[[paste0("input_selected_vars_", tab)]]
    
    selected_vars_list_u <- unique(selected_vars_list)
    diff <- setdiff(selected_vars_list_u, select_input_content)

    if (length(diff)){
      selected_vars_list <<- selected_vars_list_u[selected_vars_list_u != diff]
    }

  })
    
  #Upon activation of the Clear button, selected_vars_list is cleared and the 
  #select input is updated with an empty character
  observeEvent(input[[paste0("clear_selected_vars_",tab)]], {
    
    selected_vars_list <<- c()
    
    updateSelectInput(session, inputId = paste0("input_selected_vars_", tab), 
                      choices = character(0), 
                      selected = character(0))
  })
  
  return(list(section=section, rendered_table=rendered_table))
}

#instructions to pass to the varSelectionSection function
instructionsVarSelectCoord <- "<p class = 'textbox'> Now, select the geospatial variables you want extracted for your data. You can apply some filters to the table. To select a variable, click on its checkbox. You can choose between different time observations in the 'Time' column. If you want to choose all variables at once, click on the 'Select all' button. The selected variables will be shown in the 'Selected variables' field.</p>"

# Variable selection for the select coord section
res_coord <- varSelectionSection(tab="coord",
                           instructions=instructionsVarSelectCoord,
                           country=country())
#outputs
output$varSelectCoord <- res_coord$section
output$dynamic_datatable_coord <- res_coord$rendered_table

get_raster_file <- function(selected_vars, country){
  # Function that gets a list of paths to raster files, from what's in the "selected variables" select input
  # Inputs : selected_vars : list of variables with time tags, coming from the select input
  #          country : character indicating the chosen country
  # Output : raster_files : vector of paths to raster files
  
  #getting the data
  data <- isolate(read.csv(paste0("Data/", country, "/", country, "_variables.csv")))
  
  #splitting the character vector by '_' (separating varname and time elements)
  split_ <- base::strsplit(selected_vars, '_')
  
  #getting varnames, first elements of the list
  varname <- sapply(split_, '[[', 1)
  
  #iterating over the split_
  for (k in 1:length(split_)){
    
    #content of the list ([varname,time1,time2 etc])
    var <- split_[[k]]
    
    #if the selected var contains time information
    if (length(var)>1){
      
      #iterating over time elements
      for (i in 2:length(var)){
        time <- var[i]
        
        #if the time element is a time range (2010-2015 for example)
        if (grepl('-', time)){
          
          #splitting by '-' and getting the start and stop elements
          split_time <- unlist(base::strsplit(time, '-'))
          start <- split_time[1]
          stop <- split_time[2]
          
          #if the elements are months 
          if (substr(start, 1, 1) %in% LETTERS){
            
            #getting the corresponding time range (ex: replacing January-March to January February March)
            list_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November", "December")
            mstart <- match(start, list_months)
            mstop <- match(stop, list_months)
            time_range <- list_months[mstart:mstop]
            
          #if they are years  
          } else if (nchar(start)==4) {
              time_range <- as.character(as.numeric(start):as.numeric(stop))
              
          #if they are "each 16 days" elements    
          } else {
            
              list_16 <- c()
              for (year in 2000:3000){
                timeseq <- paste0(as.character(year), str_pad(seq(1,353,16), 3, pad = "0"))
                list_16 <- append(list_16, timeseq)
              }
              mstart <- match(start, list_16)
              mstop <- match(stop, list_16)
              time_range <- list_16[mstart:mstop]
          }
          split_[[k]] <- append(split_[[k]], time_range)
        }
      }
    }
  }
  
  #getting vectors of the source and resolution of selected variables
  src_list <- data[match(varname, data$Variable.Name), "Source.Name"]
  res_list <- data[match(varname, data$Variable.Name), "Resolution"]

  raster_files <- c() #empty vector for initialization
  
  #getting the names of the sources, as found in the folder
  #(because those names are different than those present in the csv file)
  src <- list.dirs(paste0("Data/", country), full.names = FALSE)[-1]
  src_names <- src[!grepl("/", src)]

  #iterating over the selected variables list
  for (i in 1:length(split_)){
    
    var <- split_[[i]]
    
    #getting the corresponding source and resolution
    src <- src_names[which(sapply(src_names, function(x){grepl(x, src_list[i])}))]
    res <- res_list[i]
    
    #if there's no time information
    if (length(var)==1){
      raster_files <- append(raster_files, 
                             paste0("Data/", country, "/", src, "/", res, "/", var[1], ".tif"))
      
    #if there's all, select files for all times possible
    } else if (var[2]=='all') {
      files <- list.files(paste0("Data/", country, "/", src, "/", res, "/", var[1]), full.names = T)
      raster_files <- append(raster_files, files)
      
    #if there's time information
    } else {

      #iterating over time elements
      for (k in 2:length(var)){
        time <- var[k]
        
        #if the element is a time range (2012-2018 for example)
        if (!grepl('-', time)){
          
          #if they are months
          if (substr(time, 1, 1) %in% LETTERS){
            raster_files <- append(raster_files, 
                                 paste0("Data/", country, "/", src, "/", res, "/", var[1], "/", var[1], tolower(substr(time, 1, 3)),'.tif'))
            
          #if they are years or "each 16 days" elements
          } else {
            raster_files <- append(raster_files, 
                                   paste0("Data/", country, "/", src, "/", res, "/", var[1], "/", var[1], time,'.tif'))
          }
        }
      }
    }
  }
  return(raster_files)
}

### Extracting the chosen geospatial variables for the loaded GPS coordinates ###
#Upon activation of the extract button : extracting the values of the chosen variables for the GPS coordinates
geoDataCoord <- eventReactive(input$coordextractbutton, {
  
  #initializing the loading bar
  progress <-  Progress$new()
  progress$set(message = "Please wait.", value = 0)
  
  #Getting the variables that were checked and putting them in a list
  vars <- get_raster_file(isolate(input$input_selected_vars_coord), country())
  data <- surveydata_coord() #processed data from the csv
  extracted <- list()
  
  # update : each variable is now processed individually. In the previous version, 
  # variables of the same resolution were grouped and stacked together 
  # although they had different extents, which generated an error (issue n°7)
  for (i in 1:length(vars)){

    if (file.exists(vars[[i]])){
      #key step : extracting the extracting the values of the chosen variables for the GPS coordinates (raster package)
      extracted[[i]] <- raster::extract(raster::stack(vars[[i]]), data)
      progress$inc(1/length(vars), detail = paste('Extracting',vars[[i]]))
    } else print(paste(vars[[i]], 'doesnt exist'))
    
  }
  
  #closing the progress widget
  progress$close()
  
  #merging the original survey data with the variables
  cbind(surveydata(), do.call("cbind", extracted))
})


### Making the user choose a geospatial variable from the extracted data to plot (histogram for continuous) and give a statistical summary ###

#Title and instructions for the section
output$varrep <- renderUI({
  req(geoDataCoord())
  
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #Title : Geospatial variables distribution
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                         br(h1("Geospatial variables distribution")))),
    
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #Instructions : For each of the extracted variables ...
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, 
                                                         tags$p(class = "textbox", "For each of the extracted variables, you can check a statistical summary and an histogram of its distribution among the GPS coordinates loaded."))),
    
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #selectInput for the choice of the variable
    fluidRow(style = "background-color:#FFFFFF;", column(width = 4, offset = 1, 
                                                         selectInput("plot", h4("Select Variable"), setdiff(names(geoDataCoord()), names(surveydata())))))
  )
})


#Rendering the statistical datatable for the chosen variable
output$summary <- DT::renderDataTable({
  req(geoDataCoord())
  req(input$plot)
  
  v <- geoDataCoord()[, input$plot]
  
  #building the statistical sumlary
  df <- data.frame(Minimum = min(v,na.rm = TRUE), 
                   `First Quartile` = quantile(v, 0.25,na.rm = TRUE), 
                   Median = median(v,na.rm = TRUE), 
                   Mean = mean(v,na.rm = TRUE), 
                   `Third Quartile` = quantile(v, 0.25,na.rm = TRUE), 
                   Maximum = max(v,na.rm = TRUE), 
                   `Standard Deviation` = sd(v,na.rm = TRUE))
  
  df <- apply(df, c(1,2), round, digits = 2) #rounding values
  
  #call to datatable function (DT package)
  datatable(data = df, 
            class = "hover cell-border", 
            rownames = FALSE, 
            options = list(ordering = FALSE, dom = "t", initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});",
              "}")
  )
  )
})


#Rendering the histogram for the chosen variable
output$varplot <- renderHighchart({
  req(geoDataCoord())
  req(input$plot)
  
  #building histogram
  h <- hchart(hist(geoDataCoord()[, input$plot]), color = "#7DD9BA", name = input$plot, showInLegend = FALSE)
})


### Downloading the obtained geospatial data as a CSV ###

#Download handler
output$downloadDataCoord <- downloadHandler(
  filename = function() {
    paste0(substr(input$surveydata$name, 0, nchar(input$surveydata$name)-4), "_Geospatial.csv")
  },
  content = function(file) {
    
    #writing csv and replacing NAs to empty strings (issue n°8)
    write.csv(geoDataCoord(), file, row.names = FALSE)
  },
  contentType = "text/csv"
)

#Title and instructions for the section
output$downloadCoord <- renderUI({
  req(geoDataCoord())
  
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #Title : Data Exportation
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Data Exportation")))),
    
    #Instructions : You can choose to export the extracted data as a CSV ...
    fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML("<p class = 'textbox'> You can choose to export the extracted data as a CSV. The CSV you obtain will be the original loaded CSV along with the variables you selected for each row.</p>"))),
    
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #download button
    fluidRow(style = "background-color:#FFFFFF;", column(width = 6, offset = 1, downloadButton("downloadDataCoord", h3("Export as CSV"), class = "btn-outline-success btn-lg"))),
    
    fluidRow(style = "background-color:#FFFFFF;", br())
  )
})


#By administrative units Page---------------------------------------------------

#Names of the variable representing the name of the administrative units, to update if the shape files change
unitsvarnames <- eventReactive(input$unitsbutton, { #upon activation of the units button
  
  req(input$countrySelectUnit)
  switch(input$countrySelectUnit, "Ethiopia" = c("REGIONNAME", "ZONENAME", "WOREDANAME"),
         "Rwanda" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4", "NAME_5"),
         "Kenya" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4", "NAME_5"),
         "Uganda" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4") 
  )
})

#Names of the variable representing the code of the administrative units, to update if the shape files change
unitsvarcodes <- eventReactive(input$unitsbutton, { #upon activation of the units button
  
  req(input$countrySelectUnit)
  switch(input$countrySelectUnit, "Ethiopia" = c("RID", "Z4ID", "WOREDANO_"),
         "Rwanda" = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5"),
         "Kenya" = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5"),
         "Uganda" = c("ID_1", "ID_2", "ID_3", "ID_4") 
  )
})

#initializing the chosen administrative levels as an empty dataframe
chosenAdminLevels <- data.frame()

#Loads the shapefiles for the selected country into the variable 'units'
units <- eventReactive(input$unitsbutton, {
  req(input$countrySelectUnit)
  
  #initializing the loading bar
  progress <-  Progress$new()
  progress$set(message = "Please wait.", value = 0)
  
  #listing files for the country
  levels <- list.dirs(paste0("Data/shapes/", input$countrySelectUnit))[-1]
  
  units <- lapply(levels, function(l) {
    progress$inc(1/length(levels), detail = paste("Loading shapefile", l))
    shp <- list.files(l, full.names = FALSE)[1]
    readOGR(l, substr(shp, 0, nchar(shp) - 4))
  })
  names(units) <- lapply(levels, function(x) { substr(x, regexpr("_", x)[[1]] + 1, nchar(x))})
  
  #closing the loading widget
  progress$close()
  
  #resetting the potential previously chosen admin levels
  chosenAdminLevels <<- data.frame()
  
  units
})

#Creates multiple dropdown select inputs allowing the user to choose the administrative units they're interested in
output$unitchoice <- renderUI({
  req(units())
  
  #importing objects
  units <- units()
  varnames <- unitsvarnames()
  varcodes <- unitsvarcodes()
  
  #the output here is rendered as a tag list
  tags <- tagList()
  
  tags[[1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[2]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  
  #Instructions for the AU selection : You can now pick the administrative units ...
  tags[[3]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, HTML("<p class = 'textbox'>You can now pick the administrative units you want by selecting them through the dropdown menus and clicking the Add Unit button. As you add units to your selection, they will also appear on the map.</br> You can choose to upload a list of administrative units from a csv file (in that case, mind the spelling of units and the file format). You can also save the units to a csv file for future reuse.</p>")))
  
  tags[[4]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  
  #creating the selecting inputs in a list
  listUnits <- paste(names(units)[1], units[[1]][[varcodes[1]]], "-", units[[1]][[varnames[1]]], sep = " ")
  names(listUnits) <- units[[1]][[varnames[1]]] 
  select <- list(selectInput(inputId = paste0("unit", names(units)[1]), label = h4(paste0("Select ", names(units)[1])), choices = listUnits))
  
  for(i in 2:length(units)){
    namevar <- varnames[i]
    codevar <- varcodes[i]
    nametopvar <- varnames[i-1]
    codetopvar <- varcodes[i-1]
    output[[paste0("out", names(units)[i])]] <- eval(parse(text = paste0("renderUI({
      topunit <- req(input[[paste0('unit', names(units)[", i, "-1])]])
      if((topunit != 'None') && (topunit != 'All')){
            spaces <- gregexpr(' ', topunit)[[1]]
            topunitcode <- substr(topunit, spaces[1] + 1, spaces[2] - 1)
            
            if(input$countrySelectUnit == 'Ethiopia'){
              subunits <- base::subset(units[[", i, "]], substr(", codevar, ", 0, 2*(", i, "-1)) == topunitcode)
            } else {
              subunits <- base::subset(units[[", i, "]], ", codetopvar, " == topunitcode)
            }
            
            if(length(subunits) > 0) {
              listSubunits <- paste(names(units)[", i, "], subunits$", codevar, ", '-', subunits$", namevar, ", sep = ' ')
              names(listSubunits) <- subunits$", namevar, "
            }
            else listSubunits <- c()
      }
      else listSubunits <- c()
      choices <- c('None', listSubunits)
      if (length(choices)>1){
        choices <- append(choices, 'All', after=1)
      }
      selectInput(inputId = paste0('unit', names(units)[", i, "]), label = h4(paste0('Select ', names(units)[", i, "])), choices = choices, selected = 'None')
    })")))
    select[[i]] <- uiOutput(paste0("out", names(units)[i]))
  }
  
  #iterating over the list, with the following condition:
  i <- 1
  while(i+2 <= length(select)){ #controls the display of boxes (rows of 3 boxes)
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]), column(width = 3, select[[i+1]]), column(width = 3, select[[i+2]]))
    i <- i+3
  }
 
  #dealing with remaining widgets that did not make a complete row of 3
  if(i < length(select)){
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]), column(width = 3, select[[i+1]]))
  }
  
  if(i == length(select)){
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]))
  }
  
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", 
                                     
                                     #Add Unit Button
                                     column(width = 2, offset = 1, actionButton("addUnitButton", h4("Add unit"), icon("map-marked"), class = "btn-outline-success btn-lg")),
                                     
                                     column(width = 3, offset = 1, fileInput("fileAUSelection", label=NULL, buttonLabel = "Upload units", accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))))
                                            
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", 
                                     
                                     #Chosen Units
                                     column(width = 3, offset = 1, selectInput("listChosenUnits", label = "Chosen units", choices = NULL, multiple = TRUE)),
                                     
                                     #Download units button
                                     div(style = "margin-top:1em;", column(width = 3, downloadButton("downloadSelectedAU", label="Save the selected units"))))
  
  tags
})

#downloads the selected administrative units to a csv file
output$downloadSelectedAU <- downloadHandler(
  filename = function() {
    paste0("AdminUnitSelection.csv")
  },
  content = function(file) {
    to_write <- chosenAdminLevels[chosenAdminLevels$id %in% input$listChosenUnits,]
    write.csv(to_write, file, row.names=FALSE)
  },
  contentType = "text/csv"
)

#uploads a pre-selected AU from a file
observeEvent(input$fileAUSelection, {
  
  #loading the file containing the AU selection, chosen by the user
  filepath <- input$fileAUSelection$datapath
  AUSelection <- read_csv(filepath, show_col_types = FALSE)
  chosenAdminLevels <<- AUSelection
  
  newchosenunits <- AUSelection$id
  updateSelectInput(session, inputId = "listChosenUnits", choices = newchosenunits, selected = newchosenunits)

})

updateChosenLevels <- function(chosenAdminLevels, toAdd, units){
  # Function that updates the dataframe of the chosen admin levels, each time the add unit button is clicked
  # Input : chosenAdminLevels : the already selected admin levels
  #         toAdd : the chosen unit(s) to add to the dataframe
  #         units : loaded shapefiles of administrative units, containing data
  # Output : the updated chosenAdminLevels dataframe
  
  ### for each added unit, the corresponding levels (ex: region, zone, woreda) are stored in the temporary variable chosenTmp
  #adding the chosen unit(s) as ID of the df's elements
  chosenTmp <- data.frame(id=toAdd)
  
  #iterating over the admin levels (ex: region, zone, woreda)
  for (k in 1:length(units)){
    level <- names(units)[k]
    
    #fetching the selected unit
    level_name_and_code <- input[[paste0("unit", names(units)[k])]]

    if (level_name_and_code == "None"){
      level_name <- "None"
      level_code <- "None"
      
    } else {
      #extracting its name and code
      level_name <- unlist(base::strsplit(level_name_and_code, " - "))[2]
      level_code <- trimws(gsub(level,"",unlist(base::strsplit(level_name_and_code, " - "))[1]))
    }
    
    #if the user did not chose "All"
    if (level_code != 'All'){
      
      #updating the df with the selected units
      chosenTmp[,paste0(level,"_name")] <- level_name
      chosenTmp[,paste0(level,"_code")] <- level_code
      
      #if the user chose "All"
    } else {
      
      #the level names and codes are extracted from the id column. They are different from one unit to another
      to_copy_name <- unlist(lapply(base::strsplit(chosenTmp$id, " - "), `[[`, 2))
      to_copy_code <- trimws(gsub(level,"",unlist(lapply(base::strsplit(chosenTmp$id, " - "), `[[`, 1))))
      
      chosenTmp[,paste0(level,"_name")] <- to_copy_name
      chosenTmp[,paste0(level,"_code")] <- to_copy_code
    }
  } 
  
  #merging the new chosenTmp to the old chosenAdminLevels
  chosenAdminLevels <- rbind(chosenAdminLevels, chosenTmp)
  return(chosenAdminLevels)
}

#upon activation of the addUnit button : updates the "Chosen Units" field
observeEvent(input$addUnitButton, {
  
  #importing variables
  units <- req(units())
  varcodes <- unitsvarcodes()
  varnames <- unitsvarnames()
  
  #initializing with None
  chosenunit <- "None"
  i <- length(units)
  
  #while the field is blank
  while(chosenunit == "None" & i>0) {
    chosenunit <- input[[paste0("unit", names(units)[i])]]
    i <- i-1
  }
  
  #if a unit was added
  if(chosenunit != "None") {
    
    #if the user select All
    if (chosenunit == 'All'){
      
      #importing variables
      namevar <- varnames[i+1]
      codevar <- varcodes[i+1]
      codetopvar <- varcodes[i]
      
      #topunit is the unit level just before the current one
      topunit <- req(input[[paste0('unit', names(units)[i])]])
      
      #extracting the code of this topunit
      spaces <- gregexpr(' ', topunit)[[1]]
      topunitcode <- substr(topunit, spaces[1] + 1, spaces[2] - 1)
      
      #between Ethiopia and the other countries, the underlying shapefile system is different
      if(input$countrySelectUnit == 'Ethiopia'){
        subunits <- eval(parse(text=paste0("base::subset(units[[i+1]]@data, str_starts(units[[i+1]]@data$",codevar,", topunitcode))")))
      } else {
        subunits <- eval(parse(text=paste0("base::subset(units[[i+1]]@data, ",codetopvar," == topunitcode)")))
      }
      
      #list of all the selected units
      listAll <- eval(parse(text=paste0("paste(names(units)[i+1], subunits$",codevar,", '-', subunits$",namevar,", sep = ' ')")))
      names(listAll) <- subunits$namevar
      
      chosenunit <- listAll
    }
    
    #updating the selectinput with the new unit(s)
    newchosenunits <- c(chosenunit, input$listChosenUnits)
    updateSelectInput(session, inputId = "listChosenUnits", choices = newchosenunits, selected = newchosenunits)
    
    #updating the chosen admin levels dataframe
    chosenAdminLevels <<- updateChosenLevels(chosenAdminLevels, chosenunit, units)
  }
})

#computes chosen units
chosenUnits <- reactive({
  
  #importing R variables
  units <- req(units())
  listChosen <- req(input$listChosenUnits)
  varcodes <- req(unitsvarcodes())

  unitNames <- list()
  
  l <- lapply(listChosen, function(unit){
    spaces <- gregexpr(" ", unit)[[1]]
    unitType <- substr(unit, 0, spaces[1] - 1)
    unitID <- substr(unit, spaces[1] + 1, spaces[2] - 1)
    varcode <- varcodes[which(names(units) == unitType)]
    eval(parse(text = paste0("subset(units[[unitType]], ", varcode, " == unitID)")))
  })
  
  #setting listChosen's elements as names for l
  setNames(l, listChosen)
})

#Creates the polygons corresponding to the chosen units
chosenUnitsPolygons <- reactive({
  req(input$listChosenUnits)
  chosenunits <- req(chosenUnits())

  poly <- list() #initial list of polygons : empty
  
  for(i in seq_len(length(chosenunits))){
    poly[[i]] <- chosenunits[[i]]@polygons[[1]]
    poly[[i]]@ID <- as.character(i)
  }
  
  names <- lapply(names(chosenunits), function(x){
    spaces <- gregexpr(" ", x)[[1]]
    substr(x, spaces[3] + 1, nchar(x))
  })
  list(polygons = poly, names = names)
})

#Shows the chosen administrative units as polygons on a map
output$unitsMap <- renderLeaflet({
  chosenunitspoly <- req(chosenUnitsPolygons())
  
  #creating the leaflet map
  leaflet(SpatialPolygons(chosenunitspoly$polygons)) %>% 
    addTiles() %>% 
    addPolygons(label = chosenunitspoly$names, color = "#DA302C", weight = 4, 
                labelOptions = labelOptions(noHide = T, direction = "bottom", 
                                            style = list("color" = "#DA302C","font-family" = "serif","box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-size" = "12px","border-color" = "rgba(0,0,0,0.5)")))
})

instructionsVarSelectUnit <- "<p class = 'textbox'> Now, select the geospatial variables you want extracted for your data. You can apply some filters to the table. To select a variable, click on its checkbox. You can choose between different time observations in the 'Time' column. If you want to choose all variables at once, click on the 'Select all' button. The selected variables will be shown in the 'Selected variables' field.</br> Then click on the statistics you would want computed for your administrative units.</p>"

#Gives the choice of variables to the user, like in the GPS coordinates page and also the statistics they want for these variables
selected_vars_list <- c() 
  
res_unit <- varSelectionSection(tab="unit",
                           instructions=instructionsVarSelectUnit,
                           country=input$countrySelectUnit)

output$varSelectUnit <- res_unit$section
output$dynamic_datatable_unit <- res_unit$rendered_table

#Extracts the chosen statistics for the chosen geospatial variables for each of the chosen administrative units
geoDataUnit <- eventReactive(input$unitextractbutton, {
  stats <- req(input$stats)
  chosenUnits <- req(chosenUnits())

  #initializing the loading bar
  progress <-  Progress$new()
  progress$set(message = "Please wait.", value = 0)

  #Getting the variables that were checked and putting them in a list
  vars <- get_raster_file(isolate(input$input_selected_vars_unit), input$countrySelectUnit)

  extracted <- list()
  
  for(i in 1:length(vars)){
    
    progress$inc(1/length(vars), detail = paste('Extracting',vars[[i]]))
    
    if (file.exists(vars[[i]])){
      values <- list()
      for(j in seq_len(length(chosenUnits))){
        values[[j]] <- exact_extract(raster::stack(vars[[i]]), chosenUnits[[j]], fun = stats, full_colnames = TRUE, quantiles = c(0.25, 0.75))
      }
      
      extracted[[i]] <- do.call("rbind", values)
    } else print(paste(vars[[i]], 'doesnt exist'))
    
  }
  table <- do.call("cbind", extracted)
  row.names(table) <- names(chosenUnits)
  
  #closing progress widget
  progress$close()
  
  table
})

#Showing the results, i.e a table with one row for each chosen administrative units and the chosen statistics as columns
output$unitStats <- DT::renderDataTable({
  table <- req(geoDataUnit())
  table <- apply(table, c(1,2), round, digits = 2)
  datatable(table, class = "hover cell-border", options = list(ordering = FALSE, dom = "t", scrollX = TRUE,  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});",
    "}")
  )
  )
})

includeAdminLevels <- function(geoDataUnit){
  # Function that merges the geospatial data for the unit with the corresponding administrative levels (related to improvement n°6)
  # Input : geoDataUnit : final output of the administrative unit tab
  # Output : the same data merged with the administrative levels
  
  #converting the list to a dataframe
  chosenAdminLevels <- as.data.frame(chosenAdminLevels)
  
  #adding rownames from the id column
  rownames(chosenAdminLevels) <- chosenAdminLevels$id
  chosenAdminLevels$id <- NULL
  
  #merging the 2 dataframes by their rownames. 
  #Any row id not present in both tables is dismissed (case when the user decides to delete/modify his selection)
  geoDataUnitPlus <- merge(chosenAdminLevels, geoDataUnit, by = 0)
  
  #rownames for the merged dataframe
  rownames(geoDataUnitPlus) <- geoDataUnitPlus$Row.names
  geoDataUnitPlus$Row.names <- NULL
  
  return(geoDataUnitPlus)
}

#Making these geospatial statistics available to download as a CSV
output$downloadDataUnit <- downloadHandler(
  filename = function() {
    paste0("Geospatial_Data_", input$countrySelectUnit, ".csv")
  },
  content = function(file) {
    
    #if the user chose to include the administrative levels in the final output
    if (input$includeLevels){
      
      #including the levels
      geoDataUnitPlus <- includeAdminLevels(geoDataUnit())
      
      #writing
      write.csv(geoDataUnitPlus, file)
      
    } else {
      write.csv(geoDataUnit(), file)
    }
  },
  contentType = "text/csv"
)

#Generates the section for downloading the unit
output$downloadUnit <- renderUI({
  req(geoDataUnit())
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #Title : "Data exportation"
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Data Exportation")))),
    
    #Instructions : "You can choose to export the extracted.."
    fluidRow(style = "background-color:#FFFFFF;", br(), 
             column(width = 8, offset = 1, HTML("<p class = 'textbox'> You can choose to export the extracted data as a CSV. The CSV you obtain will be the summary table you can see above. You can choose to include the chosen administrative units in the output file.</p>"))),
    
    fluidRow(style = "background-color:#FFFFFF;", br()),
    
    #download unit button
    fluidRow(style = "background-color:#FFFFFF;", 
             column(width = 2, offset = 1, downloadButton("downloadDataUnit", h3("Export as CSV"), class = "btn-outline-success btn-lg")),
             column(width = 10, offset = 1, checkboxInput('includeLevels', 'Include the administrative level info in the CSV'))
             ),
    fluidRow(style = "background-color:#FFFFFF;", br())
  )
})


})

#Launching the app
shinyApp(ui = ui, server = server)

