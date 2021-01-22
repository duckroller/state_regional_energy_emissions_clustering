vars <- setdiff(names(emissions_data),"state")

pageWithSidebar(
    headerPanel('Sectoral CO2 Emissions Clustering of US States'),
    sidebarPanel(
        helpText("To begin, select the variables and states you would like to cluster on, choose a clustering algorithm, and then click \"Clusterize!\""),
        actionButton("go", "Clusterize!"),
        hr(),
        selectInput('algo', 'Clustering Method', c("K-Means Clustering", "Hierarchical Clustering")),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
        hr(),
        #selectInput('xcol', 'X Variable', vars, selected = vars[[12]]),
        #selectInput('ycol', 'Y Variable', vars),
        pickerInput(
            inputId = "variables",
            label = "Select Variables to Cluster By",
            choices = vars,
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        ),
        

        #verbatimTextOutput('selected_states'),
        #selectInput('state_choice', 'States', state.name, multiple=TRUE, selectize=FALSE),
        tags$head(tags$style(HTML('#state_choice{overflow-y: scroll;height: 16em;}'))),
        #tags$h3("States to Cluster"),
                    pickerInput(
                inputId = "state_choice",
                label = "Select States to Cluster",
                choices = state.name,
                selected = state.name,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
            ),
            #checkboxGroupInput("state_choice", 
             #                  label = NULL, 
              #                 choices = state.name,
               #                selected = state.name,
                #               ),
        
        downloadButton("downloadData", "Download Cluster Table"),
        
    width = 3),
    mainPanel(
        plotOutput('plot1'),
        plotOutput('plot2'),
        plotOutput('ggpairsplot'),
        fluidRow(
            splitLayout(cellArgs = list(style = "overflow-x: hidden;"),
                        plotOutput('plot3'),
                        plotOutput('plot4'))
        ),
        DT::dataTableOutput("table"),
        renderText("Data Sources"),
        helpText("Energy-Related CO2 Emission Data Tables, Table 4: 2017 State energy-related carbon dioxide emissions by sector: https://www.eia.gov/environment/emissions/state/.
State Population Totals - US Census Bureau: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html.
GDP by State - Bureau of Economic Analysis: https://www.bea.gov/data/gdp/gdp-state.
FiveThirtyEight's Partisan Lean: https://github.com/fivethirtyeight/data/tree/master/partisan-lean."),
    )
)
