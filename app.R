library(shiny)
source("data_manipulation_copy.R")

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Data"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            tags$hr(),
 ################################################################################           
            titlePanel("Functions"),
            
            radioButtons("matSamp", "materialSampleType",
                         choices = c(Yes = "matSamp_yes",
                                     No = "matSamp_no"),
                         selected = "No"),


            conditionalPanel(
                condition = "input.matSamp == 'matSamp_yes'",
                    textInput("matSamp_col", "Column Name", value = "", width = NULL,
                          placeholder = NULL),
                    textInput("matSamp_check", "Values to check for(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL),
                    textInput("matSamp_replace", "Replacement Values(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL)
            ),
            
################################################################################            
            radioButtons("sex", "Sex",
                         choices = c(Yes = "sex_yes",
                                     No = "sex_no"),
                         selected = "No"),
            
            conditionalPanel(
                condition = "input.sex == 'sex_yes'",
                textInput("sex_col", "Column Name", value = "", width = NULL,
                          placeholder = NULL),
            ),  
################################################################################            
            
            radioButtons("side", "measurementSide",
                         choices = c(Yes = "side_yes",
                                     No = "side_no"),
                         selected = "No"),
            
            conditionalPanel(
                condition = "input.side == 'side_yes'",
                textInput("side_col", "Column Name", value = "", width = NULL,
                          placeholder = NULL),
            ),
################################################################################
            
            radioButtons("melt", "Melt Data",
                         choices = c(Yes = "melt_yes",
                                    No = "melt_no"),
                         selected = "No"),
            conditionalPanel(
                condition = "input.melt == 'melt_yes'",
                textInput("melt_cols", "Column Names(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL),
                radioButtons("measUnit", "Measurement Unit",
                             choices = c(Yes = "meas_yes",
                                         No = "meas_no"),
                             selected = "No"),
            ),

################################################################################
            radioButtons("age", "lifeStage",
                         choices = c(Yes = "age_yes",
                                     No = "age_no"),
                         selected = "No"),
            
            conditionalPanel(
                condition = "input.age == 'age_yes'",
                textInput("age_col", "Column Name", value = "", width = NULL,
                          placeholder = NULL),
                textInput("adult", "All adult names(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL),
                textInput("juv", "All juvenile names(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL)
            ),
################################################################################
            radioButtons("repro", "reproductiveCondition",
                         choices = c(Yes = "repro_yes",
                                     No = "repro_no"),
                         selected = "No"),
            
            conditionalPanel(
                condition = "input.repro == 'repro_yes'",
                textInput("repro_col", "Column Name", value = "", width = NULL,
                          placeholder = NULL),
                textInput("repro_val", "All reproductive names(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL),
                textInput("non_val", "All non-reproductive names(Seperate with commas)", value = "", width = NULL,
                          placeholder = NULL)
            ),
################################################################################
            
            radioButtons("temp", "Template Match",
                         choices = c(Yes = "temp_yes",
                                     No = "temp_no"),
                         selected = "No"),
            
            conditionalPanel(
                condition = "input.temp == 'temp_yes'",
                fileInput("temp_file", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("temp_header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("temp_sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons("temp_quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                
                textInput("temp_col_old", "Column Name for Old Columns", value = "", width = NULL,
                          placeholder = NULL),
                textInput("temp_col_new", "Column Name for New Columns", value = "", width = NULL,
                          placeholder = NULL),
                
                # Horizontal line ----
                tags$hr(),
            ) 
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            titlePanel("Data Pre-cleaning"),
            # Output: Data file ----
            tableOutput("contents"),
            titlePanel("Data After Cleaning"),
            tableOutput("clean_data")
        )
        
)
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    output$clean_data <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        # 
        # template <- read.csv(input$temp_file$datapath, 
        #                      header = TRUE, sep = ",")
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        
        df <- delete_empty_r_and_c(df)
        
        continue <- 5
        
        if(input$matSamp == "matSamp_yes"){
            matSamp_check <- unlist(strsplit(input$matSamp_check, ","))
            matSamp_replace <- unlist(strsplit(input$matSamp_replace, ",")) 
            df <- materialSampleType(df, input$matSamp_col, matSamp_check, matSamp_replace)
        }
        else{
            continue = continue + 1
        }
        
        if(input$sex == "sex_yes"){
            df <- sex(df, input$sex_col)
        }
        else{
            continue = continue + 1
        }
        
        if(input$side == "side_yes"){
            df <- measurementSide(df, input$side_col)
        }
        else{
            continue = continue + 1
        }
        
        if(input$age == "age_yes"){
            age_adu <- unlist(strsplit(input$adult, ","))
            age_juv <- unlist(strsplit(input$juv, ","))
            df <- lifeStage(df, input$matSamp_col, age_adu, age_juv)
        }
        else{
            continue = continue + 1
        }
        
        if(input$repro == "repro_yes"){
            repro1 <- unlist(strsplit(input$repro_val, ","))
            nonrepro1 <- unlist(strsplit(input$non_val, ","))
            df <- reproductiveCondition(df, input$matSamp_col, repro1, nonrepro1)
        }
        else{
            continue = continue + 1
        }
        
        if(input$melt == "melt_yes"){
            meltcols <- unlist(strsplit(input$melt_cols, ","))
            df <- melt_data(df, meltcols)
            if(input$measUnit == "meas_yes"){
                df <- measurementUnit(df)
            }
        }
        else{
            continue = continue + 1
        }
        
        if(input$temp == "temp_yes"){
            req(input$temp_file)
            mapping_template <- read.csv(input$temp_file$datapath,
                                         header = input$temp_header,
                                         sep = input$temp_sep,
                                         quote = input$temp_quote)
            df <- template_match(df, mapping_template, input$temp_col_old, input$temp_col_new)
        }
        else{
            continue = continue + 1
        }
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
}
# Run the app ----
shinyApp(ui, server)
