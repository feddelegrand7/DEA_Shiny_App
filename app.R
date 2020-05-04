library(shiny)
library(shinycssloaders)
library(data.table)
library(tidyverse)
library(shinyWidgets)
library(bsplus)



ui <- fluidPage(

tabsetPanel(id = "tab1", 
            
            

# Loading the data --------------------------------------------------------

            
        tabPanel(title = "Loading Data", 
                 
                 
        sidebarLayout(
            
            
            sidebarPanel(
                
                
            fileInput(inputId = "file1", 
                      label = "Load a csv file", 
                      accept = c("csv", "CSV", 
                                 "csv/text", 
                                 "Comma Separated Values")), 
            tags$hr(),
            
            numericInput(inputId = "num_observ", 
                         label = "Number of rows to display", 
                         value = 5, 
                         min = 1)
                
            ), 
            
            
            mainPanel(
                
            tags$h2("An overview of the data frame"),  
            
            tableOutput(outputId = "tbl_loading")
                
                
                
                
                
            )
            
            
            
            
            
        )
                 
                 
                 
                 
                 
                 ),
            


# Model Tuning  -------------------------------------------------


tabPanel(title = "Model Tuning", 
         
         
         fluidRow(
             
             tags$h1("Model Tuning", align = "center"), 
             
             tags$hr(),
             
             column(4,
             awesomeCheckboxGroup(
                 inputId = "input_select",
                 label = "Select the Input Variables",
                 choices = "")),
         
         column(4,
             awesomeCheckboxGroup(
                 inputId = "output_select",
                 label = "Select the Output Variables",
                 choices = "")), 
             
        column(4,
               
               pickerInput(
                   inputId = "ID_choose",
                   label = "Select the identification column", 
                   choices = "",
                   options = list(
                       style = "btn-danger")),
               
               pickerInput(
                   inputId = "RTS_choose",
                   label = "Select the Returns to Scale assumption", 
                   choices = c("crs", "vrs", "irs",  "drs", "add", "fdh"),
                   options = list(
                       style = "btn-danger")),
               
               actionButton(inputId = "help", label = "Help"), 

               br(),
               
               br(),
               
               pickerInput(
                   inputId = "orientation_choose",
                   label = "Select the orientation", 
                   choices = c("input", "ouput"),
                   options = list(
                       style = "btn-danger"))
               
               
               
               
               
                
                    )),
        br(), 
        
        br(), 
        
        br(), 
        
        br(), 
        
        br(), 
        
        br(),
        
        br(),
        
        br(),
        
        br(),
        
        br(),


        fluidRow(
            
            column(4, ""), 
            
            column(4, actionButton(inputId = "btn_calc1", 
                                   label = "Calculate Efficiency")), 
            
            column(4, "")
            
            
        ),
        
        br(), 
        
        br(), 
        
        br(), 
        
        br()
         
         
         
         ),



# Results -----------------------------------------------------------------

tabPanel(title = "Efficiency Results", 
    
    
DT::dataTableOutput("eff_results1")
    
    
    
    
    
    
    
    
)
            
            
            
            
            
            
      
            )
    
)






# Results -----------------------------------------------------------------




############################ SERVER #######################################

# -------------------------------------------------------------------------






server <- function(input, output, session) {
    




df <- reactive({
    
    req(input$file1)
    
    data <- fread(input$file1$datapath)
    
    
    updateAwesomeCheckboxGroup(session = session,
                      inputId = "input_select", 
                      label = "Select the Input Variables", 
                      choices = data %>% names(),
                      selected = NULL, 
                      status = "danger", 
                      inline = F)
    
    updateAwesomeCheckboxGroup(session = session,
                               inputId = "output_select", 
                               label = "Select the Output Variables", 
                               choices = data %>% names(),
                               selected = NULL, 
                               status = "danger", 
                               inline = F)
    
    updatePickerInput(session = session, 
                      inputId = "ID_choose", 
                      label = "Select the Identification column", 
                      choices = data %>% names())
    
    
    
    
    
    
    return(data)
    
})


output$tbl_loading <- renderTable({
        
        
        
        
    head(df(), n = input$num_observ)
        
        
        
        
    })
    

observeEvent(input$help, {
    showModal(modalDialog(
        title = "Help",
        HTML("
             
<ul>

<li>

crs: Constant returns to scale, convexity and free disposability

</li>

<li>

vrs: Variable returns to scale, convexity and free disposability

</li>

<li>

irs: Increasing returns to scale (up-scaling, but not down-scaling), additivity, and free disposability

</li>

<li>

drs: Decreasing returns to scale, convexity, down-scaling and free disposability
</li>


<li> 

add: Additivity (scaling up and down, but only with integers), and free disposability; also known af replicability and free disposability, the free disposability and replicability hull (frh) -- no convexity assumption

</li>


<li>

fdh: Free disposability hull, no convexity assumption

</li>


</ul>


<a href='https://cran.r-project.org/web/packages/Benchmarking/Benchmarking.pdf'> Source</a>

             
             
             ")
    ))
})

observeEvent(input$btn_calc1, {
    
output$eff_results1 <- DT::renderDataTable({
    
inputs <- df() %>% select(input$input_select)
outputs <- df() %>% select(input$output_select)

r_eff <- Benchmarking::dea(X = inputs, 
                         Y = outputs, 
                         RTS = input$RTS_choose, 
                         ORIENTATION = "in")

results <- tibble(score = r_eff$eff)

results
    
    
})
    
    
    
    
})




}

shinyApp(ui = ui, server = server)
