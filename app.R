library(shiny)
library(shinymanager)
require(RSQLite)
library(DBI)
library(xtable)
library(DT)
library(dplyr)
library(httr)
library(leaflet)
library(maps)
library(data.table)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(sqldf)
library(shinyWidgets)
library(stringi)
library(sjmisc)
library(shinydashboard)
library(shinyBS)
library(collapsibleTree)
library(stringi)
library(sjmisc)
library(lubridate)
library(shinyFeedback)
library(keyring)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 600000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
#credentials <- data.frame(
#    user = c( "demouser"),
#    password = c( "demouser"),
#    stringsAsFactors = FALSE
#)

dbinfo <- config::get()

ui <- secure_app(fluidPage(# classic app
                     headerPanel(h3(
                         'SEC POC Administration'
                     )),
                     
                         tabsetPanel(
                             type = "tabs",
                             tabPanel("Criteria Types",
                                      sidebarLayout(
                                          # Sidebar panel
                                          sidebarPanel(
                                              actionButton("edit_criteria_type", "Edit", width = '100px'),
                                              actionButton("add_criteria_type", "Add", width = '100px'),
                                              actionButton("delete_criteria_type", "Delete", width = '100px')
                                          )
                                          
                                          
                                          ,
                                          # main panel
                                          mainPanel(DTOutput("criteria_types_table")
                                          )
                                      )),
                             tabPanel("Trial Criteria by Type", 
                                      sidebarLayout(sidebarPanel(
                                          DTOutput("criteria_types_title_only")  
                                      ),
                                      mainPanel(DTOutput("trial_crit_by_type"))
                                      )
                                      
                                      ), #end of trial criteria per type 
                             tabPanel("Criteria Per Trial", tableOutput("table"))
                       
                     ))
                 ,
                 enable_admin = TRUE
                )

server <- function(input, output, session) {
    
    sessionInfo <- reactiveValues(
        criteria_type_selected_in_tab = NA,
        sessionCon = NA,
        df_crit_type_titles = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("criteria_type_id", "criteria_type_title")))) ,
        result_auth = NA
    )
    
    sessionInfo$result_auth <-
        secure_server(check_credentials = check_credentials(dbinfo$db_user_file, passphrase = dbinfo$passphrase), timeout = 10)
  #  sessionInfo$result_auth <- result_auth
    
   

    
    con = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
    
    #
    # Select the criteria types
    #
    
    crit_type_sql <-
        "select criteria_type_id, criteria_type_code, criteria_type_title, criteria_type_desc, criteria_type_active, criteria_type_sense
     from criteria_types order by criteria_type_id"
    df_crit_types <- dbGetQuery(con, crit_type_sql)
    
    criteria_types_dt <- datatable(df_crit_types,
                                   class = 'cell-border stripe compact wrap hover',
                                   selection = 'single',
                                   colnames = c(
                                       'Type ID',
                                       'Abbr',
                                       'Title',
                                       'Description',
                                       'Active',
                                       'Inc/Exc')
                                    )
    
    output$criteria_types_table <- DT::renderDataTable({criteria_types_dt})
            
    crit_type_titles_sql <-
        "select criteria_type_id,  criteria_type_title
     from criteria_types order by criteria_type_id"
    df_crit_type_titles <- dbGetQuery(con, crit_type_titles_sql)
    
    criteria_types_titles_dt <- datatable(df_crit_type_titles,
                                   class = 'cell-border stripe compact wrap hover',
                                   selection = 'single',
                                   colnames = c(
                                       'Type ID',
                                       'Title'
                                      ),
                                   options = list(
                                       escape = FALSE,
                                       searching = FALSE,
                                       paging = FALSE,
                                       info = FALSE,
                                       columnDefs = list(
                                           # Initially hidden columns
                                           list(
                                               visible = FALSE,
                                               
                                               targets = c(0,1)
                                           )))
    )
    
    output$criteria_types_title_only <- DT::renderDataTable({criteria_types_titles_dt})
    DBI::dbDisconnect(con)
    
    #### Row selected in trial criteria by type table
    
    observeEvent(input$criteria_types_title_only_rows_selected, {
        print("criteria types row selected ")
        print(input$criteria_types_title_only_rows_selected)
        if (!is.null(input$criteria_types_title_only_rows_selected) ) {
            print(paste("user is ",sessionInfo$result_auth$user ))
            crit_type_sel <- df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
            trial_criteria_for_type_sql <- "select nct_id, criteria_type_id, trial_criteria_orig_text, 
                                   trial_criteria_refined_text, trial_criteria_expression, update_date, update_by 
                                from trial_criteria where criteria_type_id = ? order by nct_id"
            sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
            df_trial_criteria_for_type <- dbGetQuery(sessionCon, trial_criteria_for_type_sql,  params = c(crit_type_sel))
            DBI::dbDisconnect(sessionCon)
            
            trial_criteria_by_type_dt <- datatable(df_trial_criteria_for_type,
                                           class = 'cell-border stripe compact wrap hover',
                                           selection = 'single',
                                           width  = "90vw",
                                           colnames = c(
                                               'NCT ID',
                                               'Type ID',
                                               'Original Text',
                                               'Refined Text',
                                               'Expression',
                                               'Last Updated',
                                               'Last Updated By'),
                                           options = list(
                                               info = TRUE,
                                               searching = TRUE,
                                               autoWidth = TRUE,
                                               scrollX = TRUE,
                                               deferRender = TRUE,
                                               # scrollY = "400px",
                                               scrollY = "45vh",
                                               scrollCollapse = TRUE,
                                               paging = TRUE,
                                               #paging = TRUE,
                                               style = "overflow-y: scroll"
                                               )
            )
            output$trial_crit_by_type <- DT::renderDataTable({trial_criteria_by_type_dt})
        }
    } , ignoreNULL = FALSE )
    
}


shinyApp(ui = ui, server = server)