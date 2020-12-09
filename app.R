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

ui <- secure_app(
  fluidPage(
    useShinyjs(),
    useShinyFeedback(),
    
    #
    # Wire up the close button on the criteria types modal to fire a shiny event 
    # so the data can be processed 
    #
    
    tags$script('
  $( document ).ready(function() {
    $("#add_criteria_type_bsmodal").on("hidden.bs.modal", function (event) {
    x = new Date().toLocaleString();
    // window.alert("biomarker  modal was closed at " + x);
    Shiny.onInputChange("add_criteria_type_bsmodal_close",x);
  });
  })
  '),
    
    
    tags$head(tags$style(
      HTML('

                        .modal-lg {
                        width: 95vw; height: 95vh;

                        }
                      ')
    )),
    # classic app
    headerPanel(h3('SEC POC Administration')),
    
    # 
    # Criteria Type Modal
    #
    
    bsModal(
      "add_criteria_type_bsmodal",
      "Add criteria type",
      "add_criteria_type",
      size = "large",
      fluidPage(
        fluidRow(
          column( 
            6, textInput("criteria_type_code", "Code")),
          column(
            6, textInput("criteria_type_title", "Title"))
          )
        ,
        fluidRow(
          textAreaInput('criteria_type_desc',"Description", width = '100%')  %>%
            shiny::tagAppendAttributes(style = 'width: 100%;', align = 'center')
        )
        ,
        fluidRow(
          column(
            6, radioButtons("criteria_type_active_rb","Active", choices = c("Y" = "Y", "N" = "N"), inline = TRUE)
          )
          ,
          column(
            6, radioButtons("criteria_type_sense_rb","Active", choices = c("Inclusion" = "Inclusion", "Exclusion" = "Exclusion"), inline = TRUE)
          )
        )
        ,
        fluidRow(
          column(1, align = 'right',  offset = 11,actionButton("criteria_type_save", label='Save'))
        )
      )
    ),
    bsModal(
      "add_criteria_per_trial_bsmodal",
      "Add criteria for a trial",
      "add_criteria_per_trial",
      size = "large",
      fluidPage(
        id = "add_criteria_per_trial_bsmodal",
        fluidRow(column(
          6,  textInput("criteria_per_trial_nct_id", "NCT ID")
        )
        
        ,
        column(
          6,
          selectizeInput(
            "criteria_type_typer",
            label = "Criteria Type",
            NULL,
            multiple = FALSE
          )
        ))
        ,
        fluidRow(
          textAreaInput(
            "criteria_per_trial_original_text",
            "Original Text",
            value = "",
            rows = 7,
            resize = "both"
          ) %>%
            shiny::tagAppendAttributes(style = 'width: 90%;', align = 'center')
        ),
        
        fluidRow(
          textAreaInput(
            'criteria_per_trial_refined_text',
            'Refined Text',
            width = '90%' ,
            rows = 7,
            resize = "both"
          ) %>%
            shiny::tagAppendAttributes(style = 'width: 90%;', align = 'center')
        ),
        fluidRow(
        
          textAreaInput(
            'criteria_per_trial_expression',
            'Expression',
            width = '90%' ,
            rows = 7,
            resize = "both"
          ) %>%
            shiny::tagAppendAttributes(style = 'width: 90%;', align = 'center')
          
          
        )
       , 
       fluidRow(
         column(1, alight='right', actionButton("criteria_test_eval", label = "Test Expression")),
         column(1, align = 'right',  offset = 10,actionButton("criteria_save", label='Save'))
       )
      )
    ),
    
    tabsetPanel(
      type = "tabs",
      tabPanel("Criteria Types",
               sidebarLayout(
                 # Sidebar panel
                 sidebarPanel(
                   actionButton("add_criteria_type", "Add", width = '100px'),
                   br(),
                   actionButton("edit_criteria_type", "Edit", width = '100px'),
                   br(),
                   actionButton("delete_criteria_type", "Delete", width = '100px')
                 )
                 
                 
                 ,
                 # main panel
                 mainPanel(DTOutput("criteria_types_table"))
               )),
      tabPanel(
        "Trial Criteria by Type",
        sidebarLayout(
          sidebarPanel(
            DTOutput("criteria_types_title_only"),
            br(),
            actionButton("add_criteria_by_type", "Add", width = '100px'),
            br(),
            actionButton("edit_criteria_by_type", "Edit", width = '100px'),
            br(),
            actionButton("delete_criteria_by_type", "Delete", width = '100px'),
            br(),
            br(),
            fileInput("trial_criteria_csv_file", "Upload CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            )
            
          ),
          mainPanel(DTOutput("trial_crit_by_type"))
        )
        
      ),
      #end of trial criteria per type
      tabPanel("Criteria Per Trial",
               sidebarLayout(
                 sidebarPanel(
                   DTOutput("criteria_nct_ids"),
                   br(),
                   actionButton("add_criteria_per_trial", "Add", width = '100px'),
                   br(),
                   actionButton("edit_criteria_per_trial", "Edit", width = '100px'),
                   br(),
                   actionButton("delete_criteria_per_trial", "Delete", width = '100px')
                 ),
                 mainPanel(DTOutput("trial_crit_per_trial"))
               ))
      
    )
  )
  ,
  enable_admin = TRUE
)


server <- function(input, output, session) {
  sessionInfo <- reactiveValues(
    criteria_type_selected_in_tab = NA,
    criteria_type_modal_state = "Neutral",
    criteria_type_modal_type_id = NA,
    sessionCon = NA,
    df_crit_type_titles = data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c(
        "criteria_type_id", "criteria_type_title"
      ))
    )) ,
    result_auth = NA
  )
  
  sessionInfo$result_auth <-
    secure_server(
      check_credentials = check_credentials(dbinfo$db_user_file, passphrase = dbinfo$passphrase),
      timeout = 10
    )
  #  sessionInfo$result_auth <- result_auth
  
  
  shinyjs::disable("edit_criteria_type")
  shinyjs::disable('edit_criteria_by_type')
  shinyjs::disable('delete_criteria_by_type')
  shinyjs::disable('add_criteria_by_type')
  
  
  
  con = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
  
  #
  # Select the criteria types
  #
  
  crit_type_sql <-
    "select criteria_type_id, criteria_type_code, criteria_type_title, criteria_type_desc, criteria_type_active, criteria_type_sense
     from criteria_types order by criteria_type_id"
  df_crit_types <- dbGetQuery(con, crit_type_sql)
  
  criteria_types_dt <- datatable(
    df_crit_types,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    colnames = c('Type ID',
                 'Abbr',
                 'Title',
                 'Description',
                 'Active',
                 'Inc/Exc')
  )
  
  output$criteria_types_table <-
    DT::renderDataTable({
      criteria_types_dt
    })
  
  crit_type_titles_sql <-
    "select criteria_type_id,  criteria_type_title
     from criteria_types order by criteria_type_id"
  df_crit_type_titles <- dbGetQuery(con, crit_type_titles_sql)
  
  criteria_types_titles_dt <- datatable(
    df_crit_type_titles,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    colnames = c('Type ID',
                 'Title'),
    options = list(
      escape = FALSE,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(# Initially hidden columns
        list(
          visible = FALSE,
          
          targets = c(0, 1)
        ))
    )
  )
  
  output$criteria_types_title_only <-
    DT::renderDataTable({
      criteria_types_titles_dt
    })
  
  
  
  
  
  criteria_nct_ids_sql <-
    "select distinct nct_id from trial_criteria order by nct_id"
  df_criteria_nct_id <- dbGetQuery(con, criteria_nct_ids_sql)
  
  criteria_nct_ids_dt <- datatable(
    df_criteria_nct_id,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    colnames = c('NCT ID'),
    options = list(
      escape = FALSE,
      searching = TRUE,
      paging = TRUE,
      info = TRUE,
      columnDefs = list(# Initially hidden columns
        list(
          visible = FALSE,
          
          targets = c(0)
        )),
      scrollY = "40vh",
      scrollCollapse = TRUE,
      style = "overflow-y: scroll"
    )
  )
  
  output$criteria_nct_ids <-
    DT::renderDataTable({
      criteria_nct_ids_dt
    })
  
  DBI::dbDisconnect(con)
  
  #
  # Row selected in criteria types table
  #
  
  observeEvent(input$criteria_types_table_rows_selected,
               {
                 print("criteria type row selected")
                 if (!is.null(input$criteria_types_table_rows_selected)) {
                   shinyjs::enable("edit_criteria_type")
                   shinyjs::enable("delete_criteria_type")
                   
                 } else {
                   shinyjs::disable("edit_criteria_type")
                   shinyjs::disable("delete_criteria_type")
                 }
               },
               ignoreNULL = FALSE)
  
  # 
  # Add new criteria type button clicked
  #
  observeEvent(
    input$add_criteria_type,
    {
      print("add criteria type button clicked")
      print(sessionInfo$criteria_type_modal_state)
      #
      # If this is coming via an edit - we need to set the field values in the dialog
      #
      if(sessionInfo$criteria_type_modal_state == "Edit" &&  (!is.null(input$criteria_types_table_rows_selected)) )  {
        print(paste("selected row is ", input$criteria_types_table_rows_selected))
        print(paste("editing  ", df_crit_types[input$criteria_types_table_rows_selected,]))
        rowdf <- df_crit_types[input$criteria_types_table_rows_selected,]
        updateTextInput(session, 'criteria_type_code', value = rowdf$criteria_type_code)
        updateTextInput(session, 'criteria_type_title', value = rowdf$criteria_type_title)
        updateTextInput(session, 'criteria_type_desc', value = rowdf$criteria_type_desc)
        updateRadioButtons(session, 'criteria_type_active_rb', selected = rowdf$criteria_type_active)
        updateRadioButtons(session, 'criteria_type_sense_rb', selected = rowdf$criteria_type_sense)
        sessionInfo$criteria_type_modal_type_id <- rowdf$criteria_type_id  # Stick this in the reactive values to use in the update statement.
        #tags$script(HTML(
        #  "$('#add_criteria_type_bsmodal-modal-title').val('Edit Criteria Type');"
        #))
      } else {
        updateTextInput(session, 'criteria_type_code', value = '')
        updateTextInput(session, 'criteria_type_title', value = '')
        updateTextInput(session, 'criteria_type_desc', value = '')
        updateRadioButtons(session, 'criteria_type_active_rb', selected = 'N')
        updateRadioButtons(session, 'criteria_type_sense_rb', selected = 'Inclusion')
        sessionInfo$criteria_type_modal_type_id <- NA
        
      }
      
      
    }
  )
  
  # 
  # Edit new criteria type button clicked 
  # Set the data and click the add_criteria_type button
  #
  
  observeEvent(
    input$edit_criteria_type,
    {
      print("edit criteria type button clicked")
      print(sessionInfo$criteria_type_modal_state)
      sessionInfo$criteria_type_modal_state <- "Edit"
      click('add_criteria_type')
      
      
    })
  
  
  # 
  # criteria types modal closed event 
  #
  observeEvent(input$add_criteria_type_bsmodal_close,
               {
                 print("criteria type modal closed")
                 sessionInfo$criteria_type_modal_state <- "Neutral"
                 
               })
  
  
  observeEvent(
    input$criteria_type_save, 
    {
      print("criteria type save button clicked")
    }
  )
  #----------------------------------------------------------------
  #---------------------------------------------------------------
  
  #
  # Row selected in trial criteria table by types 
  #
  observeEvent(input$trial_crit_by_type_rows_selected, {
  if (!is.null(input$trial_crit_by_type_rows_selected)) {
    shinyjs::enable('edit_criteria_by_type')
    shinyjs::enable('delete_criteria_by_type')
  } else {
    shinyjs::disable('edit_criteria_by_type')
    shinyjs::disable('delete_criteria_by_type')
  }
    
  },
  ignoreNULL= FALSE 
  )
  #
  # Row selected in trial criteria by type table
  #
  
  observeEvent(input$criteria_types_title_only_rows_selected,
               {
                 print("criteria types row selected ")
                 print(input$criteria_types_title_only_rows_selected)
                 if (!is.null(input$criteria_types_title_only_rows_selected)) {
                   shinyjs::enable('add_criteria_by_type')
                   
                  
                   
                   print(paste("user is ", sessionInfo$result_auth$user))
                   crit_type_sel <-
                     df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
                   trial_criteria_for_type_sql <-
                     "select nct_id, criteria_type_id, trial_criteria_orig_text,
                                   trial_criteria_refined_text, trial_criteria_expression, update_date, update_by
                                from trial_criteria where criteria_type_id = ? order by nct_id"
                   sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                   df_trial_criteria_for_type <-
                     dbGetQuery(sessionCon,
                                trial_criteria_for_type_sql,
                                params = c(crit_type_sel))
                   DBI::dbDisconnect(sessionCon)
                   
                   trial_criteria_by_type_dt <-
                     datatable(
                       df_trial_criteria_for_type,
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
                         'Last Updated By'
                       ),
                       options = list(
                         info = TRUE,
                         searching = TRUE,
                         autoWidth = TRUE,
                         scrollX = TRUE,
                         deferRender = TRUE,
                         scrollY = "45vh",
                         scrollCollapse = TRUE,
                         paging = TRUE,
                         style = "overflow-y: scroll"
                       )
                     )
                   output$trial_crit_by_type <-
                     DT::renderDataTable({
                       trial_criteria_by_type_dt
                     })
                 } else {
                   shinyjs::disable('add_criteria_by_type')
                   
                 }
               } ,
               ignoreNULL = FALSE)
  #
  
  #---------------------------------------------------------
  #---------------------------------------------------------
  
  #
  # Row selected in trial criteria by trial
  #
  
  observeEvent(input$criteria_nct_ids_rows_selected, {
    print("nct_id  row selected ")
    if (!is.null(input$criteria_nct_ids_rows_selected)) {
      # a NCT ID is selected
      nct_id_sel <-
        df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
      print(paste("nct_id selected:", nct_id_sel))
      trial_crit_for_ncit_id_sql <-
        "select tc.nct_id, ct.criteria_type_title, tc.trial_criteria_orig_text, tc.trial_criteria_refined_text,
tc.trial_criteria_expression, tc.update_date, tc.update_by
from trial_criteria tc join criteria_types ct on tc.criteria_type_id= ct.criteria_type_id
where tc.nct_id = ?"
      sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      df_trial_criteria_for_nct_id <-
        dbGetQuery(sessionCon,
                   trial_crit_for_ncit_id_sql,
                   params = c(nct_id_sel))
      
      DBI::dbDisconnect(sessionCon)
      
      trial_criteria_for_nct_id_dt <-
        datatable(
          df_trial_criteria_for_nct_id,
          class = 'cell-border stripe compact wrap hover',
          selection = 'single',
          width  = "90vw",
          colnames = c(
            'NCT ID',
            'Type',
            'Original Text',
            'Refined Text',
            'Expression',
            'Last Updated',
            'Last Updated By'
          ),
          options = list(
            info = TRUE,
            searching = TRUE,
            autoWidth = TRUE,
            scrollX = TRUE,
            deferRender = TRUE,
            scrollY = "45vh",
            scrollCollapse = TRUE,
            paging = TRUE,
            style = "overflow-y: scroll"
          )
        )
      output$trial_crit_per_trial <-
        DT::renderDataTable({
          trial_criteria_for_nct_id_dt
        })
      
      
    }
    
  }
  ,
  ignoreNULL = FALSE) #
  
  observeEvent(input$add_criteria_per_trial, {
    print("Add criteria per trial")
  })
  
  #-------------------------
  
  #
  # Disable or enable the test button based upon whether there is anything in the input field
  #
  observeEvent(input$criteria_per_trial_expression , {
    print(input$criteria_per_trial_expression)
    
    if(!is.null(input$criteria_per_trial_expression) &&  nchar(input$criteria_per_trial_expression[1]) > 0) {
      shinyjs::enable('criteria_test_eval')
    }  else {
      shinyjs::disable('criteria_test_eval')
    }
    
  }, ignoreNULL = FALSE)
}


shinyApp(ui = ui, server = server)