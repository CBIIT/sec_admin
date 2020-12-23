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
#library(keyring)
library(shinyalert)

library(parsedate)

source('eval_prior_therapy_app.R')
source('check_if_any.R')
dbinfo <- config::get()

ui <- secure_app(
  fluidPage(
    useShinyjs(),
    useShinyFeedback(),
    useShinyalert(),
    
    #
    # Wire up the close button on the bsmodals to fire a shiny event 
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
    
    tags$script('
  $( document ).ready(function() {
    $("#add_criteria_per_trial_bsmodal").on("hidden.bs.modal", function (event) {
    x = new Date().toLocaleString();
    // window.alert("biomarker  modal was closed at " + x);
    Shiny.onInputChange("add_criteria_per_trial_bsmodal_close",x);
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
      "Add /Edit criteria type",
      "add_criteria_type",
      size = "large",
      fluidPage(
        bsAlert('criteria_type_modal_alert'),
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
          column(2, align = 'right',  offset = 10,actionButton("criteria_type_save", label='Save and Close'))
        )
      ),
      tags$head(tags$style("#add_criteria_type_bsmodal .modal-footer{ display:none}"))
    ),
    bsModal(
      "add_criteria_per_trial_bsmodal",
      "Add / Edit criteria",
      "add_criteria_per_trial",
      size = "large",
      fluidPage(
        id = "add_criteria_per_trial_bsmodal",
        bsAlert('criteria_modal_alert'),
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
         column(2, align = 'right',  offset = 9,actionButton("criteria_save", label='Save and Close'))
       )
      ),
      
      tags$head(tags$style("#add_criteria_per_trial_bsmodal .modal-footer{ display:none}"))
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
                      accept = ".csv"
            )
            
          ),
          mainPanel(DTOutput("trial_crit_by_type"))
        )
        
      )      
      ,
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
    df_trial_criteria_for_type = NA,
    df_trial_criteria_for_nct_id = NA,
    criteria_type_selected_in_tab = NA,
    criteria_type_modal_state = "Neutral",
    criteria_type_modal_type_id = NA,
    criteria_modal_state = "Neutral",
    sessionCon = NA,
    df_crit_type_titles = data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c(
        "criteria_type_id", "criteria_type_title"
      ))
    )) ,
    result_auth = NA,
    refresh_criteria_types_counter = 0,
    refresh_criteria_counter = 0,
    df_crit_types = NA,
    df_crit_type_titles = NA,
    df_criteria_nct_id = NA
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
  shinyjs::disable('edit_criteria_per_trial')
  shinyjs::disable('delete_criteria_per_trial')
  
  
  
  con = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
  
  #
  # Select the criteria types
  #
  
  crit_type_sql <-
    "select criteria_type_id, criteria_type_code, criteria_type_title, criteria_type_desc, criteria_type_active, criteria_type_sense
     from criteria_types order by criteria_type_id"
  
  criteria_nct_ids_sql <-
    "select distinct nct_id from trial_criteria order by nct_id"
 
  ## 
  ## Observe for refetching the new criteria types when they have changed.
  ##
  
  observe({
    scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
    sessionInfo$df_crit_types <- dbGetQuery(scon, crit_type_sql)
    DBI::dbDisconnect(scon)
    
    criteria_types_dt <- datatable(
      sessionInfo$df_crit_types,
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
  })
  
  crit_type_titles_sql <-
    "select criteria_type_id,  criteria_type_title
     from criteria_types order by criteria_type_id"
  
  observe({
    scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
    
    sessionInfo$df_crit_type_titles <- dbGetQuery(scon, crit_type_titles_sql)
    DBI::dbDisconnect(scon)
    
    updateSelectizeInput(
      session,
      'criteria_type_typer',
      choices = sessionInfo$df_crit_type_titles$criteria_type_title ,
      server = TRUE
    )
    
    criteria_types_titles_dt <- datatable(
      sessionInfo$df_crit_type_titles,
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
    
  })
  
  
  ## HH you are here -- wrap in observer in case nct_ids change 
  observe( {
   
    sessionInfo$df_criteria_nct_id <- dbGetQuery(con, criteria_nct_ids_sql)
    
    criteria_nct_ids_dt <- datatable(
      sessionInfo$df_criteria_nct_id,
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
  }
  )
  
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
        print(paste("editing  ", sessionInfo$df_crit_types[input$criteria_types_table_rows_selected,]))
        rowdf <- sessionInfo$df_crit_types[input$criteria_types_table_rows_selected,]
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
  
  observeEvent( 
    input$delete_criteria_type,
    {
      print("delete criteria type clicked")
      #
      # See if there are criteria for this type.  If not, delete it without prompt.  If so, 
      # make sure this is what the user really wants to do.
      #
      if (!is.null(input$criteria_types_table_rows_selected)) {
        num_crits_sql <- "select count(*) as num_recs from trial_criteria where criteria_type_id = ?"
        rowdf <- sessionInfo$df_crit_types[input$criteria_types_table_rows_selected,]
        sessionInfo$criteria_type_modal_type_id <- rowdf$criteria_type_id  
        print(paste("delete - checking num recs for ",sessionInfo$criteria_type_modal_type_id))
        scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
        
        num_crits <- dbGetQuery(scon, num_crits_sql, 
                        params = c( sessionInfo$criteria_type_modal_type_id)) 
        DBI::dbDisconnect(scon)
        print(paste("there are ", num_crits$num_recs , " records "))
        if (num_crits$num_recs == 0 ) {
          message_str <- paste("Do you want to delete the ", rowdf$criteria_type_title, " criteria type?")
        } else {
          message_str <- paste("The", rowdf$criteria_type_title, " criteria type has ", num_crits$num_recs, " criteria records - deleting this type will delete those records as well.  Are you sure?")
        }
        shinyalert("Confirm delete", message_str , 
                   type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                   callbackR = function(x) {
                     if (x == TRUE) {
                       if ( num_crits$num_recs == 0) {
                        print("we need to delete ")
                        scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                        rs <- dbExecute(scon, "delete from criteria_types where criteria_type_id = ? ", 
                                        params = c(sessionInfo$criteria_type_modal_type_id) )
                        DBI::dbDisconnect(scon)
                        sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1

                         
                       } else {
                         shinyalert("Are you really sure?", 
                                    paste("Are you really sure you want to delete the criteria type of ",rowdf$criteria_type_title, " with ",num_crits$num_recs, " criteria records? "),
                         type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                         callbackR = function(x) {
                           if (x == TRUE) {
                             print("second confirm of delete")
                             scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                             rs <- dbExecute(scon, "delete from trial_criteria where criteria_type_id = ? ", 
                                             params = c(sessionInfo$criteria_type_modal_type_id) )
                             rs <- dbExecute(scon, "delete from criteria_types where criteria_type_id = ? ", 
                                             params = c(sessionInfo$criteria_type_modal_type_id) )
                             DBI::dbDisconnect(scon)
                             sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
                           }
                         })
                       }
                     } else {
                       print('No delete')
                     }
                   })
        
        
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
                 shinyBS::closeAlert(session, "criteria_type_code_alert")
                 shinyBS::closeAlert(session, "criteria_type_title_alert")
               })
  
  

  ##
  # Save criteria types
  #
  
  observeEvent(
    input$criteria_type_save, 
    {
      input_error <- FALSE
      print("criteria type save button clicked")
      print(paste("state - ", sessionInfo$criteria_type_modal_state ))
      # check for valid information
      if(input$criteria_type_code == '' ) {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_code_alert", content = "Please enter a criteria code", style = 'danger')
        input_error <- TRUE
      } else {
        shinyBS::closeAlert(session, "criteria_type_code_alert")
      }
      
      if (input$criteria_type_title == '') {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_title_alert", content = "Please enter a title", style = 'danger')
        input_error <- TRUE 
      } else {
        shinyBS::closeAlert(session, "criteria_type_title_alert")
      }
      
      if (input$criteria_type_desc == '') {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_desc_alert", content = "Please enter a description", style = 'danger')
        input_error <- TRUE 
      } else {
        shinyBS::closeAlert(session, "criteria_type_desc_alert")
      }
      
      if(input_error) {
        print("input error, returning")
        return
      }
      
      if (sessionInfo$criteria_type_modal_state == 'Neutral' && input_error == FALSE) {
        # insert
        print("criteria type need to do an insert")
        ct_insert <- "insert into criteria_types(criteria_type_code, criteria_type_title, criteria_type_desc,
        criteria_type_active, criteria_type_sense) values (?,?,?,?,?)"
        scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
        rs <- dbExecute(scon, ct_insert, 
                           params = c(input$criteria_type_code,input$criteria_type_title,input$criteria_type_desc, 
                                      input$criteria_type_active_rb, input$criteria_type_sense_rb))
        print(rs)
        DBI::dbDisconnect(scon)
        sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
        # save was successful, and close the panel and clear the fields
        toggleModal(session,  "add_criteria_type_bsmodal", toggle = "close")
        
      } else if (input_error == FALSE ) {
        print("criteria type need to do an update")
        print(paste("need to update type ", sessionInfo$criteria_type_modal_type_id))
        ct_update_sql <- "update criteria_types set criteria_type_code = ?, criteria_type_title = ?, 
        criteria_type_desc = ?, criteria_type_active = ?, criteria_type_sense = ? where criteria_type_id = ?"
        scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
        
        rs <- dbExecute(scon, ct_update_sql, 
                        params = c(input$criteria_type_code,input$criteria_type_title,input$criteria_type_desc, 
                                   input$criteria_type_active_rb, input$criteria_type_sense_rb, sessionInfo$criteria_type_modal_type_id)) 
        DBI::dbDisconnect(scon)
        sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
        # save was successful, and close the panel and clear the fields
        toggleModal(session,  "add_criteria_type_bsmodal", toggle = "close")
      }
    }
  )
  
  observeEvent(
    sessionInfo$refresh_criteria_types_counter,
    {
      print("need to refresh criteria types")
      scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      sessionInfo$df_crit_types <- dbGetQuery(scon, crit_type_sql)
      sessionInfo$df_crit_type_titles <- dbGetQuery(scon, crit_type_titles_sql)
      
      # MAYBE
      sessionInfo$df_criteria_nct_id <- dbGetQuery(scon, criteria_nct_ids_sql)
      
      DBI::dbDisconnect(scon)
      
    })
  #----------------------------------------------------------------
  #---------------------------------------------------------------
  
  # 
  # criteria  modal closed event 
  #
  observeEvent(input$add_criteria_per_trial_bsmodal_close,
               {
                 print("criteria  modal closed")
                 sessionInfo$criteria_modal_state <- "Neutral"
                 shinyBS::closeAlert(session, "criteria_need_nct_id_alert")
                 shinyBS::closeAlert(session, "criteria_per_trial_refined_text_alert")
                 shinyBS::closeAlert(session, "criteria_per_trial_expression_alert")
                 
                 
               })
  
  
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
  
  observeEvent(input$criteria_types_title_only_rows_selected | sessionInfo$refresh_criteria_counter,
               {
                 print("criteria types row selected ")
                 print(input$criteria_types_title_only_rows_selected)
                 if (!is.null(input$criteria_types_title_only_rows_selected)) {
                   shinyjs::enable('add_criteria_by_type')
                   
                  
                   
                   print(paste("user is ", sessionInfo$result_auth$user))
                   crit_type_sel <-
                     sessionInfo$df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
                   trial_criteria_for_type_sql <-
                     "select nct_id, criteria_type_id, trial_criteria_orig_text,
                                   trial_criteria_refined_text, trial_criteria_expression, update_date, update_by
                                from trial_criteria where criteria_type_id = ? order by nct_id"
                   sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                   sessionInfo$df_trial_criteria_for_type <-
                     dbGetQuery(sessionCon,
                                trial_criteria_for_type_sql,
                                params = c(crit_type_sel))
                   DBI::dbDisconnect(sessionCon)
                   
                   trial_criteria_by_type_dt <-
                     datatable(
                       sessionInfo$df_trial_criteria_for_type,
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
  
  observeEvent(input$criteria_nct_ids_rows_selected  | sessionInfo$refresh_criteria_counter , {
    print("nct_id  row selected ")
    if (!is.null(input$criteria_nct_ids_rows_selected)) {
      # a NCT ID is selected
      nct_id_sel <-
        sessionInfo$df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
      print(paste("nct_id selected:", nct_id_sel))
      trial_crit_for_ncit_id_sql <-
        "select tc.nct_id, ct.criteria_type_title, tc.trial_criteria_orig_text, tc.trial_criteria_refined_text,
tc.trial_criteria_expression, tc.update_date, tc.update_by
from trial_criteria tc join criteria_types ct on tc.criteria_type_id= ct.criteria_type_id
where tc.nct_id = ?"
      sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      sessionInfo$df_trial_criteria_for_nct_id <-
        dbGetQuery(sessionCon,
                   trial_crit_for_ncit_id_sql,
                   params = c(nct_id_sel))
      
      DBI::dbDisconnect(sessionCon)
      
      trial_criteria_for_nct_id_dt <-
        datatable(
          sessionInfo$df_trial_criteria_for_nct_id,
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
  
  #-------------------------------------------------------------------------------------------
  #
  # Add criteria button is pressed.
  # The same dialog is used for edit and additions - and the reactive value stored in 
  # sesssionInfo$criteria_modal_state indicates the origin of this. 
  #
  #-------------------------------------------------------------------------------------------
  
  observeEvent(input$add_criteria_per_trial, {
    print("Add criteria per trial")
    if(sessionInfo$criteria_modal_state == 'AddByType') {
      updateSelectizeInput(session, 'criteria_type_typer', 
                           selected=  sessionInfo$df_crit_type_titles$criteria_type_title[[input$criteria_types_title_only_rows_selected]])
      sessionInfo$criteria_type_id <- sessionInfo$df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
      updateTextInput(session, 'criteria_per_trial_nct_id', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_expression', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_original_text', value = '')
      
    } else if (sessionInfo$criteria_modal_state == 'EditByType') {
      updateSelectizeInput(session, 'criteria_type_typer', 
                           selected=  sessionInfo$df_crit_type_titles$criteria_type_title[[input$criteria_types_title_only_rows_selected]])
      sessionInfo$criteria_type_id <-  sessionInfo$df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
      #
      # Now get the items from the criteria table
      #
      df_sel_crit <- sessionInfo$df_trial_criteria_for_type[input$trial_crit_by_type_rows_selected,]
      updateTextInput(session, 'criteria_per_trial_nct_id', value = df_sel_crit$nct_id)
      updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = df_sel_crit$trial_criteria_refined_text)
      updateTextAreaInput(session, 'criteria_per_trial_expression', value = df_sel_crit$trial_criteria_expression)
      updateTextAreaInput(session, 'criteria_per_trial_original_text', value = df_sel_crit$trial_criteria_orig_text)
      #browser()
      
    } else if (sessionInfo$criteria_modal_state == 'Neutral') {
      #
      #Neutral - the click is from add criteria per trial
      #
      if (!is.null(input$criteria_nct_ids_rows_selected)) {
        # a NCT ID is selected
        nct_id_sel <-
          sessionInfo$df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
        print(paste("nct_id selected:", nct_id_sel))
        updateTextInput(session, 'criteria_per_trial_nct_id', value = nct_id_sel)
        updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = '')
        updateTextAreaInput(session, 'criteria_per_trial_expression', value = '')
        updateTextAreaInput(session, 'criteria_per_trial_original_text', value = '')
      } else {
        updateTextInput(session, 'criteria_per_trial_nct_id', value = '')
        updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = '')
        updateTextAreaInput(session, 'criteria_per_trial_expression', value = '')
        updateTextAreaInput(session, 'criteria_per_trial_original_text', value = '')
        
      }
    } else if (sessionInfo$criteria_modal_state == 'EditByTrial') {
      nct_id_sel <-
        sessionInfo$df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
      print(paste("nct_id selected:", nct_id_sel))
      df_sel_crit <- sessionInfo$df_trial_criteria_for_nct_id[input$trial_crit_per_trial_rows_selected,]
      updateSelectizeInput(session, 'criteria_type_typer', 
                           selected=  df_sel_crit$criteria_type_title)
      updateTextInput(session, 'criteria_per_trial_nct_id', value = nct_id_sel)
      updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = df_sel_crit$trial_criteria_refined_text)
      updateTextAreaInput(session, 'criteria_per_trial_expression', value = df_sel_crit$trial_criteria_expression)
      updateTextAreaInput(session, 'criteria_per_trial_original_text', value = df_sel_crit$trial_criteria_orig_text)
    }
  },
  label ="Add a criteria - update the text areas -- observeEvent(input$add_criteria_per_trial"
  )
  
  
  # 
  # The add criteria button is pressed from the criteria by type screen
  #
  
  observeEvent(input$add_criteria_by_type, {
    print("add criteria by type button")
    sessionInfo$criteria_modal_state <- 'AddByType'
    click('add_criteria_per_trial')
  })

  observeEvent(input$edit_criteria_by_type, {
    print("Edit criteria from by type tab")
    sessionInfo$criteria_modal_state <- 'EditByType'
    click('add_criteria_per_trial')
    
    
  }
  )
  
  
  #
  # Row selected in trial criteria table per trial
  #
  observeEvent(input$trial_crit_per_trial_rows_selected, {
    if (!is.null(input$trial_crit_per_trial_rows_selected)) {
      shinyjs::enable('edit_criteria_per_trial')
      shinyjs::enable('delete_criteria_per_trial')
    } else {
      shinyjs::disable('edit_criteria_per_trial')
      shinyjs::disable('delete_criteria_per_trial')
    }
    
  },
  ignoreNULL= FALSE 
  )
  
  
  observeEvent(input$edit_criteria_per_trial, {
    print("edit criteria per trial")
    sessionInfo$criteria_modal_state <- 'EditByTrial'
    click('add_criteria_per_trial')
    
    
  })
  
  #
  # Save an individual criteria 
  #
  
  observeEvent(input$criteria_save, {
    
    print("criteria save button")
    print(paste("state  = ", sessionInfo$criteria_modal_state))
    
    input_error_crit <- FALSE
    if (input$criteria_per_trial_nct_id == '') {
      createAlert(session, 'criteria_modal_alert', 
                  alertId = "criteria_need_nct_id_alert", content = "Please enter a NCT ID", style = 'danger')
      input_error_crit <- TRUE 
    } else {
      shinyBS::closeAlert(session, "criteria_need_nct_id_alert")
    }
    if (input$criteria_per_trial_refined_text == '') {
      createAlert(session, 'criteria_modal_alert', 
                  alertId = "criteria_per_trial_refined_text_alert", content = "Please enter the refined text", style = 'danger')
      input_error_crit <- TRUE 
    } else {
      shinyBS::closeAlert(session, "criteria_per_trial_refined_text_alert")
    }
    if (input$criteria_per_trial_expression == '') {
      createAlert(session, 'criteria_modal_alert', 
                  alertId = "criteria_per_trial_expression_alert", content = "Please enter the expression", style = 'danger')
      input_error_crit <- TRUE 
    } else {
      shinyBS::closeAlert(session, "criteria_per_trial_expression_alert")
    }
    if (input_error_crit == TRUE) {
      print("error on criteria dialog - bailing out ")
      return
    }
   
    
    insert_crit_sql <- "insert into trial_criteria(nct_id, criteria_type_id, trial_criteria_orig_text,trial_criteria_refined_text,
        trial_criteria_expression, update_date, update_by) values(?,?,?,?,?,?,?)"
    update_crit_sql <- "update trial_criteria set trial_criteria_orig_text = ? , trial_criteria_refined_text = ?, 
    trial_criteria_expression = ?, update_date = ?, update_by = ? where nct_id = ? and criteria_type_id = ? " 
    
    type_row_df <- sessionInfo$df_crit_type_titles[sessionInfo$df_crit_type_titles$criteria_type_title == input$criteria_type_typer,]
    
    if(sessionInfo$criteria_modal_state %in% c("AddByType","Neutral") && input_error_crit == FALSE ) {
     # browser()
      # Need to insert a new record from the add by type path
    

      scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      error_happened <- FALSE
      tryCatch( {
        rs <- dbExecute(scon, insert_crit_sql, 
                        params = c(input$criteria_per_trial_nct_id, type_row_df$criteria_type_id, 
                                   input$criteria_per_trial_original_text,input$criteria_per_trial_refined_text,
                                   input$criteria_per_trial_expression, format_iso_8601(Sys.time()),
                                   sessionInfo$result_auth$user))
        #sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
        # save was successful, and close the panel and clear the fields
        toggleModal(session,  "add_criteria_per_trial_bsmodal", toggle = "close")
        sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
      },
      error = function(e) {
        error_happened <- TRUE
        shinyalert("Criteria save error", paste("The following error occurred : ", e ), 
                   type = "error")
      },
      warning = function(w) {
        error_happened <- TRUE 
        shinyalert("Criteria save error", paste("The following error occurred : ", w ), 
                   type = "error")
      },
      finally = {
        DBI::dbDisconnect(scon)
        
      }
      )
      

      
      
    } else if (sessionInfo$criteria_modal_state %in% c("EditByType","EditByTrial")  && input_error_crit == FALSE ) {
      
      scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      tryCatch( {
      rs <- dbExecute(scon, update_crit_sql, 
                      params = c(input$criteria_per_trial_original_text,input$criteria_per_trial_refined_text,
                                 input$criteria_per_trial_expression, format_iso_8601(Sys.time()),
                                 sessionInfo$result_auth$user,input$criteria_per_trial_nct_id, type_row_df$criteria_type_id ))
      print(rs)
      #sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
      # save was successful, and close the panel and clear the fields
      toggleModal(session,  "add_criteria_per_trial_bsmodal", toggle = "close")
      sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
      } , 
      error = function(e) {
        error_happened <- TRUE
        shinyalert("Criteria update error", paste("The following error occurred : ", e ), 
                   type = "error")
      },
      warning = function(w) {
        error_happened <- TRUE 
        shinyalert("Criteria update error", paste("The following error occurred : ", w ), 
                   type = "error")
      },
      finally = {
        DBI::dbDisconnect(scon)
        
      }
      )
      
    }
    
    
  }
  )
  
  
  observeEvent(
    input$delete_criteria_by_type,
    {
      print("delete criteria by type")
      df_sel_crit <- sessionInfo$df_trial_criteria_for_type[input$trial_crit_by_type_rows_selected,]
      print(df_sel_crit)
      del_crit_sql <- "delete from trial_criteria where nct_id = ? and criteria_type_id = ?"
      
      shinyalert("Confirm delete", "Delete the selected criteria?" , 
                 type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                 callbackR = function(x) {
                   if (x == TRUE) {
                       scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                      rs <- dbExecute(scon, del_crit_sql, 
                              params = c(df_sel_crit$nct_id, df_sel_crit$criteria_type_id))
                      DBI::dbDisconnect(scon)
                      sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
                   }
                 }
      )
    }
  )
  
  # observeEvent(
  #   sessionInfo$refresh_criteria_counter,
  #   {
  #     if(sessionInfo$refresh_criteria_counter > 0 ) {
  #       print("refreshing criteria")
  #       crit_type_sel <-
  #         sessionInfo$df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
  #       trial_criteria_for_type_sql <-
  #         "select nct_id, criteria_type_id, trial_criteria_orig_text,
  #                                  trial_criteria_refined_text, trial_criteria_expression, update_date, update_by
  #                               from trial_criteria where criteria_type_id = ? order by nct_id"
  #       sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
  #       sessionInfo$df_trial_criteria_for_type <-
  #         dbGetQuery(sessionCon,
  #                    trial_criteria_for_type_sql,
  #                    params = c(crit_type_sel))
  #       DBI::dbDisconnect(sessionCon)
  #     }
  #   }
  # )
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
  
  # Test the criteria in the environment 
  observeEvent(input$criteria_test_eval, {
    print("testing criteria")
    patient_data_env <- new.env(size = 200L)
    eval(parse(text = "C2926 <- 'YES'"), envir = patient_data_env)
    csv_codes <- "'C2926'"
    print(csv_codes)
    session_conn = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
    
    results <- eval_prior_therapy_app(csv_codes, input$criteria_per_trial_expression , session_conn,
                           eval_env =
                             patient_data_env)
    DBI::dbDisconnect(session_conn)
    createAlert(session, 'criteria_modal_alert', title = "Criteria Test", content = results)
    
  })
  
  # csv input 
  observeEvent(
    input$trial_criteria_csv_file,
    {
      print(paste('csv file upload -- ',input$trial_criteria_csv_file$datapath )) 
      file <- input$trial_criteria_csv_file
      ext <- tools::file_ext(file$datapath)
      print(ext)
      if( ext != 'csv') {
        shinyalert("Upload error", "Please upload a .csv file" , 
                   type = "error")
      } else {
        # We have a .csv file, see if it has the correct info in it.
        new_crits_from_csv_df <-  read.csv(input$trial_criteria_csv_file$datapath, header = TRUE)
        print(new_crits_from_csv_df)
        file_col_names <- names(new_crits_from_csv_df)
        print(file_col_names)
        # See if we have the correct column names 
        mandated_columns <- c('criteria_type_id', 'nct_id', 'trial_criteria_orig_text','trial_criteria_refined_text', 'trial_criteria_expression')
        if (sum (mandated_columns == file_col_names) != 5) {
          shinyalert("Upload error", "The .csv file should contain these columns: criteria_type_id,nct_id,trial_criteria_orig_text,
                     trial_criteria_refined_text,trial_criteria_expression" , 
                     type = "error")
        } else {
          #
          # we have all the correct columns in the file
          # create the dataframe for the data append
          #

          new_crits_df <- new_crits_from_csv_df[, mandated_columns]
          new_crits_df$update_date<- format_iso_8601(Sys.time())
          new_crits_df$update_by <- sessionInfo$result_auth$user
          #
          # Now check that we have a nct_id and criteria_type_id for each row
          #
          if (sum(new_crits_df$nct_id != "") != nrow(new_crits_df)) {
            shinyalert("Upload error", "The nct_id column is blank for at least one row in the csv file.  Please fix this and upload the file again" , 
                       type = "error")
          } else if (sum(!is.na(new_crits_df$criteria_type_id)) != nrow(new_crits_df)) {
            shinyalert("Upload error", "The criteria_type_id column is blank for at least one row in the csv file.  Please fix this and upload the file again" , 
                       type = "error")
          } else {
            # Now actually do the upload
            sessionCon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
            
            # Turn on fk enforcement for this connection.
            
            rs <- dbExecute(sessionCon,'PRAGMA foreign_keys = ON')
            tryCatch( {
            ret <- dbWriteTable(sessionCon, 'trial_criteria', new_crits_df, overwrite = FALSE, append = TRUE)
            shinyalert("Upload successful", "The CSV upload was successful" , 
                       type = "success")
            }, warning = function(w) {
              print("warning")
              shinyalert("Upload warning", paste("The following warning occurred : ", w) , 
                         type = "warning")
              print(w)
            }, error = function(e) {
              print("error")
              print(e)
              shinyalert("Upload error", paste("The following error occurred : ", e) , 
                         type = "error")
            }
            
            )
            DBI::dbDisconnect(sessionCon)
            sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
            
            
          }
        }
      
      }
      
    })
  
  observeEvent(
    input$delete_criteria_per_trial,
    {
      print("delete criteria per trial")
      print(sessionInfo$df_crit_type_titles)
      print(input$trial_crit_per_trial_rows_selected)
      df_sec_crit <- sessionInfo$df_trial_criteria_for_nct_id[input$trial_crit_per_trial_rows_selected,]
      print(df_sec_crit)
      sel_criteria_type_id <- sessionInfo$df_crit_type_titles[sessionInfo$df_crit_type_titles$criteria_type_title == df_sec_crit$criteria_type_title,]$criteria_type_id
      sel_nct_id <- sessionInfo$df_trial_criteria_for_nct_id[input$trial_crit_per_trial_rows_selected,]$nct_id
      #browser()
      del_crit_sql <- "delete from trial_criteria where nct_id = ? and criteria_type_id = ?"
      
      shinyalert("Confirm delete", "Delete the selected criteria?" , 
                 type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                 callbackR = function(x) {
                   if (x == TRUE) {
                     scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
                     rs <- dbExecute(scon, del_crit_sql, 
                                     params = c(sel_nct_id, sel_criteria_type_id))
                     DBI::dbDisconnect(scon)
                     sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
                   }
                 }
      )
           # df_sel_crit <- sessionInfo$df_trial_criteria_for_type[input$trial_crit_per_trial_rows_selected,]
      #print(df_sel_crit)
    }
  )
  
}



shinyApp(ui = ui, server = server)