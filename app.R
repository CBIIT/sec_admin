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


dbinfo <- config::get()

ui <- secure_app(
  fluidPage(
    useShinyjs(),
    useShinyFeedback(),
    
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
      "Add criteria type",
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
          column(1, align = 'right',  offset = 11,actionButton("criteria_type_save", label='Save'))
        )
      )
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
    df_crit_types = NA
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
 
  observe( {
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
}
)
  crit_type_titles_sql <-
    "select criteria_type_id,  criteria_type_title
     from criteria_types order by criteria_type_id"
  df_crit_type_titles <- dbGetQuery(con, crit_type_titles_sql)
  
  updateSelectizeInput(session,
                       'criteria_type_typer',
                       choices = df_crit_type_titles$criteria_type_title ,
                       server = TRUE)
  
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
  
  observeEvent( 
    input$delete_criteria_type,
    {
      print("delete criteria type clicked")
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
                 closeAlert(session, "criteria_type_code_alert")
                 closeAlert(session, "criteria_type_title_alert")
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
        closeAlert(session, "criteria_type_code_alert")
      }
      
      if (input$criteria_type_title == '') {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_title_alert", content = "Please enter a title", style = 'danger')
        input_error <- TRUE 
      } else {
        closeAlert(session, "criteria_type_title_alert")
      }
      
      if (input$criteria_type_desc == '') {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_desc_alert", content = "Please enter a description", style = 'danger')
        input_error <- TRUE 
      } else {
        closeAlert(session, "criteria_type_desc_alert")
      }
      
      if(input_error) {
        return
      }
      
      if (sessionInfo$criteria_type_modal_state == 'Neutral') {
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
        
        
      } else {
        print("criteria type need to do an update")
      }
    }
  )
  
  observeEvent(
    sessionInfo$refresh_criteria_types_counter,
    {
      print("need to refresh criteria types")
      scon = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
      sessionInfo$df_crit_types <- dbGetQuery(scon, crit_type_sql)
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
                           selected=  df_crit_type_titles$criteria_type_title[[input$criteria_types_title_only_rows_selected]])
      sessionInfo$criteria_type_id <- df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
      updateTextInput(session, 'criteria_per_trial_nct_id', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_expression', value = '')
      updateTextAreaInput(session, 'criteria_per_trial_original_text', value = '')
    } else if (sessionInfo$criteria_modal_state == 'EditByType') {
      updateSelectizeInput(session, 'criteria_type_typer', 
                           selected=  df_crit_type_titles$criteria_type_title[[input$criteria_types_title_only_rows_selected]])
      sessionInfo$criteria_type_id <- df_crit_type_titles$criteria_type_id[[input$criteria_types_title_only_rows_selected]]
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
          df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
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
        df_criteria_nct_id$nct_id[[input$criteria_nct_ids_rows_selected]]
      print(paste("nct_id selected:", nct_id_sel))
      df_sel_crit <- sessionInfo$df_trial_criteria_for_nct_id[input$trial_crit_per_trial_rows_selected,]
      updateSelectizeInput(session, 'criteria_type_typer', 
                           selected=  df_sel_crit$criteria_type_title)
      updateTextInput(session, 'criteria_per_trial_nct_id', value = nct_id_sel)
      updateTextAreaInput(session, 'criteria_per_trial_refined_text', value = df_sel_crit$trial_criteria_refined_text)
      updateTextAreaInput(session, 'criteria_per_trial_expression', value = df_sel_crit$trial_criteria_expression)
      updateTextAreaInput(session, 'criteria_per_trial_original_text', value = df_sel_crit$trial_criteria_orig_text)
    }
  }
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
  
  observeEvent(input$criteria_save, {
    print("criteria save button")
    print(paste("state  = ", sessionInfo$criteria_modal_state))
  }
  )
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
    createAlert(session, 'criteria_modal_alert', title = "Criteria Test", content = "Criteria Expression Valid")
    
  })
}


shinyApp(ui = ui, server = server)