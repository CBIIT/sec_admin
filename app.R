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
library(writexl)

library(pool)
library(RPostgres)

source('eval_prior_therapy_app.R')
source('check_if_any.R')
dbinfo <- config::get()

local_dbname <- dbinfo$dbname
local_host <- dbinfo$host
local_user <- dbinfo$user
local_password <- dbinfo$password
local_port <- dbinfo$port
pool_idleTimeout <- 300 # 5 minute pool timeout default
pool_minSize <- 0
pool_maxSize <- 3
pool_validationInterval <- 60000000000 

pool_con <- dbPool(#drv = RPostgreSQL::PostgreSQL(), 
  drv = RPostgres::Postgres(),
  dbname = local_dbname,
  host = local_host, 
  user = local_user,
  password =local_password,
  port = local_port, 
  timezone = Sys.timezone(), 
  timezone_out = Sys.timezone(),
  idleTimeout = pool_idleTimeout,
  minSize = pool_minSize,
  maxSize = pool_maxSize,
  validationInterval = pool_validationInterval
) 

print(pool_con)

#
# generate_safe_query ----
# This is a function that takes the database connection pool as an argument.  It returns 
# a function that takes a given R dbapi function (like dbGetQuery) and arguments and tries to run that 
# statement given the passed in function. If the statement fails, it sleeps for the 
# designated time, recreates the pool, and tries again.  If the error condition is 
# transient in nature, (for example, network connectivity is lost, or the AWS database goes dormant and needs to be spun back up) 
# this will successful recover from that.
# 
# Other errors won't recover but will be noted before bailing out.
#
#
wait_times <- c(2,2)

generate_safe_query <- function(pool) {
  function(db_function, ...) {
    # print("in safe query")
    tryCatch({
      #  xx<-lapply(sys.call()[-1], deparse)
      #  print(paste0(ifelse(nchar(names(xx))>0, paste0(names(xx),"="), ""), unlist(xx), collapse=", "))
      db_function(pool, ...)
    }, error = function(e) {
      print("ERROR IN safe_query ")
      print(e$message)
      #browser()
      print("error - going to try to recreate the pool")
      tryCatch( {
        Sys.sleep(wait_times[1])  # Sleep two seconds 
        # poolClose(pool_con)
        pool_con <<- dbPool(drv = RPostgres::Postgres(),
                            dbname = local_dbname,
                            host = local_host, 
                            user = local_user,
                            password = local_password,
                            idleTimeout = pool_idleTimeout,
                            minSize = pool_minSize,
                            maxSize = pool_maxSize,
                            validationInterval = pool_validationInterval
        ) 
        db_function(pool, ...)
      } , error = function(e) {
        # Unexpected error
        print(paste("cannot recreate pool - going to sleep and try one more time  ", e$message))
        tryCatch( {
          Sys.sleep(wait_times[2])  # Sleep two seconds 
          # poolClose(pool_con)
          pool_con <<- dbPool(drv = RPostgres::Postgres(),
                              dbname = local_dbname,
                              host = local_host, 
                              user = local_user,
                              password = local_password,
                              idleTimeout = pool_idleTimeout,
                              minSize = pool_minSize,
                              maxSize = pool_maxSize,
                              validationInterval = pool_validationInterval
          ) 
          db_function(pool, ...)
        } , error = function(e) {
          # Unexpected error
          print(paste("cannot recreate pool - bailing out ", e$message))
          stop(e)
        })
      })
      
    })
  }
}
############

safe_query <<- generate_safe_query(pool_con)


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

    titlePanel(title = h4('SEC POC Administration'), 
               windowTitle = "SEC POC Administration"),
    
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
            4, radioButtons("criteria_type_active_rb","Active", choices = c("Y" = "Y", "N" = "N"), inline = TRUE)
          )
          ,
          column(
            4, radioButtons("criteria_type_sense_rb","Active", choices = c("Inclusion" = "Inclusion", "Exclusion" = "Exclusion"), inline = TRUE)
          ),
          column(
            4, numericInput('criteria_column_index','Col index', NULL)
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
         column(1, align='right', actionButton("criteria_test_eval", label = "Test Expression")),
         column(2, align = 'right',  offset = 9,actionButton("criteria_save", label='Save and Close'))
       )
      ),
      
      tags$head(tags$style("#add_criteria_per_trial_bsmodal .modal-footer{ display:none}"))
    ),
    # work queue bsmodal ----
    bsModal(
      "gen_expression_work_queue_bsmodal",
      "Generated Expression Item",
      "do_work_queue_item",
      size = "large",
      fluidPage(
        
        fluidRow(
          column( 
            6, textInput("genexp_nctid", "NCT ID")),
          column(
            6, selectizeInput(
              "genexp_type_typer",
              label = "Criteria Type",
              NULL,
              multiple = FALSE
            )
          )
        ),
        fluidRow(
          textAreaInput('genexp_crit_text',"Description", width = '100%', height = '200px')  %>%
            shiny::tagAppendAttributes(style = 'width: 100%;', align = 'center')
        ),
        fluidRow(
          textAreaInput('genexp_norm_form',"Normal Form", width = '100%')  %>%
            shiny::tagAppendAttributes(style = 'width: 100%;', align = 'center')
        ),
        fluidRow(
        textAreaInput('genexp_gen_expression',"Generated Expression", width = '100%')  %>%
          shiny::tagAppendAttributes(style = 'width: 100%;', align = 'center')
        ),
        fluidRow(column(12, align = 'center',
          actionButton('use_gen_expression','', icon = icon('arrow-down'))
        )
        ),
        fluidRow(
          textAreaInput('genexp_current_expression',"Current Expression", width = '100%')  %>%
            shiny::tagAppendAttributes(style = 'width: 100%;', align = 'center')
          
        ),
        fluidRow(
          column(7, align='right', actionButton("genexp_mark_done", label = "Mark Done")),
          column(2, align = 'right',  offset = 1,actionButton("genexp_save", label='Update Expression and Mark Done'))
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
                      accept = ".csv"
            )
            
          ),
          mainPanel(DTOutput("trial_crit_by_type"),
                    downloadButton("downloadTrialDataByType", "Download Trial Criteria", style =
                                     'padding:4px; font-size:80%'))
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
                 mainPanel(DTOutput("trial_crit_per_trial")
                          )
               ))
      ,
      tabPanel("Trials with Candidate Criteria",
              sidebarLayout(
                sidebarPanel(
                  DTOutput('crit_with_cands_count'),
                  br(),
                  actionButton("add_cand_criteria_per_trial", "Add", width = '100px'),
                  br(),
                  actionButton("edit_cand_criteria_per_trial", "Edit", width = '100px'),
                  br(),
                  actionButton("delete_cand_criteria_per_trial", "Delete", width = '100px')
                ),
                mainPanel(
                  DTOutput("crit_with_cands_for_type"),
   
                      downloadButton("downloadCandData", "Download Cand Crit Data", style =
                                       'padding:4px; font-size:80%')
                    )
                    
                    
                  
                  
                
              
               )
      )
      ,
      tabPanel("Generated Expression Work Queue",
               fluidRow(
                 column( 
                   2, checkboxInput("exclude_industrial_trials_checkbox", "Exclude Industrial Trials", value = TRUE))
                 ,
                 column(
                   10, radioGroupButtons("gen_exp_crit_type_rb", label = "Criteria Type", choiceNames = NA, choiceValues = NA)
                 )
                 
               ), 
               DTOutput("crit_work_queue"),
               actionButton("do_work_queue_item", "Process NLP Work Queue Item")
                        
               )
      ,
      tabPanel("NLP Tokenizer Concepts",
             
              
                  textInput("tokenizer_nct_id", "NCT ID"),
                
                 actionButton("get_tokenizer_input_for_trial", "Show Tokenizer Input For Trial"),
                 br(),
               DTOutput("tokenizer_output_for_trial", width = "100%"),
               downloadButton("downloadTokenizerData", "Download Tokenizer Data", style =
                                'padding:4px; font-size:80%')
               )
      ,
      tabPanel("NCIt Path Explorer",
               fluidPage(
                 
                 fluidRow(
                   column( 
                     4, textInput("path_start_ncit_code", "Start NCIt Code")),
                   column(
                     4,  textInput("path_end_ncit_code","End NCIt Code" )),
                   column(4, actionButton("generate_paths", "Show Paths"),
                          tags$style(type='text/css', "#generate_paths { width:100%; margin-top: 25px;}")
                          
                   )
                 )
               )   
                 , 
 
                   DTOutput("path_explorer_output", width = "100%"),
 
               downloadButton("downloadNCITData", "Download NCIT Path Data", style =
                                'padding:4px; font-size:80%')
      
                 
               ),
      tabPanel(
        "Synthea Test Data",
        sidebarLayout(
          sidebarPanel(
            DTOutput("synthea_codes_name"),
          ),
          mainPanel(DTOutput("synthea_data_by_code"),
                    downloadButton("downloadSyntheaTestDataByCode", "Download Synthea Test Data", style =
                                     'padding:4px; font-size:80%'))
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
    df_crit_types_with_all = NA, 
    df_crit_type_titles = NA,
    df_criteria_nct_id = NA,
    df_crit_with_cands_count = NA,
    df_crit_with_cands_for_type = NA,
    df_crit_work_queue = NA,
    refresh_work_queue_counter = 0,
    work_queue_row_df = NA,
    tokenizerData = NA,
    ncit_path_data = NA,
    crit_work_queue_dt = NA,
    df_synthea_codes_name = NA
  )
  
  sessionInfo$result_auth <-
    secure_server(
      check_credentials = check_credentials(dbinfo$db_user_file, passphrase = dbinfo$passphrase),
      timeout = 30
    )
  #  sessionInfo$result_auth <- result_auth
  
  
  shinyjs::disable("edit_criteria_type")
  shinyjs::disable('edit_criteria_by_type')
  shinyjs::disable('delete_criteria_by_type')
  shinyjs::disable('add_criteria_by_type')
  shinyjs::disable('edit_criteria_per_trial')
  shinyjs::disable('delete_criteria_per_trial')
  
  shinyjs::disable('genexp_nctid')
  shinyjs::disable('genexp_crit_type')
  shinyjs::disable('genexp_crit_text')
  shinyjs::disable('genexp_norm_form')
  shinyjs::disable('genexp_gen_expression')
  

 # con = DBI::dbConnect(RSQLite::SQLite(), dbinfo$db_file_location)
  
  #
  # Select the criteria types
  #
  
  crit_type_sql <-
    "select criteria_type_id, criteria_type_code, criteria_type_title, criteria_type_desc, criteria_type_active, 
    criteria_type_sense, criteria_column_index 
     from criteria_types order by criteria_column_index"
  
  crit_type_sql_with_all <-
    "with rb_vals as 
     (select -666 as criteria_type_id, 'ALL' as criteria_type_title , -666 as criteria_column_index
     union 
     select criteria_type_id, criteria_type_title , criteria_column_index 
     from criteria_types)
     select criteria_type_id, criteria_type_title from rb_vals order by criteria_column_index"
  
  criteria_nct_ids_sql <-
    "select distinct nct_id from trial_criteria order by nct_id"
  
  crit_with_cands_count_sql <- 
    "with cand_crit_count as (
select  criteria_type_id , count(criteria_type_id) as crit_count from candidate_criteria
group by criteria_type_id
)
select ct.criteria_type_id, ct.criteria_type_title, ccc.crit_count
from criteria_types ct join cand_crit_count ccc on ct.criteria_type_id = ccc.criteria_type_id"
 
work_queue_all_sql <- "
 SELECT cc.nct_id, ct.criteria_type_id, cc.display_order, ct.criteria_type_title,
case
  when cc.inclusion_indicator = true then 'Inclusion: ' || cc.candidate_criteria_text
  when cc.inclusion_indicator = false then 'Exclusion: ' || cc.candidate_criteria_text
end cand_crit_text,
cc.candidate_criteria_norm_form, cc.candidate_criteria_expression , tc.trial_criteria_expression

from candidate_criteria cc
join criteria_types ct on cc.criteria_type_id = ct.criteria_type_id
left outer join trial_criteria tc on cc.nct_id = tc.nct_id and cc.criteria_type_id = tc.criteria_type_id
where (tc.trial_criteria_expression is null or tc.trial_criteria_expression = '' or 
replace(tc.trial_criteria_expression,' ' ,'')  <> replace(cc.candidate_criteria_expression,' ' ,'' ) 
) 
and cc.candidate_criteria_expression <> 'NO MATCH'
and cc.criteria_type_id = $1 
and (cc.generated_date > cc.marked_done_date or cc.marked_done_date is null) 
order by cc.nct_id 
"

work_queue_all_types_sql <- "
SELECT cc.nct_id, ct.criteria_type_id, cc.display_order, ct.criteria_type_title,
case
when cc.inclusion_indicator = true then 'Inclusion: ' || cc.candidate_criteria_text
when cc.inclusion_indicator = false then 'Exclusion: ' || cc.candidate_criteria_text
end cand_crit_text,
cc.candidate_criteria_norm_form, cc.candidate_criteria_expression , tc.trial_criteria_expression,
cc.generated_date , tnd.classification_date, tnd.tokenized_date
from candidate_criteria cc
join criteria_types ct on cc.criteria_type_id = ct.criteria_type_id
join trial_nlp_dates tnd on cc.nct_id = tnd.nct_id 
left outer join trial_criteria tc on cc.nct_id = tc.nct_id and cc.criteria_type_id = tc.criteria_type_id
where (tc.trial_criteria_expression is null or tc.trial_criteria_expression = '' or 
replace(tc.trial_criteria_expression,' ' ,'')  <> replace(cc.candidate_criteria_expression,' ' ,'' ) 
) 
and cc.candidate_criteria_expression <> 'NO MATCH'
and (cc.generated_date > cc.marked_done_date or cc.marked_done_date is null) 
order by cc.nct_id 
"


work_queue_no_industrial_sql <- "
SELECT cc.nct_id, ct.criteria_type_id, cc.display_order, ct.criteria_type_title,
case
when cc.inclusion_indicator = true then 'Inclusion: ' || cc.candidate_criteria_text
when cc.inclusion_indicator = false then 'Exclusion: ' || cc.candidate_criteria_text
end cand_crit_text,
cc.candidate_criteria_norm_form, cc.candidate_criteria_expression , tc.trial_criteria_expression,
cc.generated_date , tnd.classification_date, tnd.tokenized_date
from candidate_criteria cc
join criteria_types ct on cc.criteria_type_id = ct.criteria_type_id
join trial_nlp_dates tnd on cc.nct_id = tnd.nct_id 
join trials t on cc.nct_id = t.nct_id 
left outer join trial_criteria tc on cc.nct_id = tc.nct_id and cc.criteria_type_id = tc.criteria_type_id
where (tc.trial_criteria_expression is null or tc.trial_criteria_expression = '' or 
replace(tc.trial_criteria_expression,' ' ,'')  <> replace(cc.candidate_criteria_expression,' ' ,'' ) 
) 
and cc.candidate_criteria_expression <> 'NO MATCH'  and t.study_source <> 'Industrial'
and cc.criteria_type_id = $1
and (cc.generated_date > cc.marked_done_date or cc.marked_done_date is null) 
order by cc.nct_id
"


work_queue_no_industrial_all_types_sql <- "
SELECT cc.nct_id, ct.criteria_type_id, cc.display_order, ct.criteria_type_title,
case
when cc.inclusion_indicator = true then 'Inclusion: ' || cc.candidate_criteria_text
when cc.inclusion_indicator = false then 'Exclusion: ' || cc.candidate_criteria_text
end cand_crit_text,
cc.candidate_criteria_norm_form, cc.candidate_criteria_expression , tc.trial_criteria_expression,
cc.generated_date , tnd.classification_date, tnd.tokenized_date
from candidate_criteria cc
join criteria_types ct on cc.criteria_type_id = ct.criteria_type_id
join trial_nlp_dates tnd on cc.nct_id = tnd.nct_id 
join trials t on cc.nct_id = t.nct_id 
left outer join trial_criteria tc on cc.nct_id = tc.nct_id and cc.criteria_type_id = tc.criteria_type_id
where (tc.trial_criteria_expression is null or tc.trial_criteria_expression = '' or 
replace(tc.trial_criteria_expression,' ' ,'')  <> replace(cc.candidate_criteria_expression,' ' ,'' ) 
) 
and cc.candidate_criteria_expression <> 'NO MATCH'  and t.study_source <> 'Industrial'
and (cc.generated_date > cc.marked_done_date or cc.marked_done_date is null) 
order by cc.nct_id
"
#

observeEvent( input$exclude_industrial_trials_checkbox, {
  print("exclude_industrial_trials_checkbox")
  sessionInfo$refresh_work_queue_counter <- sessionInfo$refresh_work_queue_counter + 1
}, ignoreNULL = TRUE
)

#
# Populate the work queue datatable ----
#


observeEvent(sessionInfo$refresh_work_queue_counter, {
  sessionInfo$df_crit_work_queue
  
#  print(paste ("industrials trials : ", input$exclude_industrial_trials_checkbox))
  #browser()
  if( ! is.null(input$gen_exp_crit_type_rb) && nchar(input$gen_exp_crit_type_rb) > 0)  {
    
    
  if (is.null(input$exclude_industrial_trials_checkbox) ||input$exclude_industrial_trials_checkbox == FALSE ) {
    if(input$gen_exp_crit_type_rb > -666) {
      this_sql <- work_queue_all_sql
      rs <- safe_query(dbGetQuery, this_sql, params = c( input$gen_exp_crit_type_rb))
    } else {
      this_sql <- work_queue_all_types_sql
      rs <- safe_query(dbGetQuery, this_sql)
    }  
  }
  else {
    if(input$gen_exp_crit_type_rb > -666) {
      this_sql <- work_queue_no_industrial_sql
      rs <- safe_query(dbGetQuery, this_sql, params = c( input$gen_exp_crit_type_rb))
    } else {
      this_sql <- work_queue_no_industrial_all_types_sql
      rs <- safe_query(dbGetQuery, this_sql)
      
    }  
  }
  
  rs$criteria_type_title <- as.factor(rs$criteria_type_title) 
  sessionInfo$df_crit_work_queue <- rs
  
 
  crit_work_queue_dt <- datatable(
    sessionInfo$df_crit_work_queue,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    filter = 'top',
    escape = FALSE,
   # fillContainer = TRUE,
    colnames = c('NCT ID',
                 'Criteria Type ID',
                 'Display Order',
                 'Criteria Type',
                  'Criteria Text',
                 'Normal Form',
                 'Generated Expression (new)',
                 'Current Expression',
                 'Exp Generated Date',
                 'Classification Date',
                 'Tokenized Date'
                 ),
    options = list(
      info = TRUE,
      searching = TRUE,
      autoWidth = TRUE,
      scrollX = TRUE,
   #   deferRender = TRUE,
      lengthMenu = c(50,150, 250),
      scrollY = "65vh",
   #   scrollCollapse = TRUE,
      paging = TRUE,
      style = "overflow-y: scroll",
      columnDefs = list(# Initially hidden columns
        list(
          visible = FALSE,
          
          targets = c(2,3)
        ),
        list(
          width='150px',
          targets = c(4)
        )
        ),
     list(
     width='300px',
       targets = c(5)
     )
    ),
    callback = htmlwidgets::JS(
      "table.on('dblclick', 'td',", 
      "  function() {",
      "    var row = table.cell(this).index().row;",
      "    var col = table.cell(this).index().column;",
      "    Shiny.setInputValue('work_queue_dblclick', {dt_row: row, dt_col: col});",
      "  }",
      ");"
    )
  )
  
  
  output$crit_work_queue <-
    DT::renderDataTable({
   crit_work_queue_dt   
     
      })
  
  }
})

# 
# Get paths for concept codes ----
#
observeEvent(input$generate_paths, {
  print(paste('generate paths',input$path_start_ncit_code , ' to ', input$path_end_ncit_code))


  get_path_info_sql <- 
    "
  with all_things as (
 select tcp.parent as parent_code, n1.pref_name as parent, 
            tcp.descendant as descendant_code, n2.pref_name as descendant, tcp.level, tcp.path 
from ncit_tc_with_path tcp  
join ncit n1 on tcp.parent = n1.code 
join ncit n2 on tcp.descendant = n2.code
where tcp.parent = $1 and tcp.descendant = $2
)
select al.parent_code, al.parent, al.descendant_code, al.descendant, al.level, al.path 
from all_things al
"
  rs <- safe_query(dbGetQuery, get_path_info_sql, 
                   params = c(trimws(input$path_start_ncit_code),  trimws(input$path_end_ncit_code)) ) 

  rs$enumerated_path <-
    lapply(rs$path,
           function(x)
             {
             qs <- paste("WITH  recursive split(counter, word, str) AS (
    SELECT 0,  '', '",  x ,"'||'|'
    UNION ALL SELECT
	counter + 1, 
    substr(str, 0, strpos(str, '|')),
    substr(str, strpos(str, '|')+1)
    FROM split WHERE str!=''
) 
, 
path_tab as (
SELECT s.counter, s.word as ncit_code , n.pref_name , s.word || '-' || n.pref_name as full_name 
FROM split s join ncit n on s.word = n.code where s.counter > 0 )
select * from path_tab order by counter
             
             " , sep = "" )
            # print(qs)
            # browser()
             
             rs_i <- safe_query(dbGetQuery,qs)
             paste0(rs_i$full_name, collapse = ' --> ')
             
           }
           )
  temp_rs <-  as.data.frame(lapply(rs, unlist))
  rs <- temp_rs
  sessionInfo$ncit_path_data <- rs
  #browser()
  
  path_results_dt <- datatable(
    rs,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    escape = FALSE,
    #filter = 'top',
    colnames = c(
                  'Parent Code',
                 'Parent',
                 'Descendant Code',
                 'Descendant',
                 'Distance',
                 'Path',
                 'Enumerated Path'

    ),
    options = list(
      info = TRUE,
      rownames= FALSE,
      
      searching = TRUE,
      lengthMenu = c(50,150, 250),
      #autoWidth = TRUE,
      # scrollX = TRUE,
      #  deferRender = TRUE,
      #  scrollY = "45vh",
      #   scrollCollapse = TRUE,
      paging = TRUE
      #   style = "overflow-y: scroll"
      
    )
  )
  
  output$path_explorer_output <-
    DT::renderDataTable({
      path_results_dt   
      
    })
  }
)
#
# Get tokenizer input ----
#

observeEvent(input$get_tokenizer_input_for_trial, {
  get_tokenizer_results_sql <-
    "
  select ncit_code, display_order, pref_name, span_text, start_index, end_index, inclusion_indicator, description
    from nlp_data_view where nct_id = $1
  
  "
  
  print("get tokenizer input button click") 
  rs <- safe_query(dbGetQuery, get_tokenizer_results_sql, 
                  params = c(input$tokenizer_nct_id))  
  
  rs$display_order <- as.factor(rs$display_order)
  rs$inclusion_indicator <- as.factor(rs$inclusion_indicator)
  
  sessionInfo$tokenizerData <- rs
 # browser()
  tokenizer_results_dt <- datatable(
    rs,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    escape = FALSE,
    filter = 'top',
    colnames = c('NCIt Code',
                 'Display Order',
                 'Preferred Name',
                 'Span Text',
                 'Start Index',
                 'End Index',
                 'Inclusion Indicator',
                 'Description'
    ),
    options = list(
      info = TRUE,
      
      
      searching = TRUE,
      #autoWidth = TRUE,
     # scrollX = TRUE,
    #  deferRender = TRUE,
    #  scrollY = "45vh",
   #   scrollCollapse = TRUE,
      paging = TRUE,
   #   style = "overflow-y: scroll",
       columnDefs = list(
       
         list(
           targets = match(
             c('description'),
             names(rs)
           ),
           render = JS(
             "function(data, type, row, meta) { if (data === null) { return \"\" } ",
             "return type === 'display' && data.length > 30 ?",
             "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
             "}"
           )
         )
         
         )

  
   
    )
  )
  
  output$tokenizer_output_for_trial <-
    DT::renderDataTable({
      tokenizer_results_dt   
      
    })
}
)



#
# Process the work queue button click or table doubleclick ----
#

observeEvent(input$work_queue_dblclick, {
  print(input$work_queue_dblclick)
  print(paste("row_last_clicked from double click" , input$crit_work_queue_row_last_clicked))
  
  shinyjs::click("do_work_queue_item")
  #browser()
})


observeEvent(input$do_work_queue_item, {
  print("work queue button clicked ")
  print(paste("row_last_clicked from button click" , input$crit_work_queue_row_last_clicked))
  print(paste("rows_selected from button click" , input$crit_work_queue_rows_selected))
  row_to_process <- -1
  if (!is.null(input$crit_work_queue_rows_selected)) {
    row_to_process <- input$crit_work_queue_rows_selected[1]
  } else if  (!is.null(input$crit_work_queue_row_last_clicked)) {
    row_to_process <- input$crit_work_queue_row_last_clicked
  }
  
  if (row_to_process > -1) {
    # work to do 
    sessionInfo$work_queue_row_df <- sessionInfo$df_crit_work_queue[row_to_process,]
   # print(rowdf)
    updateTextInput(session, 'genexp_nctid', value = sessionInfo$work_queue_row_df$nct_id)
   # updateTextInput(session, 'genexp_crit_type', value = sessionInfo$work_queue_row_df$criteria_type_title)
    updateSelectizeInput(session, 'genexp_type_typer', 
                         selected=  sessionInfo$work_queue_row_df$criteria_type_title)
    updateTextInput(session, 'genexp_crit_text', value = sessionInfo$work_queue_row_df$cand_crit_text)
    updateTextInput(session, 'genexp_norm_form', value = sessionInfo$work_queue_row_df$candidate_criteria_norm_form)
    updateTextInput(session, 'genexp_gen_expression', value = sessionInfo$work_queue_row_df$candidate_criteria_expression)
    updateTextInput(session, 'genexp_current_expression', value = sessionInfo$work_queue_row_df$trial_criteria_expression)
    
  }
}
  
)

observeEvent(input$use_gen_expression, {
  print("use generated expression")
  updateTextInput(session, 'genexp_current_expression', value = sessionInfo$work_queue_row_df$candidate_criteria_expression)
  
})

observeEvent(input$genexp_mark_done, {
 print("work queue mark done") 
  update_mark_done_date_sql <- 'update candidate_criteria set marked_done_date = now() 
     where nct_id = $1 and criteria_type_id = $2 and display_order = $3'
  rs <- safe_query(dbExecute, update_mark_done_date_sql, 
                  params = c(#format_iso_8601(Sys.time()),
                             sessionInfo$work_queue_row_df$nct_id, 
                             sessionInfo$work_queue_row_df$criteria_type_id,
                             sessionInfo$work_queue_row_df$display_order))
  sessionInfo$refresh_work_queue_counter <- sessionInfo$refresh_work_queue_counter + 1
}
)

observeEvent(input$genexp_save, {
  print("work queue save the expression and mark done ")
  update_mark_done_date_sql <- 'update candidate_criteria set marked_done_date = now() 
     where nct_id = $1 and criteria_type_id = $2 and display_order = $3'
  
  update_crit_expression_sql <- "update trial_criteria set trial_criteria_orig_text = $1 , trial_criteria_refined_text = $2, 
    trial_criteria_expression = $3, update_date = now(), update_by = $4 where nct_id = $5 and criteria_type_id = $6 "
  
  is_there_a_crit_sql <- "select count(*) as num_recs from trial_criteria where nct_id = $1 and criteria_type_id = $2"
  
  insert_a_crit_sql <- 'insert into trial_criteria(nct_id, criteria_type_id, trial_criteria_orig_text,
                       trial_criteria_refined_text, trial_criteria_expression, update_date, update_by) 
                       values($1,$2,$3,$4,$5,now(),$6)'
  wq_type_row_df <- sessionInfo$df_crit_type_titles[sessionInfo$df_crit_type_titles$criteria_type_title == input$genexp_type_typer,]
  

  # Also update the criteria used for matching
  num_crits <- safe_query(dbGetQuery, is_there_a_crit_sql,
                  params = c(sessionInfo$work_queue_row_df$nct_id,
                             wq_type_row_df$criteria_type_id) )
  print(paste("there are ", num_crits$num_recs , " records "))

  #browser()
  
  #Note need to mark the candidate criteria itself with the existiing critieria type
  # as marked done to make it stop showing back up (in case the criteria type was changed) 
  rs <- safe_query(dbExecute, update_mark_done_date_sql, 
                  params = c(#Sys.time(),
                             sessionInfo$work_queue_row_df$nct_id, 
                             sessionInfo$work_queue_row_df$criteria_type_id,
                             sessionInfo$work_queue_row_df$display_order))
  if (num_crits$num_recs == 0 ) {
    print('need to insert')
    rs <- safe_query(dbExecute, insert_a_crit_sql,
                    params = c(sessionInfo$work_queue_row_df$nct_id,
                               wq_type_row_df$criteria_type_id,
                               sessionInfo$work_queue_row_df$cand_crit_text,
                               sessionInfo$work_queue_row_df$candidate_criteria_norm_form,
                               input$genexp_current_expression,
                              # Sys.time(),
                               sessionInfo$result_auth$user)
                    )
  } else {
    print('need to update ')
    rs <- safe_query(dbExecute, update_crit_expression_sql, 
                  params = c(sessionInfo$work_queue_row_df$cand_crit_text,
                             sessionInfo$work_queue_row_df$candidate_criteria_norm_form,
                             input$genexp_current_expression, 
                            # Sys.time(),
                             sessionInfo$result_auth$user,
                             sessionInfo$work_queue_row_df$nct_id,
                             wq_type_row_df$criteria_type_id ))
  } 
  sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
  sessionInfo$refresh_work_queue_counter <- sessionInfo$refresh_work_queue_counter + 1
  
}
)

#
# The criteria types radio button in the work queue window has been changed ----
#

observeEvent(input$gen_exp_crit_type_rb, {
  print("crit types in radio button group changed ")
  print(paste("selected radio button is ", input$gen_exp_crit_type_rb))
  sessionInfo$refresh_work_queue_counter <- sessionInfo$refresh_work_queue_counter + 1
  
}, ignoreInit = TRUE)


observe({
  sessionInfo$df_crit_with_cands_count <- safe_query(dbGetQuery, crit_with_cands_count_sql)
  crit_with_cands_count_dt <- datatable(
    sessionInfo$df_crit_with_cands_count,
    class = 'cell-border stripe compact wrap hover',
    selection = 'single',
    colnames = c('Type ID',
                 'Title',
                 'Num Cand Crit'),
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
  
  output$crit_with_cands_count <-
    DT::renderDataTable({
      crit_with_cands_count_dt
    })
})


observeEvent(input$crit_with_cands_count_rows_selected, {
  print("crit with cands type selected")
  
  if (!is.null(input$crit_with_cands_count_rows_selected)) {
    select_cands_sql <- "
  SELECT cc.nct_id, cc.criteria_type_id,
case
  when cc.inclusion_indicator = true then 'Inclusion: ' || cc.candidate_criteria_text
  when cc.inclusion_indicator = false then 'Exclusion: ' || cc.candidate_criteria_text
end cand_crit_text,
cc.candidate_criteria_norm_form, cc.candidate_criteria_expression

from candidate_criteria cc
join criteria_types ct on cc.criteria_type_id = ct.criteria_type_id
left outer join trial_criteria tc on cc.nct_id = tc.nct_id and cc.criteria_type_id = tc.criteria_type_id
where (tc.trial_criteria_expression is null or tc.trial_criteria_expression = '' ) and cc.criteria_type_id = $1
order by cc.nct_id, cc.criteria_type_id "
    print(sessionInfo$df_crit_with_cands_count[input$crit_with_cands_count_rows_selected,]$criteria_type_id)
    
    raw_cands <- safe_query(dbGetQuery, select_cands_sql,
                params = c(sessionInfo$df_crit_with_cands_count[input$crit_with_cands_count_rows_selected,]$criteria_type_id) )

    #sessionInfo$df_crit_with_cands_for_type <- aggregate(data=sessionInfo$df_crit_with_cands_for_type, cand_crit_text~nct_id+criteria_type_id, paste, collapse = '\n' )
   # sessionInfo$df_crit_with_cands_for_type <- aggregate(data=raw_cands, cand_crit_text~nct_id+criteria_type_id, paste, collapse = '\n' )
    sessionInfo$df_crit_with_cands_for_type <- raw_cands
    # browser()
    crit_with_cands_for_type_dt <-
      datatable(
        sessionInfo$df_crit_with_cands_for_type,
        class = 'cell-border stripe compact wrap hover',
        selection = 'single',
        width  = "90vw",
        colnames = c(
          'NCT ID',
          'Type ID',
          'Candidate Original Text',
          'Normal Form',
          "Generated Expression"
          
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
    output$crit_with_cands_for_type <-
      DT::renderDataTable({
        crit_with_cands_for_type_dt
      })
    
  }
})
             
  ## 
  ## Observe for refetching the new criteria types when they have changed.
  ##
  
  observe({
    sessionInfo$df_crit_types <- safe_query(dbGetQuery, crit_type_sql)
    sessionInfo$df_crit_types_with_all <- safe_query(dbGetQuery, crit_type_sql_with_all)
    criteria_types_dt <- datatable(
      sessionInfo$df_crit_types,
      class = 'cell-border stripe compact wrap hover',
      selection = 'single',
      colnames = c('Type ID',
                   'Abbr',
                   'Title',
                   'Description',
                   'Active',
                   'Inc/Exc',
                   'Column')
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

    sessionInfo$df_crit_type_titles <- safe_query(dbGetQuery, crit_type_titles_sql)

    updateSelectizeInput(
      session,
      'criteria_type_typer',
      choices = sessionInfo$df_crit_type_titles$criteria_type_title ,
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      'genexp_type_typer',
      choices = sessionInfo$df_crit_type_titles$criteria_type_title ,
      server = TRUE
    )
    
   # browser()
    updateRadioGroupButtons(
      session = session, inputId = "gen_exp_crit_type_rb",
      choiceNames =  sessionInfo$df_crit_types_with_all$criteria_type_title,
      choiceValues = sessionInfo$df_crit_types_with_all$criteria_type_id
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

   
    sessionInfo$df_criteria_nct_id <- safe_query(dbGetQuery, criteria_nct_ids_sql)

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
        updateNumericInput(session, 'criteria_column_index', value=rowdf$criteria_column_index)
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
        updateNumericInput(session, 'criteria_column_index', value = NA)
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
        num_crits_sql <- "select count(*) as num_recs from trial_criteria where criteria_type_id = $1"
        rowdf <- sessionInfo$df_crit_types[input$criteria_types_table_rows_selected,]
        sessionInfo$criteria_type_modal_type_id <- rowdf$criteria_type_id  
        print(paste("delete - checking num recs for ",sessionInfo$criteria_type_modal_type_id))

        num_crits <- safe_query(dbGetQuery, num_crits_sql, 
                        params = c( sessionInfo$criteria_type_modal_type_id)) 

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
                        rs <- safe_query(dbExecute, "delete from criteria_types where criteria_type_id = $1 ", 
                                        params = c(sessionInfo$criteria_type_modal_type_id) )
                        sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1

                         
                       } else {
                         shinyalert("Are you really sure?", 
                                    paste("Are you really sure you want to delete the criteria type of ",rowdf$criteria_type_title, " with ",num_crits$num_recs, " criteria records? "),
                         type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                         callbackR = function(x) {
                           if (x == TRUE) {
                             print("second confirm of delete")
                             rs <- safe_query(dbExecute, "delete from trial_criteria where criteria_type_id = $1 ", 
                                             params = c(sessionInfo$criteria_type_modal_type_id) )
                             rs <- safe_query(dbExecute, "delete from criteria_types where criteria_type_id = $1 ", 
                                             params = c(sessionInfo$criteria_type_modal_type_id) )
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
      #browser()
      if (is.na(input$criteria_column_index)) {
        createAlert(session, 'criteria_type_modal_alert', 
                    alertId = "criteria_type_col_index_alert", content = "Please enter a column index", style = 'danger')
        input_error <- TRUE 
      } else {
        shinyBS::closeAlert(session, "criteria_type_col_index_alert")
      }
      if(input_error) {
        print("input error, returning")
        return
      }
      
      if (sessionInfo$criteria_type_modal_state == 'Neutral' && input_error == FALSE) {
        # insert
        print("criteria type need to do an insert")
        ct_insert <- "insert into criteria_types(criteria_type_id, criteria_type_code, criteria_type_title, criteria_type_desc,
        criteria_type_active, criteria_type_sense,criteria_column_index) values (nextval('trial_diseases_sequence'),$1,$2,$3,$4,$5,$6)"
        rs <- safe_query(dbExecute, ct_insert, 
                           params = c(input$criteria_type_code,input$criteria_type_title,input$criteria_type_desc, 
                                      input$criteria_type_active_rb, input$criteria_type_sense_rb, input$criteria_column_index))
        print(rs)
        sessionInfo$refresh_criteria_types_counter <- sessionInfo$refresh_criteria_types_counter + 1
        # save was successful, and close the panel and clear the fields
        toggleModal(session,  "add_criteria_type_bsmodal", toggle = "close")
        
      } else if (input_error == FALSE ) {
        print("criteria type need to do an update")
        print(paste("need to update type ", sessionInfo$criteria_type_modal_type_id))
        ct_update_sql <- "update criteria_types set criteria_type_code = $1, criteria_type_title = $2, 
        criteria_type_desc = $3, criteria_type_active = $4, criteria_type_sense = $5 , criteria_column_index = $6 where criteria_type_id = $7"

        rs <- safe_query(dbExecute, ct_update_sql, 
                        params = c(input$criteria_type_code,input$criteria_type_title,input$criteria_type_desc, 
                                   input$criteria_type_active_rb, input$criteria_type_sense_rb,input$criteria_column_index,
                                   sessionInfo$criteria_type_modal_type_id)) 
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
      sessionInfo$df_crit_types <- safe_query(dbGetQuery, crit_type_sql)
      sessionInfo$df_crit_type_titles <- safe_query(dbGetQuery, crit_type_titles_sql)
      sessionInfo$df_crit_types_with_all <- safe_query(dbGetQuery, crit_type_sql_with_all)
      
      
      # MAYBE
      sessionInfo$df_criteria_nct_id <- safe_query(dbGetQuery, criteria_nct_ids_sql)
      

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
                     "select nct_id, criteria_type_id, trial_criteria_orig_text, trial_criteria_refined_text, trial_criteria_expression, 
                       to_char(update_date, 'YYYY-MM-DD HH24:MI:SS') as update_date, update_by 
                   from trial_criteria where criteria_type_id = $1 order by nct_id"
                   sessionInfo$df_trial_criteria_for_type <-
                     safe_query(dbGetQuery,
                                trial_criteria_for_type_sql,
                                params = c(crit_type_sel))

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
tc.trial_criteria_expression, to_char(tc.update_date, 'YYYY-MM-DD HH24:MI:SS') as update_date, tc.update_by
from trial_criteria tc join criteria_types ct on tc.criteria_type_id= ct.criteria_type_id
where tc.nct_id = $1"
      sessionInfo$df_trial_criteria_for_nct_id <-
        safe_query(dbGetQuery,
                   trial_crit_for_ncit_id_sql,
                   params = c(nct_id_sel))
      

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
        trial_criteria_expression, update_date, update_by) values($1,$2,$3,$4,$5,now(),$6)"
    update_crit_sql <- "update trial_criteria set trial_criteria_orig_text = $1 , trial_criteria_refined_text = $2, 
    trial_criteria_expression = $3, update_date = now(), update_by = $4 where nct_id = $5 and criteria_type_id = $6 " 
    
    type_row_df <- sessionInfo$df_crit_type_titles[sessionInfo$df_crit_type_titles$criteria_type_title == input$criteria_type_typer,]
    
    if(sessionInfo$criteria_modal_state %in% c("AddByType","Neutral") && input_error_crit == FALSE ) {
     # browser()
      # Need to insert a new record from the add by type path
    

      error_happened <- FALSE
      tryCatch( {
        rs <- safe_query(dbExecute, insert_crit_sql, 
                        params = c(input$criteria_per_trial_nct_id, type_row_df$criteria_type_id, 
                                   input$criteria_per_trial_original_text,input$criteria_per_trial_refined_text,
                                   input$criteria_per_trial_expression, #format_iso_8601(Sys.time()),
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
       
        
      }
      )
      

      
      
    } else if (sessionInfo$criteria_modal_state %in% c("EditByType","EditByTrial")  && input_error_crit == FALSE ) {
      
      tryCatch( {
      rs <- safe_query(dbExecute, update_crit_sql, 
                      params = c(input$criteria_per_trial_original_text,input$criteria_per_trial_refined_text,
                                 input$criteria_per_trial_expression, #format_iso_8601(Sys.time()),
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
      del_crit_sql <- "delete from trial_criteria where nct_id = $1 and criteria_type_id = $2"
      
      shinyalert("Confirm delete", "Delete the selected criteria?" , 
                 type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                 callbackR = function(x) {
                   if (x == TRUE) {
                      rs <- safe_query(dbExecute, del_crit_sql, 
                              params = c(df_sel_crit$nct_id, df_sel_crit$criteria_type_id))
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

    results <- eval_prior_therapy_app(csv_codes, input$criteria_per_trial_expression , safe_query,
                           eval_env =
                             patient_data_env)
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

            # Turn on fk enforcement for this connection.
            
            #rs <- dbExecute(sessionCon,'PRAGMA foreign_keys = ON')
            tryCatch( {
            ret <- safe_query(dbWriteTable, 'trial_criteria', new_crits_df, overwrite = FALSE, append = TRUE)
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
      del_crit_sql <- "delete from trial_criteria where nct_id = $1 and criteria_type_id = $2"
      
      shinyalert("Confirm delete", "Delete the selected criteria?" , 
                 type = "warning", showCancelButton = TRUE, showConfirmButton = TRUE, confirmButtonText = "Delete",
                 callbackR = function(x) {
                   if (x == TRUE) {
                     rs <- safe_query(dbExecute, del_crit_sql, 
                                     params = c(sel_nct_id, sel_criteria_type_id))
                     sessionInfo$refresh_criteria_counter <- sessionInfo$refresh_criteria_counter + 1
                   }
                 }
      )
           # df_sel_crit <- sessionInfo$df_trial_criteria_for_type[input$trial_crit_per_trial_rows_selected,]
      #print(df_sel_crit)
    }
  )
  
  
  # Download trial data by type ----
  output$downloadTrialDataByType <- downloadHandler(
    filename = function() { paste('sec_trial_data_by_type_csv_', Sys.Date(), '.csv', sep = "") }
    ,
    content = function(file) {
      print("writing file")
      write.csv( sessionInfo$df_trial_criteria_for_type, file, row.names = FALSE)
    }
  )
  
  output$downloadCandData <- downloadHandler(
    filename = function() { paste('sec_candidate_data_csv_', Sys.Date(), '.csv', sep = "") }
    ,
    content = function(file) {
      print("writing file")
      write.csv( sessionInfo$df_crit_with_cands_for_type, file, row.names = FALSE)
    }
  )
  
  output$downloadTokenizerData <- downloadHandler(
    filename = function() { paste('sec_tokenizer_data_csv_', Sys.Date(), '.csv', sep = "") }
    ,
    content = function(file) {
      print("writing file")
      write.csv( sessionInfo$tokenizerData, file, row.names = FALSE)
    }
  )
  
  # Download NCIT Path data ----
  output$downloadNCITData <- downloadHandler(
    filename = function() { paste('sec_ncit__path_data_csv_', Sys.Date(), '.csv', sep = "") }
    ,
    content = function(file) {
      print("writing file")
      write.csv( sessionInfo$ncit_path_data, file, row.names = FALSE)
    }
  )
  
  # Synthea Test Data tab
  observe({
    synthea_codes_name_sql <- "select code, name, count(*) as c
        from testdata.synthea_test_codes inner join testdata.synthea_test_data on testdata.synthea_test_codes.code=testdata.synthea_test_data.raw_concept_code
        group by code, name order by c desc;"
    sessionInfo$df_synthea_codes_name <- safe_query(dbGetQuery, synthea_codes_name_sql)

    synthea_codes_name_dt <- datatable(
      sessionInfo$df_synthea_codes_name,
      class = 'cell-border stripe compact wrap hover',
      selection = 'single',
      colnames = c('Code', 'Code Name', 'Patient Count'),
      options = list(
        escape = FALSE,
        searching = FALSE,
        paging = TRUE,
        info = FALSE,
        columnDefs = list(# Initially hidden columns
          list(
            visible = FALSE,
            targets = c(0, 1, 0)
          ))
      )
    )
    
    output$synthea_codes_name <-
      DT::renderDataTable({
        synthea_codes_name_dt
      })
    
  })
  
  # Onclick handler for when a Code in the Synthea Test Data tab is clicked;
  # queries and renders Synthea test data matching that code.
  observeEvent(input$synthea_codes_name_rows_selected, {
    if (!is.null(input$synthea_codes_name_rows_selected)) {
      code <- sessionInfo$df_synthea_codes_name$code[[input$synthea_codes_name_rows_selected]]
      test_data_sql <- "select ptnum, raw_concept_code, raw_value, vocabulary_id, actual_concept_code, concept_cd, valtype_cd, tval_char, nval_num
          from testdata.synthea_test_data where raw_concept_code = $1"
      sessionInfo$df_synthea_data <- safe_query(dbGetQuery, test_data_sql, params = c(code))
      
      synthea_data_dt <-
        datatable(
          sessionInfo$df_synthea_data,
          class = 'cell-border stripe compact wrap hover',
          selection = 'single',
          width  = "90vw",
          colnames = c(
            'ptnum',
            'raw_concept_code',
            'raw_value',
            'vocabulary_id',
            'actual_concept_code',
            'concept_cd',
            'valtype_cd',
            'tval_char',
            'nval_num'
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
      output$synthea_data_by_code <-
        DT::renderDataTable({
          synthea_data_dt
        })
    }
  }, ignoreNULL = TRUE)
  
}


shinyApp(ui = ui, server = server)