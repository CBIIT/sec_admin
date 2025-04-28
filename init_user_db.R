# Credentials data
credentials <- data.frame(
  user = c("poc_admin"),
  password = c("1234"), # password will automatically be hashed
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

conf <- config::get()

shinymanager::create_sql_db(credentials_data = credentials, config_path = conf$db_config_file)
