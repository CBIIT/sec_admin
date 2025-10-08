# Credentials data
credentials <- data.frame(
  user = c("poc_admin"),
  password = c("1234"), # password will automatically be hashed using the passphrase below
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# Create the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "localdb/test.sqlite", # will be created, update `db_user_file` in config.yml
  passphrase = "1234" # `passphrase` in config.yml
)
