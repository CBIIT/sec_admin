# Credentials data
credentials <- data.frame(
  user = c("poc_admin"),
  password = c("1234"), # password will automatically be hashed
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# Create the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "localdb/users.sqlite", # will be created
  passphrase = "1234"
)
