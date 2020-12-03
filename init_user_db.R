# Credentials data
credentials <- data.frame(
  user = c("poc_admin"),
  password = c("somerealpasswordgoeshere"), # password will automatically be hashed
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path =  'filepath/users.sqlite', # will be created
  passphrase = 'a passphrase goes here '
)
