conf <- config::get()
sqlite <- DBI::dbConnect(RSQLite::SQLite(), dbname=conf$db_user_file)
pg <- DBI::dbConnect(RPostgres::Postgres(), dbname=conf$dbname, user=conf$user_admin, host=conf$host, port=conf$port, password=conf$password_admin)
logs <- shinymanager::read_db_decrypt(conn=sqlite, name="logs", passphrase=conf$passphrase)
creds <- shinymanager::read_db_decrypt(conn=sqlite, name="credentials", passphrase=conf$passphrase)
pwd_mngt <- shinymanager::read_db_decrypt(conn=sqlite, name="pwd_mngt", passphrase=conf$passphrase)
# This will create the SQL tables specified in pg_template.yml
shinymanager::create_sql_db(credentials_data=creds, config_path=conf$db_config_file)
# Make sure the types match the table schema
pwd_mngt$n_wrong_pwd <- 0
pwd_mngt$must_change <- as.logical(pwd_mngt$must_change)
pwd_mngt$have_changed <- as.logical(pwd_mngt$have_changed)
pwd_mngt$date_change <- as.Date(pwd_mngt$date_change)
# By default, the pwd_mngt and logs are not migrated over
DBI::dbWriteTable(pg, name="pwd_mngt", value = pwd_mngt, overwrite=T)
DBI::dbWriteTable(pg, name="logs", value=logs, overwrite=T)
