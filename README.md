# SEC POC Admin Dashboard

**This is the administration dashboard for the [Structured Eligibility Criteria Proof of Concept (SEC POC)](https://github.com/CBIIT/sec_poc) project.**  
Before continuing here, please review the [main SEC POC README](https://github.com/CBIIT/sec_poc) for an overview of the project, database setup, and environment configuration.

## Overview

This admin dashboard is an R/Shiny application for managing user access and monitoring the SEC POC system. It is built with [shinymanager](https://datastorm-open.github.io/shinymanager/), which provides authentication and user management via an encrypted SQLite database by default.

The dashboard connects to the main SEC PostgreSQL database to generate statistics and update structured eligibility criteria.

## User Management

- **Local Development:**  
  User credentials are stored in an encrypted SQLite database.  
  To initialize a fresh user database for local testing, run [`init_user_db.R`](init_user_db.R):

  ```r
  source("init_user_db.R")
  ```

  This script creates a default admin user and sets up the database using an encryption passphrase to be referenced in `config.yml`.

- **Production:**  
  The `localdb/users.sqlite` file is deployed to production and updated live as users interact with the site.  
  **Important:** Before deploying a new app version, always download the latest `users.sqlite` from the admin dashboard to ensure you have the most up-to-date user data.

- **Migration to PostgreSQL:**  
  Migrating from SQLite to PostgreSQL is recommended since it's less error prone than working directly with the SQLite users database.
  - See [c30bbc5](https://github.com/CBIIT/sec_admin/commit/c30bbc54bf2557ca88df14bd4885f3240f992acf) as a starting point for the migration.

## Database Connection

- The app connects to the main SEC PostgreSQL database for application data and statistics.
- Database connection settings are managed in [`config.yml`](config.yml) and can be set via environment variables.

## Local Development Setup

1. **Install R 4.5.1** and [renv](https://rstudio.github.io/renv/).
2. **Restore dependencies:**
   ```r
   renv::restore()
   ```
3. **Set environment variables** as described in the [main SEC POC README](https://github.com/CBIIT/sec_poc).
4. **Initialize the user database** (see above).
5. **Run the app:**
   ```r
   Rscript app.R
   # or
   R -e "shiny::runApp()"
   ```

## Production Deployment

1. **Update user database:**  
   Download the latest `users.sqlite` from the admin dashboard and store it as `localdb/users.sqlite` before deploying.
2. **Deploy code:**  
   Follow the deployment process (see the [main SEC POC README](https://github.com/CBIIT/sec_poc) for details).

## Best Practices

- **Do not store secrets** in the repository. Use Posit Connect UI to set environment variables securely.
- **Conduct periodic backups of users.sqlite** the user database. The latest backup is in the FHIR AWS Account under s3://sec-poc-archive/shinymanager-sql-2025-10-08.sqlite
- **Consider migrating** to PostgreSQL for improved security and scalability.
