# File for utility functions

# Ensure all tables exist
# Create shopping_list table if it doesn't exist
# Open database connection

# Edit dbs
 # db <- dbConnect(SQLite(), "src/data/family_management.db")
 #  dbRemoveTable(db, "calendar_db")
 #  dbDisconnect(db)

ensure_database_setup<- function(db) {
  dbExecute(db, "CREATE TABLE IF NOT EXISTS meal_lookup_table (Item TEXT, 
            Quantity INTEGER,
            Unit TEXT)")
  
  # recreate the lookup table each time?
  meal_plan <- readr::read_csv("data/meal_lookup_table_editable.csv")
  dbWriteTable(db, "meal_lookup_table", meal_plan, overwrite = TRUE, row.names = FALSE)
  
  dbExecute(db, "CREATE TABLE IF NOT EXISTS shopping_list_db (Item TEXT, 
            Quantity INTEGER,
            Unit TEXT)")
  dbExecute(db, "CREATE TABLE IF NOT EXISTS finances_db (Type TEXT,
            Item TEXT,
            Quantity INTEGER)")
  dbExecute(db, "CREATE TABLE IF NOT EXISTS fitness_db (
                    Name TEXT,
                    Goal TEXT,
                    Plan TEXT,
                    Milestone TEXT,
                    Deadline DATE)")
  dbExecute(db, "CREATE TABLE IF NOT EXISTS calendar_db (title TEXT, 
            start DATE,
            end DATE,
            description TEXT)")
  dbExecute(db, "CREATE TABLE IF NOT EXISTS meal_planning_db (Item TEXT, 
            Date DATE,
            NeedIngred TEXT)")
  dbExecute(db, "CREATE TABLE IF NOT EXISTS chores_db (Chore TEXT, 
            AssignedTo DATE,
            DueDate DATE,
            Recurrence TEXT,
            Status TEXT,
            LastUpdated DATE)")

  dbExecute(db, "CREATE TABLE IF NOT EXISTS message_board_db (threadID TEXT,
            messageID TEXT,
            author TEXT,
            comment TEXT)")
}



load_data <- function(db, table_name) {
  dbReadTable(db, table_name)
}


save_data <- function(df, table_name) {
  dbWriteTable(db, table_name, df, overwrite = TRUE)
}