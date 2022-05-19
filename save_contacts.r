# save contact lists locally, in db

save_contacts <- function(df, table_name, overwrite = FALSE) {
  # if overwrite =  FALSE but table exists, stop
  msg2 <- "\nWriting to NSSE db..."
  if (dbExistsTable(NSSE, table_name) & isFALSE(overwrite)) {
    cat("Table(s) exist but overwrite=FALSE\n")
    stop()
  } else if (dbExistsTable(NSSE, table_name) & isTRUE(overwrite)) {
    msg2 <- "\nOverwriting table on NSSE db..."
  }

  cat("\014Writing locally...")
  xlsx::write.xlsx(as.data.frame(df),
                   paste0("data/", table_name, ".xlsx"),
                   row.names = FALSE,
                   showNA = FALSE)
  cat(msg2)
  if (dbExistsTable(NSSE, table_name)) {
    dbRemoveTable(NSSE, table_name)
  }
  dbCreateTable(NSSE, table_name, df)
  # populate
  dbAppendTable(NSSE,  table_name, df)
  # check
  cat(paste0("\nTable written to NSSE.", table_name, "\n"))
  tbl(NSSE, table_name)
}
# e.g. will write mtcars (1st run)
save_contacts(mtcars, "mtcars")
# will stop (mtcars exists)
save_contacts(mtcars, "mtcars")
# will overwrite
save_contacts(mtcars, "mtcars", overwrite = TRUE)
