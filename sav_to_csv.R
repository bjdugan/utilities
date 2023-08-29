# convert SPSS to CSV

require(dplyr)
require(haven)
require(readr)
require(tibble)
require(purrr)

# assume files are in DL folder


sav_to_csv <- function(unitid, yr = substr(Sys.Date(), 3, 4), 
                        path = "C:/Users/bjdugan/Downloads/") {
  # maybe add stop clause for where it exists
  
  # assume it's in DL folder, make temp folder to avoid 
  unzip(paste0(path, "Data", yr, "_", unitid, ".zip"),
        exdir = paste0(path, "/tmp_", unitid))
  files <- lapply(dir(paste0(path, "tmp_", unitid), full.names = TRUE),
      read_sav) %>% 
    `names<-`(c("data", "comments"))
  tibble(x = files, 
         file = paste0(path, "tmp_", unitid, "/",
           unlist(lapply(dir(paste0(path, "tmp_", unitid)), function(x)
           gsub(pattern = "\\.sav", replacement = "\\.csv", x)))), 
         ) %>% 
    pmap(write_csv)
  tibble(x = map(files, mutate_if, is.labelled, as_factor, "labels"), 
         file = paste0(path, "tmp_", unitid, "/",
                       unlist(lapply(dir(paste0(path, "tmp_", unitid), pattern = "\\.sav"), function(x)
                         gsub(pattern = "\\.sav", replacement = " \\(labels\\)\\.csv", x)))), 
  ) %>% 
    pmap(write_csv)
  # zip
  zip(paste0(path, "Data", yr, "_", unitid, "_csv.zip"), 
      files = dir(paste0(path, "tmp_", unitid), full.names = TRUE),
      flags = "-r9Xj"
      )
  # not file.remove but unlink - also removes files 
  unlink(paste0(path, "tmp_", unitid, "/"), recursive = TRUE)
  
  return(cat("Ready to go.\n"))
}

sav_to_csv(231651)



