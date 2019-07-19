# project template
# a simple and consistent way to add project logs and new project directories. 
# can be used prior to adding .Rproj to existing directory.

log_project <- function(mes, dir = "./") {
  stopifnot(is.character(mes), 
            dir.exists(dir))
  require(stringr)
  
  # extract nth element of dir, the project name
  project <- str_split(dir, "/") %>% 
    unlist() %>% 
    `[`(length(.)) 
  
  mes <- c(as.character(Sys.time()), mes)
  file <- paste0(dir, "/project_log_", project, ".txt")
  
  # if exists, append will be set to true
  # else create
  if (file.exists(file)) {
    writeLines( c(readLines(file), mes), file )
  } else {
    writeLines(mes, file)
  }
}

add_project <- function(dir = "./"){
  require(dplyr)
  require(stringr)
  #stopifnot(!dir.exists(dir))
  
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  project <- str_split(dir, "/") %>% 
    unlist() %>% 
    `[`(length(.))
  
  dir.create(paste0(dir, "/data"))
  dir.create(paste0(dir, "/plots"))
  dir.create(paste0(dir, "/references"))
  log_project(paste("Beginning project log for", project), dir = dir)

}

catchup <- function(dir = "/", n = 8) {
  # preview last n lines
  
  
}
# # example (not run) ####
# dir <- "C:/Users/bjdugan/Desktop/example"
# add_project(dir)
# log_project("I did some things...", dir = dir)
