make_project_dir <- function(
    name, 
    path = getwd(), 
    subdirs = c("data", "correspondence", "plots", "reports"), 
    user = NULL, 
    project = FALSE)
{
  # create a project directory and related files  
  new_dir <- paste0(path, "/", name, "/")

  if (!dir.exists(new_dir)) {
    dir.create(new_dir)
    
    paste0(new_dir, subdirs) |> 
      lapply(dir.create)
    
    # add readme
    file.create(paste0(new_dir, "readme.md"))
    writeLines(paste0("# ", name, "\n", Sys.time(), "\n\n"),
               paste0(new_dir, "readme.md"))
    
    # add template process document 
    source("make_process_doc.r")
    make_process_doc(name, path = new_dir, user = user)
    
    # add Rstudio project
    if (isTRUE(project)) usethis::create_project(new_dir)
  }
}

# examples
# a simple directory in the current one
#make_project_dir("my project")

# create only selected subdirs (e.g. if not making reports), with an actual name (not PC username), and open the Rstudio project.
# make_project_dir(
#   name = "my project lite",
#   path = "C:/users/bjdugan/desktop/",
#   subdirs = c("data", "plots"),
#   user = "Brendan",
#   project = TRUE)

