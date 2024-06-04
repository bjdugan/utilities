make_project_dir <- function(
    name, 
    path = getwd(), 
    subdirs = c("data", "correspondence", "plots", "reports"), 
    user = NULL, 
    project = FALSE)
{
  # create a project directory and related files  
  new_dir <- paste0(path, "/", name, "/")
  
  # windows follows C:/users/username
  if (is.null(user)) user <- unlist(strsplit(getwd(), "/"))[3]
  
  if (!dir.exists(new_dir)) {
    dir.create(new_dir)
    
    paste0(new_dir, subdirs) |> 
      lapply(dir.create)
    
    # add readme
    file.create(paste0(new_dir, "readme.md"))
    writeLines(paste0("# ", name, "\n", Sys.time(), "\n\n"),
               paste0(new_dir, "readme.md"))
    
    # add template process document 
    file.create(paste0(new_dir, "process_doc.rmd"))
    paste0(
      "---\n",
      "title: Process Document: ", name, "\n",
      "author: ", user, "\n",
      "date: ", Sys.Date(), "\n", 
      "output: html_document\n", 
      "---\n",
      "```{r setup, include=FALSE}\n",
      "# load packages, funs, data here\n",
      "```\n",
      "## Purpose\n\n", 
      "## Background\n\n",
      "## Scope\n\n",
      "Owner: ", user, "\n\n",
      "Support:\n\n",
      "### Timeline\n\n", 
      "### Definitions\n\n",
      "## Process\n\n",
      "## Resources\n\n"
    ) |> 
      writeLines(paste0(new_dir, "process_doc.rmd"))
    
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
