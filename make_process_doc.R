make_process_doc <- function(name, path = getwd(), user = NULL, full_opts = TRUE) {
  # for new tasks, processes, projects, create a process document
  
  # windows follows C:/users/username
  if (is.null(user)) user <- unlist(strsplit(getwd(), "/"))[3]
  
  if (!grepl("/$", path)) path <- paste0(path, "/")
  
  file.create(paste0(path, name, ".rmd"))

  paste0(
    '---\n',
    'title: "Process Document: ', name, '"\n',
    'author: "', user, '"\n',
    'date: "`r Sys.Date()`"\n', 
    if (!full_opts) {'output: html_document:\n'} else {
      paste0(
        'output:\n',
        '  html_document:\n',
        '    toc: yes\n',
        '    toc_float: yes\n',
        '    theme: paper\n',
        '    code_folding: hide\n',
        '    css: styles.css\n'
      )
    },
    '---\n',
    '```{r setup, include=FALSE}\n',
    '# load packages, funs, data here\n',
    '```\n',
    '### Purpose\n\n', 
    '### Background\n\n',
    '### Scope\n\n',
    'Owner: ', user, '\n\n',
    'Support:\n\n',
    '#### Timeline\n\n', 
    '#### Definitions\n\n',
    '### Process\n\n',
    '### Resources\n\n'
  ) |> 
    writeLines(paste0(path, name, ".rmd"))
}
# e.g. 
# make_process_doc("test", user = "me")
# rmarkdown::render("test.rmd")
