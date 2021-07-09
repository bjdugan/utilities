# generate dictionary information (codebook) from haven labelled SPSS datasets
# this works well for an individual SPSS data frame, or map()'d over a list of data frames, and is much easier to use than looking at str() or attributes() output

# Try adding argument so that it either returns a dictionary (default, current), or the originating (SPSS) data, 
# ie so it can be used in pipes for a "side effect".
# Need to find out how to assign return, ie with name = "name", then "name" <<- d
# alternatively, work this in with T-pipe (returns LHS instead of right), magrittr::`%T>%`
# use assign("name")

make_dict <- function(data, return_data = FALSE, name = NA, overwrite = FALSE) {
  
  require(tibble)
  require(dplyr)
  require(purrr)
  require(haven)

  # other control features?
  stopifnot(
    any(map_lgl(data, is.labelled)),
    is.data.frame(data) )
    
  d <- tibble(
    variable_name = colnames(data),
    variable_label = map(data, attributes) %>%
      map(pluck, "label") %>%
      as.character(),
    variable_format = map(data, attributes) %>%
      map(pluck, "format.spss") %>%
      as.character(),
    value_labels = map(data, attributes) %>%
      map(pluck, "labels") %>%
      as.character()
  )
  
  if (isFALSE(return_data)) {
    return(d)
  } else {
    # generic name if none provided
    if(all("dict" %in% ls(".GlobalEnv") & overwrite == FALSE)) {
      cat("\014'dict' already exists; provide a new name or set overwrite=TRUE\n")
      stop()
    } else
    n <- ifelse(is.na(name), 
                 "dict", 
                 name
                 )
    
    assign(n, d, pos = 1)
    return(data)
  }
  
}

# demo ####
# figure out how to write vignette?

# this will vary by install location and version
# demo <- haven::read_spss("C:/Program Files/IBM/SPSS/Statistics/27/Samples/English/demo.sav")
# 
# # if we want to read SPSS data but want to keep (survey) metainformation, we can make a dictionary or codebook.
# demo_dict <- make_dict(demo)
# rm(demo_dict)
# 
# # or pass along original data but make dict as side-effect
# demo_r <- make_dict(demo, return_data = TRUE, name = "demo_dict") %>% 
#   mutate_if(is.labelled, as.factor)
# 
# # only re-assign/overwrite any global obj 'dict' with overwrite = TRUE
# demo_r <- make_dict(demo, return_data = TRUE) %>% 
#   mutate_if(is.labelled, as.factor)
# 
# 
# # if we want to manipulate data originating from SPSS and return it as SPSS, or export to SPSS with complete features (variable lable, value labels, formats) then do as follows. 
# could be a related function? 
# demo_r <- mutate_all(is.labelled, as_factor, "labels")
# map2(demo_r, as.list(demo_dict$label), `attr<-`, which = "label") %>%
#   bind_rows() %>%
#   write_sav("demo.sav")
