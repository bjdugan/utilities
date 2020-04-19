# functions for simple calculation of EIs, formatting EIs. 
require(tibble)
require(dplyr)

ei_dict <- tibble(
  EI = factor(c("HO", "RI", "LS", "QR", "CL", "DD", "SF", "ET", "QI", "SE"),
  levels = c("HO", "RI", "LS", "QR", "CL", "DD", "SF", "ET", "QI", "SE")),
  theme = factor(
    c(rep("Academic Challenge", 4), 
      "Learning with Peers", "Learning with Peers", 
      "Experiences with Faculty", "Experiences with Faculty", 
      "Campus Environment", "Campus Environment"), 
    levels = c("Academic Challenge", 
               "Learning with Peers", 
               "Experiences with Faculty",
               "Campus Environment")), 
  variables = c('c("HOapply", "HOanalyze", "HOevaluate", "HOform")',
  'c("RIintegrate", "RIsocietal", "RIdiverse", "RIownview", "RIperspect", "RInewview", "RIconnect")',
  'c("LSreading", "LSnotes", "LSsummary")',
  'c("QRconclude", "QRproblem", "QRevaluate")',
  'c("CLaskhelp", "CLexplain", "CLstudy", "CLproject")',
  'c("DDrace", "DDeconomic", "DDreligion", "DDpolitical")',
  'c("SFcareer", "SFotherwork", "SFdiscuss", "SFperform")',
  'c("ETgoals", "ETorganize", "ETexample", "ETdraftfb", "ETfeedback")',
  'c("QIstudent", "QIadvisor", "QIfaculty", "QIstaff", "QIadmin")',
  'c("SEacademic", "SElearnsup", "SEdiverse", "SEsocial", "SEwellness", "SEnonacad", "SEactivities", "SEevents")')
  
)
  
order_eis <- function(x) {
  # EIs if character will be sorted alpha, but we have internal ordering.
  stopifnot(all(x %in% ei_dict$EI))
  
  if (is.character(x)) {
    factor(x, levels =  levels(ei_dict$EI))
  }
}
# demo
# sort(levels(ei_dict$EI))
# order_eis(sort(levels(ei_dict$EI)))
