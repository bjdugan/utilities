# gather contacts from given range of NSSE years
# could expand to allow for FSSE or BCSSE e.g. by replacing "matches(...)" with project detected in criteria

get_contacts <- function(project_years, contact_types = 1:2) {

  require(DBI)
  require(dplyr)
  require(dbplyr)
  require(odbc)
  require(tidyr)

  stopifnot(is.character(project_years),
            dbCanConnect(odbc(), "CPR_NSSE"),
            dbCanConnect(odbc(), "CPR_Interface")
            )


  NSSE <- dbConnect(odbc(), "CPR_NSSE")
  CPR_interface <- dbConnect(odbc(), "CPR_Interface")

  semi_join(
    tbl(NSSE, "master_schools") %>%
      filter(is.na(EXCLUDE) & unitid > 0) %>%
      select(unitid, name_report = NAME_REPORT),
    tbl(NSSE, "school_info") %>%
      select(unitid = IPEDS, matches("NSSE\\d+")) %>%
      pivot_longer(-unitid) %>%
      filter(unitid > 0 &
               (name %in% project_years &
                  value == -1)) %>%
      distinct(unitid),
    by = "unitid") %>%
    collect() %>%
    left_join(
      tbl(CPR_interface, "contacts_all_current") %>%
        filter(contactType %in% contact_types & study == "NSSE" & active == 1) %>%
        select(unitid, fn, ln, email, contact_type = contactType) %>%
        collect(),
      by = "unitid"
    ) %>%
    mutate(contact_type = case_when(contact_type == 1 ~ "1 - CPM",
                                    contact_type == 2 ~ "2 - CAC")) %>%
    distinct(email, .keep_all = TRUE) %>%
    arrange(unitid, contact_type)
}
# e.g.
# get_contacts(paste0("NSSE", 2015:2017))
