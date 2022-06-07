library(tidyverse)

fos <- read_delim("data/mag_fos/FieldsOfStudy.txt",
                  delim = "\t",
                  col_names = c( "fieldofstudyid",
                                 "rank",
                                 "normalizedname",
                                 "displayname",
                                 "maintype",
                                 "level",
                                 "papercount",
                                 "citationcount",
                                 "createddate"),
                  col_types = cols(
                    fieldofstudyid = col_double(),
                    rank = col_double(),
                    normalizedname = col_character(),
                    displayname = col_character(),
                    maintype = col_character(),
                    level = col_double(),
                    papercount = col_double(),
                    citationcount = col_double(),
                    createddate = col_date(format = "")
                  ))


fos_children <- read_delim("data/mag_fos/FieldOfStudyChildren.txt",
                           delim = "\t",
                           col_names = c( "fieldofstudyid",
                                          "childfieldofstudyid"),
                           col_types = cols(
                             fieldofstudyid = col_double(),
                             childfieldofstudyid = col_double()
                           ))


# start with the top

get_level <- function(fos, selection = 0, children = fos_children) {
  fos %>%
    filter(level == selection) %>%
    select(fieldofstudyid, !!glue::glue("level_{selection}") := displayname) %>%
    left_join(children, by = "fieldofstudyid")
}

# get all 6 levels (0-5)
lvl_0 <- get_level(fos, 0)
lvl_1 <- get_level(fos, 1)
lvl_2 <- get_level(fos, 2)
lvl_3 <- get_level(fos, 3)
lvl_4 <- get_level(fos, 4)
lvl_5 <- get_level(fos, 5)


res <- lvl_0 %>%
  left_join(lvl_1, by = c("childfieldofstudyid" = "fieldofstudyid"),
            suffix = c("_0", "_1")) %>%
  left_join(lvl_2, by = c("childfieldofstudyid_1" = "fieldofstudyid"),
            suffix = c("_1", "_2")) %>%
  left_join(lvl_3, by = c("childfieldofstudyid_2" = "fieldofstudyid"),
            suffix = c("_2", "_3")) %>%
  left_join(lvl_4, by = c("childfieldofstudyid" = "fieldofstudyid"),
            suffix = c("_3", "_4")) %>%
  left_join(lvl_5, by = c("childfieldofstudyid_4" = "fieldofstudyid"),
            suffix = c("_4", "_5"))
res

# here, we do not try to rebuild the whole hierarchy. we are just interested
# in a look-up, so we can find the top-level FOS for a given fos_id
res_long <- res %>%
  pivot_longer(starts_with("child")) %>%
  distinct(fieldofstudyid, level_0, value) %>%
  rename(fos_id_top = fieldofstudyid, top_fos = level_0, fos_id = value)

res_long


write_csv(res_long, "data/mag_fos/fos_lookup.csv")
write_csv(res, "data/mag_fos/fos_wide.csv")

