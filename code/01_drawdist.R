



#############################################################
#
# Script for randomly sampling districts 
# for coding prioritization criteria
#
###########################################################

library(dplyr)
library(readxl)
library(googlesheets4)

gsheet_id = "1HhWVkPA4mRCoalvUNt0GSS2Ljr3omwylPSSo0I5BRsE"
dist_raw = readLines("/Users/raj2/Dropbox/XXXX-USDA-NSLPOnline/Data/ELSI_csv_export_6373994304161543022481.csv")

header = 7
footer = length(dist_raw)-7
dist_df = read.csv(textConnection(dist_raw[header:footer]), header = TRUE, stringsAsFactors = FALSE)

## clean colnames
colnames(dist_df) = gsub("\\.", "_", tolower(colnames(dist_df)))
dist_df[dist_df == "–" | dist_df == "†" |
          dist_df == "‡"] <- NA

dist_df <- dist_df %>%
  mutate(state_upper = toupper(state_name__district__latest_available_year),
         distname_init = trimws(gsub("[^[:alnum:][:space:]]", "", toupper(agency_name))),
         distname_clean = trimws(gsub("[\\(]?[0-9][0-9]+[\\)]?$", "", distname_init)))

## sort by total enrollment 
dist_df_sorted = dist_df %>%
          mutate(totstud = as.numeric(total_students_all_grades__excludes_ae___district__2018_19),
                 lep_num = as.numeric(limited_english_proficient__lep____english_language_learners__ell___district__2018_19),
                 frpl_num = as.numeric(free_and_reduced_lunch_students__public_school__2018_19),
                lep_perc = lep_num/totstud,
                frpl_perc = frpl_num/totstud) %>%
          dplyr::select(-contains("num")) %>%
          arrange(desc(totstud))

## pull name of district, nces id, and state
set.seed(072413)
dist_df_pull = dist_df_sorted %>%
          slice(1:100) %>%
          sample_n(100) %>%
          mutate(coder = sample(c("Rosy",
                                  "Kate"), 
                                100, 
                                replace = TRUE),
            nces_id = 
                gsub("^=", "", 
                     agency_id___nces_assigned__district__latest_available_year)) %>%
          dplyr::select(coder,
                        state_upper,
                        county_name__district__2018_19,
                        nces_id,
                        distname_clean,
                        web_site_url__district__2018_19,
                        totstud)

sheet_write(dist_df_pull %>%
            filter(coder == "Rosy"), gsheet_id,
            sheet = "Rosy")

sheet_write(dist_df_pull %>%
              filter(coder == "Kate"), gsheet_id,
            sheet = "Kate")


