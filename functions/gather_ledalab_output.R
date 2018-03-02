# This script collects all data together
library(tidyverse)
library(readxl)
library(forcats)

path_clean = paste(getwd(), "data", "clean", sep = "/")
output_files <- list.files(path_clean, pattern = ".*_era.txt") # Specify pattern of filenames to read
path_xlsx = paste(getwd(), "data", "xlsx", sep = "/")
xlsx_files <- list.files(path_xlsx, pattern = ".*.xlsx")

# Read all files put gather them into a data_frame
sc_data <-
        data_frame(file_name = output_files) %>% # Create var to contain filenames
        mutate( content = map(paste0(path_clean, "/", file_name), ~read_tsv(.)), # Read files into nested dfs
                TestId = gsub("^(\\d{2})_.*$","\\1", file_name), # Make TestId readable
                session = gsub(".*(recall).*|.*(emot).*","\\1\\2", file_name),
                stim_group = gsub("^.*_(A).*$|.*_(B).*$|.*_(recall).*$","\\1\\2", file_name) ,
                stim_group = if_else(stim_group == "", NA_character_, stim_group)
        ) %>% 
        arrange(TestId, session) %>% 
        mutate(stim_group = zoo::na.locf(stim_group)) %>% 
        unnest(content) %>% # Unpack
        select(TestId, session, stim_group, event = Event.Nr, SCR = CDA.SCR, SCL = CDA.Tonic, nSCR = CDA.nSCR) %>% # Keep only important variables and rename them
        gather(metric, value, -TestId, -session, -stim_group, -event) %>% # Tidy format
        # group_by(TestId, metric) %>%
        # mutate(value = if_else(metric != "nSCR",scale(value),value)) %>%  # Standardize values by TestId and metric
        # ungroup() %>% 
        spread(metric, value) %>% 
        filter(!TestId %in% c(10))

# TODO: group is still not perfectly parsed! ??
## TODO: filter completely bad recordings, like 10_A_emot_SCR.txt

stimuli <-
        data_frame(file_name = paste0(path_xlsx, "/", xlsx_files)) %>% # Create var to contain filenames
        mutate(
                name = gsub(".*/(.*)_teszt_excel.xlsx$", "\\1", file_name),
                TestId = gsub("^(\\d{2})_.*$", "\\1", name),
                session = gsub("^\\d{2}_*(.*)_.$", "\\1", name),
                session = if_else(session == "", "emot", session),
                stim_group = gsub(".*_(.)$", "\\1", name),
                content = map(file_name, ~ read_xlsx(., 1, na = c("", "NA")))
        ) %>%
        mutate(content = map(content, ~ select(.,
                                               event = matches("Event.Nr"),
                                               stimulus = matches("Pictures"),
                                               # category = matches("Neut.Neg."),
                                               block = matches("Block"),
                                               familiarity = contains("familiarity"),
                                               picture_set = contains("old(1)_new(9)"),
                                               arousal = contains("arousal"),
                                               valence = matches("response_valencia")
        ))) %>%
        mutate(content = map(content, ~ drop_na(.))) %>% 
        select(-file_name, -name) %>%
        unnest(content) %>% 
        mutate(stimulus = stimulus %>% as.character()) %>% 
        mutate(picture_set = case_when(
                                        picture_set == 1 ~ "Original",
                                        picture_set == 9 ~ "New",
                                        TRUE ~ picture_set %>% as.character())
        )

# Create picture data
# stimuli %>%
#         select(stimulus, category) %>% 
#         mutate(category = case_when(category == 0 ~ "Neutral",
#                                     category == 1 ~ "Negative",
#                                     TRUE ~ category %>% as.character()
#                                     )
#                ) %>% 
#         drop_na() %>%
#         filter(!duplicated(stimulus)) %>%
#         arrange(category, stimulus) %>% 
#         write_tsv("d:/Documents/GitHub/own projects/EDA process/data/stimuli.csv")

# temp %>% slice(c(2, 31, 40, 43, 47, 49))

# Load picture data
pictures <- 
        read_tsv("d:/Documents/GitHub/own projects/EDA process/data/stimuli.csv",
                 col_types = list(col_character(), col_character(), col_character()))

# Participant data
participants <- 
        read_excel("D:/Documents/GitHub/own projects/EDA process/data/SCR_all.xlsx", 1) %>% 
        select(TestId = Subj, stim_group = first_list, group) %>% 
        mutate(group = recode(group, "N" = "Nightmare", "C" = "Control"))

# Match markers to pictures
merged <-
        sc_data %>% 
        left_join(participants, by = c("TestId", "stim_group")) %>% 
        left_join(stimuli, by = c("TestId", "session", "stim_group","event")) %>% 
        left_join(pictures, by = "stimulus")

# Save merged file
# write_csv(merged, "./data/merged_data.csv")


# TODO: 
# 1: Write 
# 2: Detect if the tester has a SC response, and exclude those who doesn't
# 3: Create statistical analysis


merged <- read_csv("./data/merged_data.csv")
merged %>% 
        group_by(TestId, group, session) %>% 
        summarise(SCR = mean(SCR, na.rm = T),
                  SCL = mean(SCL, na.rm = T)
        )  %>% 
        gather(metric, value, c("SCR","SCL")) %>% 
        spread(session, value) %>% 
        rowwise() %>% 
        mutate(diff = recall - emot,
               ratio = recall/emot) %>% 
        select(-recall, -emot) %>%
        gather(calc, value, c("diff", "ratio")) %>% 
        unite(matric_calc, c("metric", "calc")) %>% 
        spread(matric_calc, value) %>% 
        write_csv("data/difference_scores.csv")

