## TODO: Search and replace "Light Tigger" ans "Paused" strings in raw files

library(tidyverse)
library(magrittr)

header_length <- 14 # How many lines is the header
footer_length <- 2 # How many lines is the footer

## FUNCTION: Remove the last marker from a data.frame
# INPUT: A dataframe with time(int), value(dbl), and marker(int) columns
# OUTPUT: The same dataframe with the last marker replaced by NA
# EXAMPLE: remove_last_marker(clean_df)

remove_last_marker <- function(df){
        last_marker <-
                df %>% 
                filter(marker == 1) %>% 
                summarise(time = last(time)) %$% 
                time
        
        df %>% 
                mutate(marker = if_else(time == last_marker, NA_integer_, marker))
}

## FUNCTION: Add a specified marker to a data.frame
# INPUT: df: dataframe with time(int), value(dbl), and marker(int) columns
#        manual_marker: a single marker(int)
# OUTPUT: The same dataframe with the marker added
# EXAMPLE: add_marker(clean_df, 14588)

add_marker <- function(df, manual_marker){
        df %>% 
                mutate(marker = if_else(time == manual_marker, 1L, marker))
        
}

## FUNCTION: Strip header and footer, and remove false markers from a file, while changing the time format to seconds. 
# INPUT: file_name: path to a biotrace file to be cleaned
#        treshold: the smallest number of samples between two markers. 320 corresponds to 10 seconds
#        manual_requests: a data frame that contains the manual change requests. Columns: file(chr), action(chr), marker(int)
# OUTPUT: A cleaned file (same name but different dir), 00_false_marker_log.txt contains the details #         of the changes made.
# EXAMPLE: clean_markers(paste0(path_raw, "/12_recall_SCR.txt"), treshold = 320)

clean_markers <- function(file_name,
                          treshold = 320,
                          manual_requests){
        
        # Extract from manual requests the file names where the last marker has to be deleted
        clean_last_files <-
                manual_requests %>% 
                filter(action == "cut last marker") %$% 
                file
        
        # Extract from manual requests the file names where a marker has to be added
        add_marker_files <-
                manual_requests %>% 
                filter(action == "add marker") %$% 
                file
        
        # Extract from manual requests which markers to add
        if (file_name %in% add_marker_files){
                manual_marker <- 
                        manual_requests %>% 
                        filter(file == file_name) %$% 
                        marker
        }
        
        # Save the length of file
        file_length <- read_lines(file_name) %>% length
        # Save the header
        header <- read_lines(file_name, n_max = header_length) 
        # Save the footer
        footer <- read_lines(file_name, skip = file_length - footer_length) 
        # Get sampling frequency
        sampling_frequency <- 
                header %>% 
                grep("Output rate", ., value = T) %>% 
                strsplit("\t") %>% 
                extract2(1) %>% 
                extract(2) %>% 
                as.numeric()
        # Read the data
        df <-
                read_tsv(
                        file_name,
                        skip = header_length,
                        col_names = FALSE,
                        col_types = list(col_integer(), col_double(), col_integer(), col_character()),
                        trim_ws = TRUE,
                        n_max = file_length - header_length - footer_length
                ) %>%
                select(time = X1,
                       value = X2,
                       marker = X3)
        
        # Extract the time of the wrong markers
        false_markers <-
                df %>%
                mutate(marker = if_else(is.na(marker), 0L, 1L), # make 1: marker, 0: no marker
                       cum = cumsum(marker)) %>% # Create a variable for grouping purposes
                group_by(cum) %>%
                summarise(time = first(time),
                          N = n()) %>% 
                mutate(short = if_else(N < treshold, 1L, 0L),
                       # Identify too short time bw markers
                       false_marker = lag(short),  # identify the wrong marker
                       # Also consider the last marker false if it is too close to the end
                       false_marker = if_else((short == 1L & last(time) == time), 1L, false_marker)) %>%
                # as.data.frame()
                filter(false_marker == 1) %$%
                time
        
        clean_df <- df 
        clean_df[which(clean_df$time %in% false_markers), "marker"] <- NA_integer_ # Remove false markers
        # Apply manual requests here, based on a config file
        # First, remove last markers where not needed
        if (file_name %in% clean_last_files) clean_df <- remove_last_marker(clean_df)
        if (file_name %in% add_marker_files) clean_df <- add_marker(clean_df, manual_marker)
        
        clean_df %<>% 
                mutate(time = time/sampling_frequency, 
                       value = value %>% round(3),
                       marker = marker %>% as.character())
        
        # Write the clean file to a different library with the same filename
        # This file only contains the data, with variables: Time, SC, Marker. No header or footer.
        # This can be imported to Ledalab as 'Text 1' type file.
        write_tsv(
                format(
                        clean_df %>% as.data.frame(),
                        nsmall = 3,
                        na.encode = FALSE
                ),
                gsub("raw", "clean", file_name),
                append = F,
                na = "",
                col_names = F
        )
        
        # Create a log file about the removed markers
        write_lines(
                paste0(
                        "Date : ",
                        date(),
                        "\nTreshold : ",
                        treshold,
                        "\nFile name: ",
                        gsub(paste0(path_raw, "/"), "", file_name),
                        "\nGood markers: ",
                        clean_df$marker %>% as.numeric() %>% sum(na.rm = T),
                        "\nFalse markers removed: ",
                        false_markers %>% length,
                        "\nFalse marker timestamps: ",
                        paste(false_markers, collapse = "\t"),
                        "\nManual changes: ",
                        manual_requests[file_name == manual_requests$file, "action"] %>% unlist(use.names = F),
                        "\t", ifelse(exists("manual_marker"), manual_marker, ""),
                        "\n"
                ),
                paste0(path_clean, "/00_false_marker_log.txt"),
                append = T
        )
}

quietly_read_tsv <- quietly(read_tsv)
safely_select <- safely(select)
possibly_select <- possibly(select, NULL)
