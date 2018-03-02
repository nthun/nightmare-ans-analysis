# DESCRIPTION: Calculates two types of AUC (area under the curve), common for calculating hormonal changes over time. The function works by calculating the area of the rectangles and triangles that make up the measurement points, with respect to time. The funcion can calculate AUCg (AUC in respect to ground) or AUCi (AUC in respect to baseline).
# For further info, check: Fekedulegn, D. B., Andrew, M. E., Burchfiel, C. M., Violanti, J. M., Hartley, T. A., Charles, L. E., & Miller, D. B. (2007). Area Under the Curve and Other Summary Indicators of Repeated Waking Cortisol Measurements. Psychosomatic Medicine, 69(7), 651–659. https://doi.org/10.1097/PSY.0b013e31814c405c
# INPUT:        time: time points of the measurements,
#               measurement: measurements,
#               type: either "g" (default) , or  "i". See description. 
# OUTPUT: A single value that represents the selected AUC type
# EXAMPLE: calculate_auc(time = c(0, 15, 45, 90), measurement = c(5, 10.2, 12.3, 11.4), type = "g")

# install.packages("tidyverse")
calculate_auc <- function(time,
                          measurement,
                          type = "g") {
        library(tidyverse)
        output <- 
        data_frame(time, measurement) %>%
                mutate(
                        length = time - lag(time), # length of time between successive timepoints
                        increase = measurement - lag(measurement), # change in measurement
                        rectangle = if_else( # Calculate the area of rectangles and triangles
                                increase >= 0,
                                length * lag(measurement), # Increasing measure calculation
                                length * measurement # Decreasing measure calculation
                        ),
                        triangle = if_else( 
                                increase >= 0,
                                length * increase / 2, # Increasing measure calculation
                                length * (lag(measurement) - measurement) / 2 # Decreasing
                        ),
                        AUC = triangle + rectangle, # Add up rectangles and triangles
                        baseline = max(time, na.rm = T) * min(measurement, na.rm = T) # baseline
                ) %>%
                summarise(AUCg = sum(AUC, na.rm = T), # Calculate both AUC metrics
                          AUCi = AUCg - max(baseline, na.rm = T))
        case_when(type == "g" ~ output$AUCg, # Return the appropriate AUC metric
                  type == "i" ~ output$AUCi)
}