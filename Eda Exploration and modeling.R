# EDA analysis
library(tidyverse)
library(magrittr)
library(lme4)
library(lmerTest)

# READ DATA
merged <- read_csv("./data/merged_data.csv")

# Check assumptions
ggplot(data = merged) +
        aes(SCR) +
        geom_histogram()

ggplot(data = merged) +
        aes(sample = SCR) +
        geom_qq()
        
# Make necessary transformations on data

merged <-
        merged %>% 
        filter(!TestId %in% c("01", "05", "18", "31")) %>% # Removing excluded participants
        group_by(TestId) %>% 
        mutate(SCR = SCR %>% sqrt %>% scale %>% as.numeric()) %>% 
        ungroup()  %>% 
        mutate(picture_set = if_else(session == "emot" & is.na(picture_set), "Original", picture_set))

# Check if it is better (yes)
ggplot(data = merged) +
        aes(SCR) +
        geom_histogram()

## Build lmer with a top-down method
# 1. Disregard random structure
nm.lm <- lm(SCR ~ group * block * category, data = merged)
summary(nm.lm)
AIC(nm.lm)

# 2. Use full model with full random structure (random intercept and slope)
nm.lmer.is <- lmer(SCR ~ group * block * category + (block|TestId), data = merged, REML = F)
nm.lmer.i <- lmer(SCR ~ group * block * category + (1|TestId), data = merged, REML = F)

AIC(nm.lmer.is) # Best AIC, but only by a small margin
AIC(nm.lmer.i)


anova(nm.lmer.is, nm.lmer.i) # IS is indeed a better model
summary(nm.lmer.is)

# So we go with a random intercept and slope model

# 3. Removing fixed terms from the model
nm.model <- nm.lmer.i # This is the structure we go forward

# 3-way interaction
nm.3I <- update(nm.model, . ~ . - group:block:category) # Remove 3way interaction
anova(nm.model, nm.3I) # Does not add to the model

nm.model <- nm.3I
# 2-way interaction
nm.2I.1 <-  update(nm.model, . ~ . - group:block)
nm.2I.2 <-  update(nm.model, . ~ . - group:category)
nm.2I.3 <-  update(nm.model, . ~ . - block:category)

anova(nm.model, nm.2I.1)

# group:block does not improve the model, so remove
nm.model <- nm.2I.1

# Get rid of unimportant main effects
nm.M.1 <-  update(nm.model, . ~ . - group)
nm.M.2 <-  update(nm.model, . ~ . - block)
nm.M.3 <-  update(nm.model, . ~ . - category)

anova(nm.model, nm.M.1) # No main effect should be removed

# So the final model is:
summary(nm.model)

merged %>% 
        filter(session == "emot") %>% 
        drop_na(block) %>% 
        group_by(block, category, group) %>% 
        summarise(Mean = mean(SCR, na.rm = T),
                  Sd = sd(SCR, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
                aes(x = block, y = Mean, ymin = Mean-Se, ymax = Mean + Se) +
                geom_errorbar(width = .2) +
                geom_col(position = "dodge") +
                facet_grid(group~category, scales = "free_x") +
                geom_smooth(method = lm, se = FALSE)


# TODO: Manipulation check for negative and neutral pictures, using the subjective arousal

merged %>% 
        filter(session == "emot") %>% 
        drop_na(category) %>% 
        group_by(category, group) %>% 
        summarise(Mean = mean(SCR, na.rm = T),
                  Sd = sd(SCR, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
                aes(x = category, y = Mean, ymin = Mean-Se, ymax = Mean + Se) +
                geom_errorbar(width = .2) +
                geom_col(position = "dodge") +
                facet_grid(.~group, scales = "free_x")

### CHECKING RECALL DATA
## Build lmer with a top-down method
# 1. Disregard random structure 
morning.lm <- lm(SCR ~ group * category * session + picture_set*group*category, data = merged)
morning.i <- lmer(SCR ~ group * category * session + picture_set*group*category + (1|TestId), data = merged)
morning.is <- lmer(SCR ~ group * category * session + picture_set*group*category + (session|TestId), data = merged) # We don't have 4-way interaction in the model

# The best model is the random intercept, random slope model
AIC(morning.lm)
anova(morning.i, morning.is)
morning.model <- morning.is
summary(morning.model)

# Checking 3-way interactions
morning.3I.1 <- update(morning.model, . ~ . - group:session:category) # Drop
morning.3I.2 <- update(morning.model, . ~ . - group:picture_set:category) # Drop

anova(morning.model, morning.3I.1, morning.3I.2)
morning.model <- update(morning.model, . ~ . -group:session:category -group:picture_set:category ) 

# Checking 2-way interactions
morning.2I.1 <- update(morning.model, . ~ . - group:session) # Drop
morning.2I.2 <- update(morning.model, . ~ . - picture_set:category) # Drop
morning.2I.3 <- update(morning.model, . ~ . - group:category) # Do not drop
morning.2I.4 <- update(morning.model, . ~ . - group:picture_set) # Drop
morning.2I.5 <- update(morning.model, . ~ . - session:category) # Drop

morning.model <- update(morning.model, . ~ . - group:session - group:picture_set - picture_set:category - session:category) 
morning.model %>% summary

# Remove main effects
morning.M.1 <- update(morning.model, . ~ . - group) 
morning.M.2 <- update(morning.model, . ~ . - session) # Dop
morning.M.3 <- update(morning.model, . ~ . - picture_set) # Drop
morning.M.4 <- update(morning.model, . ~ . - category) 

anova(morning.model, morning.M.1, morning.M.2, morning.M.3, morning.M.4)

morning.model <- update(morning.model, . ~ . - session - picture_set) 

summary(morning.model)


merged %>% 
        drop_na(picture_set, category) %>% 
        mutate(picture_set = picture_set %>% factor(levels = c("Original", "New")),
               ) %>% 
        group_by(picture_set, session, category, group) %>% 
        summarise(Mean = mean(SCR, na.rm = T),
                  Sd = sd(SCR, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
        aes(x = session, y = Mean, ymin = Mean-Se, ymax = Mean + Se, fill = picture_set, group = picture_set) +
        geom_errorbar(position = position_dodge()) +
        geom_col(position = position_dodge()) +
        facet_grid(group ~ category)
##
merged %>% 
        drop_na(picture_set, category) %>% 
        mutate(picture_set = picture_set %>% factor(levels = c("Original", "New")),
        ) %>% 
        group_by(picture_set, session, category, group) %>% 
        summarise(Mean = mean(SCR, na.rm = T),
                  Sd = sd(SCR, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
        aes(x = group, y = Mean, ymin = Mean-Se, ymax = Mean + Se, fill = picture_set, group = picture_set) +
        geom_errorbar(position = position_dodge()) +
        geom_col(position = position_dodge()) +
        facet_grid(session ~ category)

###
merged %>% 
        drop_na(picture_set, category) %>% 
        group_by(category, group) %>% 
        summarise(Mean = mean(SCR, na.rm = T),
                  Sd = sd(SCR, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
        aes(x = category, y = Mean, ymin = Mean-Se, ymax = Mean + Se, fill = group) +
        geom_errorbar(position = position_dodge()) +
        geom_col(position = position_dodge())


