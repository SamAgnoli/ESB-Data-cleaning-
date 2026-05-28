# ---- Load Packages ----
if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if(!require(janitor)) {install.packages("janitor")}; library(janitor)
if(!require(here)) {install.packages("here")}; library(here)
if(!require(skimr)) {install.packages("skimr")}; library(skimr)
if(!require(readxl)) {install.packages("readxl")}; library(readxl)
if(!require(psych)) {install.packages("psych")}; library(psych)


# ---- Import Data -----
ESB_Data_Phase_2 <- read_excel("ESB Data Phase 2.xlsx") %>%
  slice(-1)

# ---- Inspect Original Data ----
# glimpse(ESB_Data_Phase_2)
# names(ESB_Data_Phase_2)

# ---- Clean Column Names ----
clean1 <- ESB_Data_Phase_2 %>%
  clean_names()                               # standardize column names
  

# ---- cat-pd ----
clean2 <- clean1 %>% 
  mutate(

    across(
      starts_with("cat"),
      ~ as.numeric(recode(.,
                          "1 = Very untrue of me" = 1,
                          "2 = Moderately untrue of me" = 2,
                          "3 = Neither true nor untrue of me" = 3,
                          "4 = Moderately true of me" = 4,
                          "5 = Very true of me" = 5
      ))
    ),
    cat_rigid = rowMeans(across(starts_with("cat")), na.rm = FALSE)
  )
# quality test
# clean2 %>% select(starts_with("cat_"),-cat_rigid) -> test2
# omega(test1)


#idas mania----

clean3 <- clean2 %>%
  mutate(
    across(
      starts_with("idas"),
      ~ recode(.,
               "1 = not at all" = 1,
               "2 = a little bit" = 2,
               "3 = moderately" = 3,
               "4 = quite a bit" = 4,
               "5 = extremely" = 5
      )
    )
  ) %>%
  mutate(across(starts_with("idas"), as.numeric),
         idas_mania = rowSums(select(., starts_with("idas")), na.rm = FALSE)) 
# quality test
# clean3 %>% select(starts_with("idas_"),-idas_mania) -> test3
# omega(test3)



#---- bfas----
###Define intellect and openness columns
intellect_cols <- c(
  "bfas_o_1_1",
  "bfas_o_2_1",
  "bfas_o_3_1",
  "bfas_o_4_1",
  "bfas_o_5_1",
  "bfas_o_6_1",
  "rbfas_o_7_1",
  "rbfas_o_8_1",
  "rbfas_o_9_1",
  "rbfas_o_10_1"
)

openness_cols <- c(
  "bfas_o_11_1",
  "bfas_o_12_1",
  "bfas_o_13_1",
  "bfas_o_14_1",
  "bfas_o_15_1",
  "bfas_o_16_1",
  "rbfas_o_17_1",
  "rbfas_o_18_1",
  "rbfas_o_19_1",
  "rbfas_o_20_1"
)


clean4 <- clean3 %>%
  mutate(
    # recode
    across(
      starts_with("bfas"),
      ~ recode(.,
               "1 = Very inaccurate" = 1,
               "2 = Moderately inaccurate" = 2,
               "3 = Neither accurate nor inaccurate" = 3,
               "4 = Moderately accurate" = 4,
               "5 = Very accurate" = 5
      )
    ),
    # reverse keying select items
          rbfas_o_7_1=6-bfas_o_7_1,
         rbfas_o_8_1=6-bfas_o_8_1,
         rbfas_o_9_1=6-bfas_o_9_1,
         rbfas_o_10_1=6-bfas_o_10_1,
         rbfas_o_17_1=6-bfas_o_17_1,
         rbfas_o_18_1=6-bfas_o_18_1,
         rbfas_o_19_1=6-bfas_o_19_1,
         rbfas_o_20_1=6-bfas_o_20_1) %>% 
    # score scales
    mutate(bfas_intellect = rowSums(select(., all_of(intellect_cols)), na.rm = FALSE),
           bfas_openness  = rowSums(select(., all_of(openness_cols)), na.rm = FALSE),
          bfas_ote       = bfas_intellect + bfas_openness)

# quality test
# clean4 %>% select(all_of(intellect_cols)) -> test4a
# omega(test4a)  
# clean4 %>% select(all_of(openness_cols)) -> test4b
# omega(test4b)  
# 
# clean4 %>% select(contains("bfas_o"),-starts_with("rbfas")) -> test4
# omega(test4) 
# alpha(test4)



# ---- Mini-IPIP big 5----
reverse_miniipip_items <- c(
  "rbfmipipm_3_1",
  "rbfmipipm_4_1",
  "rbfmipipm_7_1",
  "rbfmipipm_8_1",
  "rbfmipipm_11_1",
  "rbfmipipm_12_1",
  "rbfmipipm_15_1",
  "rbfmipipm_16_1"
)


extraversion_cols <- c(
  "bfmipipm_1_1",
  "bfmipipm_2_1",
  "rbfmipipm_3_1",
  "rbfmipipm_4_1"
)

agreeableness_cols <- c(
  "bfmipipm_5_1",
  "bfmipipm_6_1",
  "rbfmipipm_7_1",
  "rbfmipipm_8_1"
)

conscientiousness_cols <- c(
  "bfmipipm_9_1",
  "bfmipipm_10_1",
  "rbfmipipm_11_1",
  "rbfmipipm_12_1"
)

neuroticism_cols <- c(
  "bfmipipm_13_1",
  "bfmipipm_14_1",
  "rbfmipipm_15_1",
  "rbfmipipm_16_1"
)

clean5 <- clean4 %>%
  mutate(
    across(
      starts_with("bfmipipm"),
      ~ recode(.,
               "1 = Very Inaccurate" = 1,
               "2 = Moderately Inaccurate" = 2,
               "3 = Neither Accurate Nor Inaccurate" = 3,
               "4 = Moderately Accurate" = 4,
               "5 = Very Accurate" = 5
      )
    ),
    rbfmipipm_3_1=6-bfmipipm_3_1,
    rbfmipipm_4_1=6-bfmipipm_4_1,
    rbfmipipm_7_1=6-bfmipipm_7_1,
    rbfmipipm_8_1=6-bfmipipm_8_1,
    rbfmipipm_11_1=6-bfmipipm_11_1,
    rbfmipipm_12_1=6-bfmipipm_12_1,
    rbfmipipm_15_1=6-bfmipipm_15_1,
    rbfmipipm_16_1=6-bfmipipm_16_1) %>% 
    
    
    
    mutate(bfminipm_extraversion = rowSums(select(., all_of(extraversion_cols)), na.rm = FALSE),
           bfminipm_agreeableness = rowSums(select(., all_of(agreeableness_cols)), na.rm = FALSE),
           bfminipm_conscientiousness = rowSums(select(., all_of(conscientiousness_cols)), na.rm = FALSE),
           bfminipm_neuroticism = rowSums(select(., all_of(neuroticism_cols)), na.rm = FALSE)
  ) 

# clean5 %>% select(all_of(extraversion_cols)) -> test5a
# omega(test5a)  
# clean5 %>% select(all_of(agreeableness_cols)) -> test5b
# omega(test5b)  
# clean5 %>% select(all_of(conscientiousness_cols)) -> test5c
# omega(test5c) 
# clean5 %>% select(all_of(neuroticism_cols)) -> test5d
# omega(test5d) 

# #rbfmipipm_16_1 not loading? (neuroticism)
# clean5 %>% select(bfmipipm_15_1,bfmipipm_16_1,bfmipipm_14_1,bfmipipm_13_1) -> test5dx
# lowerCor(test5dx)


#----- CAPE -------

clean6 <- clean5 %>%
  mutate(
    across(
      .cols = names(.)[startsWith(names(.), "cape") & !str_detect(names(.), "b")],
      ~ recode(.,
               "Never" = 1,
               "Sometimes" = 2,
               "Often" = 3,
               "Nearly always" = 4,
               "never" = 1,
               "sometimes" = 2,
               "often" = 3,
               "nearly always" = 4
      )
    )
  ) %>%
  mutate(
    across(
      .cols = names(.)[startsWith(names(.), "cape") & !str_detect(names(.), "b")],
      as.numeric
    )
  )

##### CAPE distress; Non answer now equals 0

clean7 <- clean6 %>%
  mutate(
    across(
      .cols = names(.)[startsWith(names(.), "cape") & str_detect(names(.), "b")],
      ~ as.numeric(
        coalesce(
          recode(.,
                 "Not distressed" = 1,
                 "A bit distressed" = 2,
                 "Quite distressed" = 3,
                 "Very distressed" = 4,
                 "not distressed" = 1,
                 "a bit distressed" = 2,
                 "quite distressed" = 3,
                 "very distressed" = 4,
                 .default = 0
          ),
          0
        )
      )
    )
  )


cape_neg_cols <- c("cape_16", "cape_17", "cape_18", "cape_19", "cape_20",
                   "cape_21", "cape_22", "cape_23", "cape_24", "cape_25",
                   "cape_26", "cape_27")

cape_neg_d_cols <- c("cape_16b", "cape_17b", "cape_18b", "cape_19b", "cape_20b",
                     "cape_21b", "cape_22b", "cape_23b", "cape_24b", "cape_25b",
                     "cape_26b", "cape_27b")
cape_pos_cols <- c(
  "cape_1", "cape_2", "cape_3", "cape_4", "cape_5",
  "cape_6", "cape_7", "cape_8", "cape_9", "cape_10",
  "cape_11", "cape_12", "cape_13", "cape_14", "cape_15"
)

cape_pos_d_cols <- c(
  "cape_5b", "cape_7b", "cape_8b", "cape_9b", "cape_10b",
  "cape_11b", "cape_14b", "cape_12b", "cape_13b", "cape_15b",
  "cape_1b", "cape_2b", "cape_3b", "cape_4b", "cape_6b"
)  

be_cols <- c("cape_5", "cape_7", "cape_8", "cape_9", "cape_10", "cape_11", "cape_14")

pa_cols <- c("cape_12", "cape_13", "cape_15")

pi_cols <- c("cape_1", "cape_2", "cape_3", "cape_4", "cape_6")

be_cols_d <- c("cape_5b", "cape_7b", "cape_8b", "cape_9b",
               "cape_10b", "cape_11b", "cape_14b")

pa_cols_d <- c("cape_12b", "cape_13b", "cape_15b")

pi_cols_d <- c("cape_1b", "cape_2b", "cape_3b",
               "cape_4b", "cape_6b")

clean8 <- clean7 %>%
  mutate(
    cape_neg = rowSums(select(., all_of(cape_neg_cols)), na.rm = FALSE),
    cape_neg_d = rowSums(select(., all_of(cape_neg_d_cols)), na.rm = TRUE),
    cape_pos = rowSums(select(., all_of(cape_pos_cols)), na.rm = FALSE),
    cape_pos_d = rowSums(select(., all_of(cape_pos_d_cols)), na.rm = TRUE),
    cape_be = rowSums(select(., all_of(be_cols)), na.rm = FALSE),
    cape_pa = rowSums(select(., all_of(pa_cols)), na.rm = FALSE),
    cape_pi = rowSums(select(., all_of(pi_cols)), na.rm = FALSE),
    cape_be_d = rowSums(select(., all_of(be_cols)), na.rm = FALSE),
    cape_pa_d = rowSums(select(., all_of(pa_cols)), na.rm = FALSE),
    cape_pi_d = rowSums(select(., all_of(pi_cols)), na.rm = FALSE)
  )

# clean7 %>% select(all_of(cape_neg_cols)) -> test7a
# omega(test7a)  
# clean7 %>% select(all_of(cape_neg_d_cols)) -> test7b
# omega(test7b)  
# clean7 %>% select(all_of(cape_pos_cols)) -> test7c
# omega(test7c)  
# clean7 %>% select(all_of(cape_pos_d_cols)) -> test7d
# omega(test7d)  
# clean7 %>% select(all_of(be_cols)) -> test7e
# omega(test7e)  
# clean7 %>% select(all_of(pa_cols)) -> test7f
# omega(test7f)
# clean7 %>% select(all_of(pi_cols)) -> test7g
# omega(test7g)


#pos distress weird? perceptual abnormalities
# corr.test(test7d) # low Ns in cells for distress items, may want to recode as 0s 
#                   # people who were not asked a distress item due to not experiencing 
#                   # that specific PLE
# corr.test(test7f) # too few items for omega


# ---- Select Variables of Interest ----
clean9 <- clean8 %>%
  select(
    starts_with("cape"),
    starts_with("cat"),
    starts_with("idas"),
    starts_with("bfas"),
    starts_with("bfmipipm"),
    starts_with("atq"),
    cat_rigid,
    idas_mania,
    bfas_intellect,
    bfas_openness,
    bfminipm_extraversion,
    bfminipm_agreeableness,
    bfminipm_conscientiousness,
    bfminipm_neuroticism, 
    cape_neg,
    cape_neg_d,
    cape_pos,
    cape_pos_d,
    cape_be,
    cape_pa,
    cape_pi,
    progress,
    duration,
    prolific_pid
  )


# ---- View Cleaned Dataset ----
View(clean9)

clean9 <- clean9 %>%
  rename(prolific_final = prolific_pid)

# create final data set and clean environment -------
phase2_questionnaire_data <- clean9
rm(list = setdiff(ls(), "phase2_questionnaire_data"))

write.csv(phase2_questionnaire_data, "phase2_questionnaire_data.csv", row.names = FALSE)








