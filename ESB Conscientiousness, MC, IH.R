#Phase 1 (Rapp lab getting SPI)

library(readxl)
library(dplyr)

# Load data
Data <- read_excel(
  "G:\\My Drive\\Emotion Regulation and Conspiracy Thinking\\Data\\ESB Data Phase 1.xlsx"
)

# Remove first row if needed
Data <- Data[-1, ]

#Getting SPI_total

# Define SPI-135 column names
spi_columns <- c(
  "SPI.135_1_1", "SPI.135_2_1", "SPI.135_3_1", "SPI.135_4_1",
  "SPI.135_5_1", "SPI.135_6_1", "SPI.135_7_1", "SPI.135_8_1",
  "SPI.135_9_1", "SPI.135_10_1", "SPI.135_11_1", "SPI.135_12_1",
  "SPI.135_13_1"
)

# Define Likert accuracy levels (must match your data's text exactly)
accuracy_levels <- c(
  "Very Inaccurate",
  "Moderately Inaccurate",
  "Slightly Inaccurate",
  "Slightly Accurate",
  "Moderately Accurate",
  "Very Accurate"
)

# Extract only the SPI columns into a new dataframe
data_SPI <- Data %>%
  select(all_of(spi_columns)) %>%
  mutate(across(everything(),
                ~ as.numeric(factor(trimws(.), levels = accuracy_levels)))) %>%
  mutate(
    SPI.135_12_1 = 7 - SPI.135_12_1,  # reverse-code item 12
    SPI_total = rowSums(across(all_of(spi_columns)), na.rm = TRUE)
  )

#######################################################################################
#Phase 2 (Rapp lab getting Misplaced Certainty, Intellectual humility, and Auction Task)
# Load data
DataT2 <- read_excel(
  "G:\\My Drive\\Emotion Regulation and Conspiracy Thinking\\Data\\ESB Data Phase 2.xlsx"
)

DataT2 <- DataT2[-1, ]

# Grabbing Misplaced Certainty, Intellectual humility
mc_vars <- c("MC_1_1", "MC_2_1")
ih_vars <- c("IH_1_1", "IH_2_1", "IH_3_1", "IH_4_1", "IH_5_1", "IH_6_1")

# --- Define MC recode mapping ---
mc_map <- c(
  "Strongly disagree" = 1,
  "Disagree" = 2,
  "Somewhat disagree" = 3,
  "Neither agree nor disagree" = 4,
  "Somewhat agree" = 5,
  "Agree" = 6,
  "Strongly agree" = 7
)

DataT2 <- DataT2 %>%
  mutate(across(all_of(mc_vars), ~ recode(.x, !!!mc_map))) %>%
  mutate(across(all_of(mc_vars), as.numeric))

# --- Clean IH variables (keep only 1–5) ---
DataT2 <- DataT2 %>%
  mutate(across(all_of(ih_vars), ~ parse_number(as.character(.x))))

# --- Create separate data frames ---
data_MC <- DataT2 %>%
  select(all_of(mc_vars)) %>%
  mutate(Sum_MC = rowSums(across(everything()), na.rm = TRUE))

data_IH <- DataT2 %>%
  select(all_of(ih_vars)) %>%
  mutate(Sum_IH = rowSums(across(everything()), na.rm = TRUE))

