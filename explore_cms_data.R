# explore_cms_data.R
# Exploratory analysis for the CMS patient-reported outcomes dataset.

# Load libraries --------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  stop("Please install the tidyverse package before running this script: 
  install.packages('tidyverse')")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  stop("Please install the lubridate package before running this script: 
  install.packages('lubridate')")
}

library(tidyverse)
library(lubridate)

# Path to the dataset --------------------------------------------------------
data_path <- "PATIENT_REPORTED_OUTCOMES_FACILITY(1).csv"
output_dir <- "exploration_outputs"

if (!file.exists(data_path)) {
  stop("Dataset not found at: ", data_path)
}

# Create output directory for tables and plots -------------------------------
dir.create(output_dir, showWarnings = FALSE)

# Read the dataset -----------------------------------------------------------
raw_data <- read_csv(
  data_path,
  col_types = cols(
    `Facility ID` = col_character(),
    `Facility Name` = col_character(),
    Address = col_character(),
    `City/Town` = col_character(),
    State = col_character(),
    `ZIP Code` = col_character(),
    `County/Parish` = col_character(),
    `Telephone Number` = col_character(),
    `Measure ID` = col_character(),
    `Measure Name` = col_character(),
    Voluntary_Reporting = col_character(),
    Score = col_character(),
    Footnote = col_character(),
    `Start Date` = col_character(),
    `End Date` = col_character()
  ),
  guess_max = 2000
)

# Data cleaning ----------------------------------------------------------------
clean_data <- raw_data |>
  mutate(    Score = na_if(Score, "Not Available"),    Score = na_if(Score, ""),
    ScoreNumeric = parse_number(Score),
    StartDate = mdy(`Start Date`),
    EndDate = mdy(`End Date`),
    Facility = `Facility Name`
  )

# Exploration outputs --------------------------------------------------------

# 1. Data shape and unique counts
summary_counts <- tibble(
  rows = nrow(clean_data),
  unique_facilities = n_distinct(clean_data$`Facility ID`),
  unique_states = n_distinct(clean_data$State),
  unique_counties = n_distinct(clean_data$`County/Parish`),
  unique_measures = n_distinct(clean_data$`Measure ID`)
)

print(summary_counts)
write_csv(summary_counts, file.path(output_dir, "summary_counts.csv"))

# 2. Column overview and missing values
missing_summary <- clean_data |>
  summarize(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") |>
  arrange(desc(missing_count))

print(missing_summary)
write_csv(missing_summary, file.path(output_dir, "missing_summary.csv"))

# 3. Score statistics
score_summary <- clean_data |>
  summarize(
    available_scores = sum(!is.na(ScoreNumeric)),
    missing_scores = sum(is.na(ScoreNumeric)),
    min_score = min(ScoreNumeric, na.rm = TRUE),
    median_score = median(ScoreNumeric, na.rm = TRUE),
    mean_score = mean(ScoreNumeric, na.rm = TRUE),
    max_score = max(ScoreNumeric, na.rm = TRUE)
  )

print(score_summary)
write_csv(score_summary, file.path(output_dir, "score_summary.csv"))

# 4. Top states by facility count
state_counts <- clean_data |>
  count(State, sort = TRUE) |>
  rename(facility_count = n)

print(state_counts)
write_csv(state_counts, file.path(output_dir, "state_counts.csv"))

# 5. Top counties by facility count
county_counts <- clean_data |>
  count(`County/Parish`, State, sort = TRUE) |>
  rename(facility_count = n)

print(county_counts |> slice_head(n = 20))
write_csv(county_counts, file.path(output_dir, "county_counts.csv"))

# 6. Score distribution by state (only states with at least 5 observations)
score_by_state <- clean_data |>
  filter(!is.na(ScoreNumeric)) |>
  group_by(State) |>
  summarize(
    observations = n(),
    avg_score = mean(ScoreNumeric, na.rm = TRUE),
    median_score = median(ScoreNumeric, na.rm = TRUE),
    sd_score = sd(ScoreNumeric, na.rm = TRUE)
  ) |>
  filter(observations >= 5) |>
  arrange(desc(avg_score))

print(score_by_state)
write_csv(score_by_state, file.path(output_dir, "score_by_state.csv"))

# 7. Score distribution by facility
top_facilities_by_score <- clean_data |>
  filter(!is.na(ScoreNumeric)) |>
  arrange(desc(ScoreNumeric)) |>
  select(`Facility ID`, `Facility Name`, State, `County/Parish`, ScoreNumeric, Voluntary_Reporting) |>
  slice_head(n = 20)

print(top_facilities_by_score)
write_csv(top_facilities_by_score, file.path(output_dir, "top_facilities_by_score.csv"))

# Plotting -------------------------------------------------------------------

# Histogram of numeric scores
clean_data |>
  filter(!is.na(ScoreNumeric)) |>
  ggplot(aes(x = ScoreNumeric)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Available CMS Scores",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal() -> score_hist

ggsave(file.path(output_dir, "score_histogram.png"), score_hist, width = 8, height = 5)

# Bar chart: facility count by state (top 15)
state_counts |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(State, facility_count), y = facility_count)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 15 States by Facility Count",
    x = "State",
    y = "Facility Count"
  ) +
  theme_minimal() -> state_bar

ggsave(file.path(output_dir, "top_states_by_facility_count.png"), state_bar, width = 8, height = 6)

# Boxplot of score by state for states with sufficient scores
clean_data |>
  filter(!is.na(ScoreNumeric)) |>
  group_by(State) |>
  filter(n() >= 5) |>
  ungroup() |>
  ggplot(aes(x = reorder(State, ScoreNumeric, FUN = median), y = ScoreNumeric)) +
  geom_boxplot(fill = "orange", alpha = 0.75) +
  coord_flip() +
  labs(
    title = "CMS Score Distribution by State (states with >= 5 scores)",
    x = "State",
    y = "Score"
  ) +
  theme_minimal() -> state_boxplot

ggsave(file.path(output_dir, "score_distribution_by_state.png"), state_boxplot, width = 10, height = 8)

# Final notes ----------------------------------------------------------------
cat("Exploration complete. Results and plots are saved in:", normalizePath(output_dir), "\n")
