
# install.packages(tidyverse)
# install.packages(readxl)
# install.packages(psych)
# install.packages(gt)
# install.packages(janitor)

library(tidyverse)
library(readxl)
library(psych)
library(gt)
library(janitor)

# Load data
data <- read_excel("./Attachment_Anxiety_Data.xlsx")

# Inspect data
head(data)

# Rename all variables to lower/snake case 
data.rename <- janitor::clean_names(data)

data.clean <- data.rename |>
  
  # Use mutate to rename variables based on README file associated with 'Attachment and social anxiety' data set
  mutate(gender = case_when(gender==1 ~ "Male", 
                            gender==2 ~"Female",
                            TRUE ~ "NA")) |> 
  
  mutate(age_group = case_when(age_group==1 ~ "18-24", 
                               age_group==2 ~ "24-34",
                               age_group==3 ~ "35-44", 
                               age_group==4 ~ "45-54", 
                               age_group==5 ~ "55-64", 
                               TRUE ~ "NA")) |> 

  # Note: README file lists 2 and 5 as both "In a Relationship" which was retained here
  mutate(relationship = case_when(relationship==1 ~ "Married",
                                  relationship==2 ~ "In a Relationship",
                                  relationship==4 ~ "Divorced",
                                  relationship==5 ~ "In a Relationship",
                                  relationship==6 ~ "Never Been in a Relationship",
                                  relationship==7 ~ "Prefer Not to Say",
                                  TRUE ~ "NA")) |>
  
  mutate(ethnicity = case_when(ethnicity==1 ~ "White/Caucasian",
                               ethnicity==2 ~ "Asian/Asian British",
                               ethnicity==5 ~ "Other Ethnic Group",
                               ethnicity==6 ~ "Prefer Not to Say",
                               TRUE ~ "NA")) |>
  
  # Select all variables except URN and consent
  select(gender:s_est_10) |>
  
  # Mutate to convert survey response variables to integers
  mutate_at(5:43, as.integer)

# Answer key: which items belong to each scale
# "-" marks reverse-coded items 

# sias = Social Interaction Anxiety Scale
sias_key <- list(sias=c("sa_1", 
                        "sa_2", 
                        "sa_3",
                        "sa_4",
                        "-sa_5",
                        "sa_6",
                        "sa_7",
                        "sa_8",
                        "-sa_9",
                        "sa_10",
                        "-sa_11",
                        "sa_12",
                        "sa_13",
                        "sa_14",
                        "sa_15",
                        "sa_16",
                        "sa_17",
                        "sa_18",
                        "sa_19",
                        "sa_20"))

# ecras = Experiences in Close Relationships Anxiety Subscale
ecras_key <- list(ecras=c("aa_1",
                          "aa_2",
                          "aa_3",
                          "aa_4",
                          "aa_5",
                          "aa_6",
                          "aa_7",
                          "aa_8",
                          "aa_9"))

# rses = Rosenberg Self-Esteem Scale
rses_key <- list(rses=c("s_est_1",
                        "-s_est_2",
                        "s_est_3",
                        "s_est_4",
                        "-s_est_5",
                        "-s_est_6",
                        "s_est_7",
                        "-s_est_8",
                        "-s_est_9",
                        "s_est_10"))
            
# Returns average, Chronbach's alpha, and reliability score
# Turns each into it's own data frame

sias_scores <-scoreItems(sias_key, data.clean, totals = F, min=0, max=4)
sias_scores.df <-as.data.frame(sias_scores$scores)

ecras_scores <-scoreItems(ecras_key, data.clean, totals = F, min=1, max=7)
ecras_scores.df <-as.data.frame(ecras_scores$scores)

rses_scores <-scoreItems(rses_key, data.clean, totals = F, min=1, max=4)
rses_scores.df <-as.data.frame(rses_scores$scores)

# Combine scores into one data frame
response_scores.df <- cbind(sias_scores.df, ecras_scores.df, rses_scores.df)

# Combine with clean data frame 
data.clean <- cbind(data.clean, response_scores.df)

# Inspect data frame
head(data.clean)

data.final <- data.clean |>
  select("gender", "age_group", "relationship", "ethnicity", "sias", "ecras", "rses") |>
  mutate_if(is.numeric, round, 2)

# Create a demographics table of Relationship status by age group
demographic.data <- data.final |>
  tabyl(relationship, age_group)

# Create your reproducible gt table
demographic.table <- demographic.data |>
  gt() |>
  tab_header(
    title = md("*Distribution of Relationship Status by Age Group*")
  )|>
  tab_spanner(
    label="Age Group",
    columns = c("18-24", "24-34",	"35-44",	"45-54", "55-64")
  )|> 
  cols_label(
    relationship = "Relationship Status",
  )

# Summary statistics for response scales
summary <- describe(select(data.final, sias:rses))
summary

# Collect the alpha values from each Scores list
alphas <- c(sias_scores$alpha,
                    ecras_scores$alpha, 
                    rses_scores$alpha) 
alphas <- round(alphas, 2)
             
# Sample size for document
n <- (describe(data.final))[3, 2]

# Naming the scales for the table and creating a column
scales <- c("Social Interaction Anxiety", 
            "Attachment-Related Anxiety", 
            "Rosenberg Self-Esteem")
summary$scales<- scales

# Select the columns for the table
summary <- summary |>
  select(scales, n, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)

# Combine summary and alpha
table.data <- cbind(summary, alphas)

# Make a beautiful table
response.table <- gt(table.data) |>
  tab_header(
    title = md("*Descriptive Statistics for Attachment Anxiety Responses*")) |>
  tab_stubhead(label= "Composites")|>
  tab_spanner(
    label="Central Tendency",
    columns = c(mean, median)
  )|>
  tab_spanner(
    label="Variability",
    columns = c(sd, range)
  )|>
  tab_spanner(
    label="Reliability",
    columns = c(alphas)
  )|> 
  cols_label(
    scales = "Response Scales",
    n = "N",
    mean = "Mean", 
    sd = "SD", 
    range = "Range", 
    alphas = "Chronbach's α"
  )

# Use filter to extract only those participants who are over 18 and under 65

data.filtered <- data.final |>
  filter(age_group=="24-34")

# Checking that age restriction worked
table(data.filtered$age_group) 

# Collect the alpha values from each Scores list
alphas_filtered <- c(sias_scores$alpha,
                     ecras_scores$alpha, 
                     rses_scores$alpha) 
alphas_filtered <- round(alphas_filtered, 2)

# Sample size for document
n_filtered <- (describe(data.filtered))[3, 2]

# Select the columns for the table
summary_filtered <- describe(select(data.filtered, sias:rses))
summary_filtered

# Naming the scales for the table and creating a column
scales <- c("Social Interaction Anxiety", 
            "Attachment-related Anxiety", 
            "Rosenberg Self-Esteem")
summary_filtered$scales<- scales

# Select the columns for the table
summary_filtered <- summary_filtered |>
  select(scales, n, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)

# Combine summary and alpha
table.data_filtered <- cbind(summary_filtered, alphas_filtered)

# Make a beautiful table
response.table_filtered <- gt(table.data_filtered) |>
  tab_header(
    title = md("*Descriptive Statistics for Attachment Anxiety Responses for Ages 24 to 34*")) |>
  tab_stubhead(label= "Composites")|>
  tab_spanner(
    label="Central Tendency",
    columns = c(mean, median)
  )|>
  tab_spanner(
    label="Variability",
    columns = c(sd, range)
  )|>
  tab_spanner(
    label="Reliability",
    columns = c(alphas_filtered)
  )|> 
  cols_label(
    scales = "Response Scales",
    n = "N",
    mean = "Mean", 
    sd = "SD", 
    range = "Range", 
    alphas_filtered = "Chronbach's α"
  )
