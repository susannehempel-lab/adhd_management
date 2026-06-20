# Produce stratified plot (Functional - medication)

library(dplyr)
library(here)
library(quarto)
library(glue)

library(broom)
library(dplyr)
library(glue)
library(htmltools)
library(knitr)
library(kableExtra)
library(metafor)
library(stringr)


# This tells the metareg functions whether to do comparator vs int or control
# vs int (the default) TRUE vs FALSE
# Probably should be false
comparator_vs_int <- FALSE

git_raw_root <- "https://raw.githubusercontent.com/susannehempel-lab/adhd_management/refs/heads/main"
qmd_url  <- glue("{git_raw_root}/adhd_management.qmd")
d_location <- file.path(git_raw_root, "data/adhd_management_data.csv")

# 1. Download the d frame first
d <- read.csv(d_location)

d <- d %>% 
  mutate(ID = gsub("\\s*\\{[^}]*\\}", "", ID))

# no comp_atomoxetine so fill with blanks
d$comp_atomoxetine <- ""
d$comp_nonstimulant <- ""
d$comp_NS <- ""

#d <- d %>% dplyr::filter(study.design == "RCT")



## Functional:medication
# Separate analysis for 
# Atomoxetine

d <- d %>% dplyr::mutate(
  strata_1 = case_when(
    int_NS == "int_NS" ~ "Atomoxetine",
    int_AMPH == "int_AMPH" ~ "Amphetamine",
    int_MPH == "int_MPH" ~ "Methylphenidate"
  ))


d$comp_is_int <-
  # test if comp is the intervention (i.e. subgroup) of interest
  d[paste0("comp_atomoxetine")] == paste0("comp_atomoxetine")

d$functional_mean_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$functional_mean_comp_cont,
  # leave alone
  d$functional_mean_int_cont
)
# same for SD  
d$functional_SD_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$functional_SD_comp_cont,
  # leave alone
  d$functional_SD_int_cont
)
# same for N
d$functional_n_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$functional_n_comp_cont,
  # leave alone
  d$functional_n_int_cont
)    
if(length(d$comp_is_int) > 0 && any(d$comp_is_int, na.rm = TRUE)) {
  cat("<h3> Possible previous subgroup problem now fixed. </h3><p>")
}



d_es <- metafor::escalc(
  m1i = functional_mean_ctrl_cont,
  sd1i = functional_SD_ctrl_cont,
  n1i = functional_n_ctrl_cont,
  m2i = functional_mean_int_cont,
  sd2i = functional_SD_int_cont,
  n2i = functional_n_int_cont,
  measure = "SMD", 
  slab = d$ID, 
  data = d,
  append = TRUE
) %>% dplyr::filter(
  !is.na(yi) & !is.na(strata_1)
)

d_es$functional_positive_direction = "higher"

d_es <- d_es %>%
  dplyr::mutate(
    reverse = (functional_positive_direction == "higher" &
                 functional_value == "Lower is better") | 
      (functional_positive_direction == "lower" &
         functional_value == "Higher is better"),
    yi = ifelse(
      reverse,
      d_es$yi * -1, d_es$yi
    )
  ) %>% dplyr::arrange(int_label, ID)


d_amphetamine <- d_es %>% 
  dplyr::filter(strata_1 == "Amphetamine") 
fit_Amphetamine <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_amphetamine,
  slab = ID, test = "knha")

d_Atomoxetine <- d_es %>% 
  dplyr::filter(strata_1 == "Atomoxetine") 
fit_Atomoxetine <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_Atomoxetine,
  slab = ID, test = "knha")

d_Methylphenidate <- d_es %>% 
  dplyr::filter(strata_1 == "Methylphenidate") 
fit_Methylphenidate <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_Methylphenidate,
  slab = ID, test = "knha")
fit_all <- 
  metafor::rma(
    yi = yi,
    vi = vi,
    data = d_es,
    slab = ID, test = "knha")

rows_vector <- c(20:16, 13:9, 6:3)
metafor::forest(
  fit_all,
  #ilab.lab = "strata_1", # name of the column
  #ilab = strata_1,  # variable in the column
  rows = rows_vector,
  mlab = glue::glue(
  "Overall model: I-square {(round(fit_Methylphenidate$I2))} %"),
  xlab = "Standardized Mean Difference"
  )

addpoly(fit_Methylphenidate, row = 2, font = 2, 
        mlab = glue::glue(
          "Methylphenidate Model I-squared ", 
          (round(fit_Methylphenidate$I2)), "%"), col = "lightblue")

addpoly(fit_Amphetamine, row = 8, font = 2, 
        mlab = glue::glue(
          "Amphetamine Model I-squared ", 
          (round(fit_Amphetamine$I2)), "%"), col = "lightblue")

addpoly(fit_Atomoxetine, row = 15, font = 2, 
        mlab = glue::glue(
          "Atomoxetine Model I-squared ", 
          (round(fit_Atomoxetine$I2)), "%"), col = "lightblue")







############  Now blood pressure ##############################
 # delete all, so we don't mess up
rm(list = ls())

comparator_vs_int <- FALSE

git_raw_root <- "https://raw.githubusercontent.com/susannehempel-lab/adhd_management/refs/heads/main"
qmd_url  <- glue("{git_raw_root}/adhd_management.qmd")
d_location <- file.path(git_raw_root, "data/adhd_management_data.csv")

# 1. Download the d frame first
d <- read.csv(d_location)
d <- d %>% 
  mutate(ID = gsub("\\s*\\{[^}]*\\}", "", ID))



# Kay, 2009 has intervention Atomoxetine and Comparator Amphetamine, s 
# they need to be in the data again.

d_kay <- d %>% dplyr::filter(Refid == 2637)
# Move d_kay comp vars to int
d_kay <- d_kay %>% dplyr::mutate(
  int_AMPH = "int_AMPH",
  int_label = "S-APMH",
  int_NS = "",
  BP_mean_int_cont = BP_mean_comp_cont,
  BP_SD_int_cont = BP_SD_comp_cont,
  BP_n_int_cont = BP_n_comp_cont
)  
d <- dplyr::bind_rows(d, d_kay)  



# no comp_atomoxetine so fill with blanks
d$comp_atomoxetine <- ""
d$comp_nonstimulant <- ""
d$comp_NS <- ""

#d <- d %>% dplyr::filter(study.design == "RCT")



d <- d %>% dplyr::mutate(
  strata_1 = case_when(
    int_NS == "int_NS" ~ "Atomoxetine",
    int_AMPH == "int_AMPH" ~ "Amphetamine",
    int_MPH == "int_MPH" ~ "Methylphenidate"
  ))


d$comp_is_int <-
  # test if comp is the intervention (i.e. subgroup) of interest
  d[paste0("comp_atomoxetine")] == paste0("comp_atomoxetine")

d$BP_mean_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$BP_mean_comp_cont,
  # leave alone
  d$BP_mean_int_cont
)
# same for SD  
d$BP_SD_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$BP_SD_comp_cont,
  # leave alone
  d$BP_SD_int_cont
)
# same for N
d$BP_n_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$BP_n_comp_cont,
  # leave alone
  d$BP_n_int_cont
)    
if(length(d$comp_is_int) > 0 && any(d$comp_is_int, na.rm = TRUE)) {
  cat("<h3> Possible previous subgroup problem now fixed. </h3><p>")
}

d_es <- metafor::escalc(
  m1i = BP_mean_ctrl_cont,
  sd1i = BP_SD_ctrl_cont,
  n1i = BP_n_ctrl_cont,
  m2i = BP_mean_int_cont,
  sd2i = BP_SD_int_cont,
  n2i = BP_n_int_cont,
  measure = "SMD", 
  slab = d$ID, 
  data = d,
  append = TRUE
) %>% dplyr::filter(
  !is.na(yi) & !is.na(strata_1)
)

d_es$BP_positive_direction = "higher"

d_es <- d_es %>%
  dplyr::mutate(
    reverse = (BP_positive_direction == "higher" &
                 BP_value == "Lower is better") | 
      (BP_positive_direction == "lower" &
         BP_value == "Higher is better"),
    yi = ifelse(
      reverse,
      d_es$yi * -1, d_es$yi
    )
  ) %>% dplyr::arrange(int_label, ID)


d_amphetamine <- d_es %>% 
  dplyr::filter(strata_1 == "Amphetamine") 
fit_Amphetamine <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_amphetamine,
  slab = ID, test = "knha")

d_Atomoxetine <- d_es %>% 
  dplyr::filter(strata_1 == "Atomoxetine") 
fit_Atomoxetine <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_Atomoxetine,
  slab = ID, test = "knha")

d_Methylphenidate <- d_es %>% 
  dplyr::filter(strata_1 == "Methylphenidate") 
fit_Methylphenidate <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_Methylphenidate,
  slab = ID, test = "knha")

# We have Kay in the data twice, so use vcalc to account for the correlation
# in the standard errors, accounting for the shared control group. 
V_matrix <- vcalc(vi = vi, cluster = Refid, rho = 0.5, data = d_es)
V_matrix <- as.matrix(nearPD(V_matrix, keepDiag = TRUE)$mat)

fit_all <- 
  metafor::rma.mv(
    yi = yi,
    V = V_matrix,
    random = ~ 1 | Refid / yi, 
    data = d_es,
    slab = ID, test = "knha")


# calculate I2
wi <- 1 / fit_all$vi
v_bar <- (fit_all$k - 1) * sum(wi) / (sum(wi)^2 - sum(wi^2))

# 2. Extract the variance components from your rma.mv model
sigma2_between <- fit_all$sigma2[1] # Level 3 (Refid)
sigma2_within  <- fit_all$sigma2[2] # Level 2 (Within-study arms)

# 3. Calculate total variance
total_variance <- sigma2_between + sigma2_within + v_bar

# 4. Compute the partitioned I2 percentages
i2_between <- (sigma2_between / total_variance) * 100
i2_within  <- (sigma2_within / total_variance) * 100
i2_total   <- ((sigma2_between + sigma2_within) / total_variance) * 100

# Print the clean results
cat("I2 Between-Study (Refid):    ", round(i2_between, 2), "%\n")
cat("I2 Within-Study  (Arm level):", round(i2_within, 2), "%\n")
cat("Total I2:                     ", round(i2_total, 2), "%\n")


rows_vector <- c(29:24, 21:14, 11:3)
metafor::forest(
  fit_all,
  #ilab.lab = "strata_1", # name of the column
  #ilab = strata_1,  # variable in the column
  rows = rows_vector,
  mlab = glue::glue(
    "Overall model: I-square {(round(i2_total))} %"),
  xlab = "Standardized Mean Difference"
)

addpoly(fit_Methylphenidate, row = 2, font = 2, 
        mlab = glue::glue(
          "Methylphenidate Model I-squared ", 
          (round(fit_Methylphenidate$I2)), "%"), col = "lightblue")

addpoly(fit_Amphetamine, row = 13, font = 2, 
        mlab = glue::glue(
          "Amphetamine Model I-squared ", 
          (round(fit_Amphetamine$I2)), "%"), col = "lightblue")

addpoly(fit_Atomoxetine, row = 23, font = 2, 
        mlab = glue::glue(
          "Atomoxetine Model I-squared ", 
          (round(fit_Atomoxetine$I2)), "%"), col = "lightblue")






############  Now Psychosocial  ##############################
# delete all, so we don't mess up
rm(list = ls())

comparator_vs_int <- FALSE

git_raw_root <- "https://raw.githubusercontent.com/susannehempel-lab/adhd_management/refs/heads/main"
qmd_url  <- glue("{git_raw_root}/adhd_management.qmd")
d_location <- file.path(git_raw_root, "data/adhd_management_data.csv")

# 1. Download the d frame first
d <- read.csv(d_location)
d <- d %>% 
  mutate(ID = gsub("\\s*\\{[^}]*\\}", "", ID))





# no comp_atomoxetine so fill with blanks
d$comp_atomoxetine <- ""
d$comp_nonstimulant <- ""
d$comp_NS <- ""

#d <- d %>% dplyr::filter(study.design == "RCT")





d$comp_is_int <-
  # test if comp is the intervention (i.e. subgroup) of interest
  d[paste0("comp_atomoxetine")] == paste0("comp_atomoxetine")

d$symptom_mean_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$symptom_mean_comp_cont,
  # leave alone
  d$symptom_mean_int_cont
)
# same for SD  
d$symptom_SD_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$symptom_SD_comp_cont,
  # leave alone
  d$symptom_SD_int_cont
)
# same for N
d$symptom_n_int_cont <- ifelse(
  d$comp_is_int,
  # copy comp 
  d$symptom_n_comp_cont,
  # leave alone
  d$symptom_n_int_cont
)    
if(length(d$comp_is_int) > 0 && any(d$comp_is_int, na.rm = TRUE)) {
  cat("<h3> Possible previous subgroup problem now fixed. </h3><p>")
}

d <- d %>% dplyr::mutate(
  strata_1 = case_when(
    int_CBT_only == "int_CBT_only" ~ "CBT",
    int_DBT_only == "int_DBT_only" ~ "DBT",
    int_MBCT_only == "int_MBCT_only" ~ "MBCT",
    TRUE ~ "Other"
  ))


# 3 studies have a comparator of other
# moving them to control group
d_other <- d %>%
  dplyr::filter(
    Refid %in% c(8307, 2954, 1399)
  )

d_other <- d_other %>% dplyr::mutate(
  strata_1 = "Other",
  symptom_mean_int_cont = symptom_mean_comp_cont,
  symptom_SD_int_cont = symptom_SD_comp_cont,
  symptom_n_int_cont = symptom_n_comp_cont
)  

d_CBT <- d %>%
  dplyr::filter(
    Refid %in% c(1626)
  )

d_CBT <- d_CBT %>% dplyr::mutate(
  strata_1 = "CBT",
  symptom_mean_int_cont = symptom_mean_comp_cont,
  symptom_SD_int_cont = symptom_SD_comp_cont,
  symptom_n_int_cont = symptom_n_comp_cont
)  

d <- dplyr::bind_rows(d, d_other, d_CBT)


d_es <- metafor::escalc(
  m1i = symptom_mean_ctrl_cont,
  sd1i = symptom_SD_ctrl_cont,
  n1i = symptom_n_ctrl_cont,
  m2i = symptom_mean_int_cont,
  sd2i = symptom_SD_int_cont,
  n2i = symptom_n_int_cont,
  measure = "SMD", 
  slab = d$ID, 
  data = d,
  append = TRUE
) %>% dplyr::filter(
  !is.na(yi) & intervention == "Psychosocial"
)

d_es$symptom_positive_direction = "higher"

d_es <- d_es %>%
  dplyr::mutate(
    reverse = (symptom_positive_direction == "higher" &
                 symptom_value == "Lower is better") | 
      (symptom_positive_direction == "lower" &
         symptom_value == "Higher is better"),
    yi = ifelse(
      reverse,
      d_es$yi * -1, d_es$yi
    )
  ) %>% dplyr::arrange(int_label, ID)




d_es <- d_es %>%  dplyr::arrange(strata_1, ID)

d_CBT <- d_es %>% 
  dplyr::filter(strata_1 == "CBT") 
fit_CBT <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_CBT,
  slab = ID, test = "knha")

d_DBT <- d_es %>% 
  dplyr::filter(strata_1 == "DBT") 
fit_DBT <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_DBT,
  slab = ID, test = "knha")

d_MBCT <- d_es %>% 
  dplyr::filter(strata_1 == "MBCT") 
fit_MBCT <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_MBCT,
  slab = ID, test = "knha")


d_Other <- d_es %>% 
  dplyr::filter(strata_1 == "Other") 
fit_Other <- metafor::rma(
  yi = yi,
  vi = vi,
  data = d_Other,
  slab = ID, test = "knha")

V_matrix <- vcalc(vi = vi, cluster = Refid, rho = 0.5, data = d_es)
V_matrix <- as.matrix(nearPD(V_matrix, keepDiag = TRUE)$mat)

fit_all <- 
  metafor::rma.mv(
    yi = yi,
    V = V_matrix,
    random = ~ 1 | Refid / yi, 
    data = d_es,
    slab = ID, test = "knha")

# calculate I2
wi <- 1 / fit_all$vi
v_bar <- (fit_all$k - 1) * sum(wi) / (sum(wi)^2 - sum(wi^2))

# 2. Extract the variance components from your rma.mv model
sigma2_between <- fit_all$sigma2[1] # Level 3 (Refid)
sigma2_within  <- fit_all$sigma2[2] # Level 2 (Within-study arms)

# 3. Calculate total variance
total_variance <- sigma2_between + sigma2_within + v_bar

# 4. Compute the partitioned I2 percentages
i2_between <- (sigma2_between / total_variance) * 100
i2_within  <- (sigma2_within / total_variance) * 100
i2_total   <- ((sigma2_between + sigma2_within) / total_variance) * 100

# Print the clean results
cat("I2 Between-Study (Refid):    ", round(i2_between, 2), "%\n")
cat("I2 Within-Study  (Arm level):", round(i2_within, 2), "%\n")
cat("Total I2:                     ", round(i2_total, 2), "%\n")

rows_vector <- c(36:30, 27:25, 22:19, 16:3)
metafor::forest(
  fit_all,
  #ilab.lab = "strata_1", # name of the column
  #ilab = strata_1,  # variable in the column
  rows = rows_vector,
  mlab = glue::glue(
    "Overall model: I-square {(round(i2_total))} %"),
  xlab = "Standardized Mean Difference"
)

addpoly(fit_Other, row = 2, font = 2, 
        mlab = glue::glue(
          "Other Model I-squared ", 
          (round(fit_MBCT$I2)), "%"), col = "lightblue")

addpoly(fit_MBCT, row = 18, font = 2, 
        mlab = glue::glue(
          "MBCT Model I-squared ", 
          (round(fit_MBCT$I2)), "%"), col = "lightblue")

addpoly(fit_DBT, row = 24, font = 2, 
        mlab = glue::glue(
          "DBT Model I-squared ", 
          (round(fit_DBT$I2)), "%"), col = "lightblue")


addpoly(fit_CBT, row = 29, font = 2, 
        mlab = glue::glue(
          "CBT Model I-squared ", 
          (round(fit_CBT$I2)), "%"), col = "lightblue")


