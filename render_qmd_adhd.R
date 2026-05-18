# This file exists to render the qmd file.
# This is done for the subgroup analysis
library(dplyr)
library(here)
library(quarto)

# This tells the metareg functions whether to do comparator vs int or control
# vs int (the default) TRUE vs FALSE
# Probably should be false
comparator_vs_int <- FALSE


git_root <- glue::glue(
  "https://github.com/susannehempel-lab/adhd_management/raw/refs/",
  "heads/main"
  )
git_raw_root <- glue::glue(
  "https://raw.githubusercontent.com/susannehempel-lab/adhd_management/refs/heads/main"
)


data_location <- 
  file.path(
    git_root, "data/adhd_management_data.csv"
  )

git_root <- "https://raw.githubusercontent.com/susannehempel-lab/adhd_management/refs/heads/main"
qmd_url  <- glue("{git_root}/adhd_management.qmd")
data_path <- file.path(getwd(), "your_data_file.RData")

# 2. Download the QMD file to a temporary location
qmd_location <- tempfile(fileext = ".qmd")
download.file(url = qmd_url, destfile = qmd_location, mode = "wb")



d <- read.csv(data_location)



subgroup <- ""
# If running a subgroup analysis, put variable name here or leave blank
# The intervention subtypes int_stimulant, comp_stimulant, int_methylphenidate, int_amphetamine 
# are for the direct comparisons (int vs comp), e.g., int_stimulant was only used when 
# the comparator arm was not also a stimulant.
# The variables int_S, int_NS, int_MPH, int_AMPH are for these subgroup analyses.
# CBT_only, DBT_only, and MBCT_only indicates the intervention is primarily CBT, DBT, or MBCT
# subgroup <- "longacting"
# subgroup <- "stimulant"
# subgroup <- "nonstimulant"
# subgroup <- "methylphenidate"
# subgroup <- "S"
# subgroup <- "NS"
# subgroup <- "MPH"
# subgroup <- "AMPH"
# subgroup <- "bupropion"
# subgroup <- "atomoxetine"
# subgroup <- "CBT_only"
# subgroup <- "DBT_only"
# subgroup <- "MBCT_only"


# temporary fix because create a variable called comparator, and the data 
# has a variable called comparator.
# d <- d %>%
# dplyr::select(-comparator)

# One study has both stimulant and non-stimulant. Remove if we're doing
# stimulant or non-stimulant subgroups
# if (subgroup %in% c("S", "NS" , "MPH" , "AMPH" , "stimulant", "nonstimulant")) {
#   d <- d %>% 
#     dplyr::filter(
#       !(int_stimulant == "int_stimulant" &
#           int_nonstimulant == "int_nonstimulant")
#     )
# }

# no comp_atomoxetine so fill with blanks
d$comp_atomoxetine <- ""
d$comp_nonstimulant <- ""
d$comp_NS <- ""

# remove subgroups
if (subgroup != "") {
  if (subgroup != "longterm") {
    # if comp_{subgroup} doesn't exist, it throws an error, so check and fill with
    # NA.
    if (!(paste0("comp_", subgroup) %in% names(d))) {
      d[paste0("comp_", subgroup)] <- ""
    }
    #check if int_subgroup or comp_subgroup are in the subgroup
    keep <- d[paste0("int_", subgroup)] == paste0("int_", subgroup) |
      d[paste0("comp_", subgroup)] == paste0("comp_", subgroup)
    # if comp and int are subgroup, then we don't want them
    keep <- keep & !(d[paste0("int_", subgroup)] == paste0("int_", subgroup) &
      d[paste0("comp_", subgroup)] == paste0("comp_", subgroup))
  } 
  if (subgroup == "longterm") {
    keep <- d[paste0("int_", subgroup)] == paste0("int_", subgroup) 
  } 
  d <- d[keep, ]
}


my_local_data_path <- file.path(getwd(), "adhd_params.RData")
save.image(file = my_local_data_path)

output_file <- glue::glue("adhd_management_{subgroup}.html")


my_local_data_path <- file.path(getwd(), "adhd_params.RData")
save.image(file = my_local_data_path)

output_file <- glue::glue("adhd_management_{subgroup}.html")

if (comparator_vs_int) {
  output_file <- glue::glue("analysis_management_comparator_{subgroup}.html")
}

quarto::quarto_render(
  input = qmd_location, 
  execute_params = list(data_path = data_path),
  output_format  = "html",
  output_file = output_file
)



############# RCTs only, not CTs#####################################
# only relevant for the main analysis, not intervention subgroups
d_orig <- d
d <- d %>% dplyr::filter(study.design == "RCT")

save.image("data.RData")

output_file <- glue::glue("adhd_management_RCT_only_{subgroup}.html")

if (comparator_vs_int) {
  output_file <- 
    glue::glue("adhd_management_RCT_only_comparator_{subgroup}.html")

}

quarto::quarto_render(
  qmd_location, output_format  = "html",
  output_file = output_file
)

### Remove high Risk of Bias Studies ########################
# only relevant for main analysis, not intervention subgroups
d <- d_orig
d <- d %>% dplyr::filter(RoB != "High risk")

save.image("data.RData")

output_file <- glue::glue("adhd_management_no_high_RoB_{subgroup}.html")

if (comparator_vs_int) {
  output_file <- 
    glue::glue("adhd_management_no_high_RoB_comparator_{subgroup}.html")
}

quarto::quarto_render(
  qmd_location, output_format  = "html",
  output_file = output_file
)


############################################################################
# Comparison vs Intervention Analysis
# This analysis removes all of the control variables, and then 
# renames the comparison variables to control variables, 
# Then we can use the same QMD file to create the html. 
# This is comp stimulant vs int non_stimulant 
# (which should have been CER vs int, re-labelled in Distiller instead)

comparator_vs_int <- TRUE

if (comparator_vs_int) {
  output_file <- 
    glue::glue(
      "adhd_management_comp_stimulant_vs_int_nonstimulant_comparator_{subgroup}.html"
      )
}

d <- d %>%
  dplyr::filter(
    comp_stimulant == "comp_stimulant" &
      int_nonstimulant == "int_nonstimulant")

save.image("data.RData")

quarto::quarto_render(
  qmd_location, output_format  = "html",
  output_file = 
    output_file
)


