# Function to do meta-regression for combined vs medication
CombMedMetaReg <- function(d_es) {
  cat("<h2>Meta-regression: Indirect comparison between combined vs(medication plus psychosocial) 
  and medication</h2>")
  
  d_es <- d_es %>%
    dplyr::filter(intervention %in% c("Combined", "Medication"))
  
  # Count how many stimulant and non-stimulant studies there are.
  n_medication_studies <- sum(d_es$intervention == "Medication")
  n_combined_studies <- sum(d_es$intervention == "Combined")
  
  cat(glue::glue(n_medication_studies, " medication  studies<p>"))
  cat(glue::glue(n_combined_studies, " combined studies<p>"))
  
  if (n_medication_studies >= 2  & n_combined_studies >= 2) {
    
    d_es <- d_es %>%
      dplyr::mutate(
        combined_study = intervention == "Combined"
      )
    
    fit_med_comb_meta <-  metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID, test="knha",
      mods =~ combined_study
    )
    
    knitr::kable(broom::tidy(fit_med_comb_meta), digits = 3) %>% print()
    
  } else {
    cat("Not enough studies to compare medication and combined<p>")
  }
  
}




# Function to do meta-regression for combined vs psychosocial
CombPsychMetaReg <- function(d_es) {
  cat("<h2>Meta-regression: Indirect comparison between combined vs(medication plus psychosocial) 
  and psychosocial</h2>")
  d_es <- d_es %>%
    dplyr::filter(intervention %in% c("Combined", "Psychosocial"))
  
  # Count how many stimulant and non-stimulant studies there are.
  n_psychosocial_studies <- sum(d_es$intervention == "Psychosocial")
  n_combined_studies <- sum(d_es$intervention == "Combined")
  
  cat(glue::glue(n_psychosocial_studies, " psychosocial  studies<p>"))
  cat(glue::glue(n_combined_studies, " combined studies<p>"))
  
  if (n_psychosocial_studies >= 2 & n_combined_studies >= 2) {
    
    d_es <- d_es %>%
      dplyr::mutate(
        combined_study = intervention == "Combined"
      )
    
    fit_ps_comb_meta <-  metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID, test="knha",
      mods =~ combined_study
    )
    
    knitr::kable(broom::tidy(fit_ps_comb_meta), digits = 3) %>% print()
    
  } else {
    cat("Not enough studies to compare psychosocial and combined<p>")
  }
}


