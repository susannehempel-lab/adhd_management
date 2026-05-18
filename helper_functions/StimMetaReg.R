
# Modified from original adhd metareg functions file.
# No reverse, so that is removed.
# Don't remember what value did, so removing that.
# removing high RoB stuff


StimMetaReg <- function(d_es) {
  
  cat("<h3>Meta-regression: stimulant vs non-stimulant</h3>")
  d_es <- d_es %>%
    dplyr::filter(
      (int_S == "int_S" |
         comp_S == "comp_S" |
         int_NS == "int_NS")
      )
  
  d_es$stimulant_study <- d_es$int_S == "int_S" | d_es$comp_S == "comp_S"
  
  
  # Count how many stimulant and non-stimulant studies there are.
  n_stimulant_studies <- sum(d_es$stimulant_study) 
  n_nonstimulant_studies <- sum(!d_es$stimulant_study) 
  
  cat(glue::glue(n_stimulant_studies, " stimulant studies<p>"))
  cat(glue::glue(n_nonstimulant_studies, " nonstimulant studies<p>"))
  
  
  if (n_stimulant_studies >= 2 & n_nonstimulant_studies >= 2) {
    
    fit_stim_meta <-  metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID,test="knha",
      mods =~ stimulant_study
    )
    
    
    knitr::kable(broom::tidy(fit_stim_meta), digits = 3) %>% print()
    
  } 
  
  if (n_stimulant_studies <2 | n_nonstimulant_studies < 2 ) {
    cat("Not enough studies to compare stimulant and nonstimulant<p>")
  }
  
}




