
# Modified from original adhd metareg functions file.
# No reverse, so that is removed.
# Don't remember what value did, so removing that.
# removing high RoB stuff


MPHDoseMetaReg <- function(d_es) {
  
  cat("<h3>Meta-regression: dose for Methylphenidate</h3>")

  # Print table of doses
  d_es %>% dplyr::filter(!is.na(dose)) %>%
    dplyr::group_by(dose) %>%
    dplyr::summarise(N = dplyr::n()) %>% 
    knitr::kable() %>% print()
  
  
  if (n_stimulant_studies >= 2 & n_nonstimulant_studies >= 2) {
    
    fit_dose_meta <-  metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID,
      test="knha",
      mods =~ dose
    )
    
    
    knitr::kable(broom::tidy(fit_dose_meta), digits = 3) %>% print()
    
  } 
  

  
}




