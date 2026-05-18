
# Modified from original adhd metareg functions file.
# No reverse, so that is removed.
# Don't remember what value did, so removing that.
# removing high RoB stuff


AmphMphMetaReg <- function(d_es) {
  
  cat("<h3>Meta-regression: AMPH vs MPH</h3>")
  d_es <- d_es %>%
    dplyr::filter(
      (int_AMPH == "int_AMPH" |
         comp_AMPH == "comp_AMPH" |
         int_MPH == "int_MPH" |
         comp_MPH == "comp_MPH" )
      )
  
  cat("<h3>AMPH vs MPH meta-regression</h3><p>")
  
  d_es$AMPH <- d_es$int_AMPH == "int_AMPH" | d_es$comp_AMPH == "comp_AMPH"
  
  
  # Count how many AMPH and non-AMPH studies there are.
  table(d_es$AMPH)
  
  n_AMPH_studies <- sum(d_es$AMPH)
  n_MPH_studies <- sum(!d_es$AMPH)
  
  cat(glue::glue(n_AMPH_studies, "n AMPH studies<p>"))
  cat(glue::glue(n_MPH_studies, " nMPH studies<p>"))
  
  
  if (n_AMPH_studies >= 2 & n_MPH_studies >= 2) {
    
    fit_stim_meta <-  metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID,test = "knha",
      mods =~ AMPH
    )
    
    
    knitr::kable(broom::tidy(fit_stim_meta), digits = 3) %>% print()
    
  } 
  
  if (n_AMPH_studies < 2 | n_MPH_studies < 2 ) {
    cat("Not enough studies to compare AMPH and MPH<p>")
  }
  
}




