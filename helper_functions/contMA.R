

ContMA <- function(
    data,
    label = "ID",
    outcome,
    comparator_analysis = TRUE, 
    comparator_vs_int = FALSE,
    medication_analysis = FALSE  # if medication is TRUE, move comp to int
) {
  
  # Can't do both!
  if (comparator_vs_int) {
    comparator_analysis <- FALSE
  }
  
  mean_ctrl_cont = glue::glue("{outcome}_mean_ctrl_cont")
  SD_ctrl_cont = glue::glue("{outcome}_SD_ctrl_cont")
  n_ctrl_cont = glue::glue("{outcome}_n_ctrl_cont")
  mean_int_cont = glue::glue("{outcome}_mean_int_cont")
  SD_int_cont = glue::glue("{outcome}_SD_int_cont")
  n_int_cont = glue::glue("{outcome}_n_int_cont")
  
  mean_comp_cont = glue::glue("{outcome}_mean_comp_cont")
  SD_comp_cont = glue::glue("{outcome}_SD_comp_cont")
  n_comp_cont = glue::glue("{outcome}_n_comp_cont")
  
  
  # if comparator vs int, swap comp for ctrl
  if (comparator_vs_int) {
    mean_ctrl_cont = glue::glue("{outcome}_mean_comp_cont")
    SD_ctrl_cont = glue::glue("{outcome}_SD_comp_cont")
    n_ctrl_cont = glue::glue("{outcome}_n_comp_cont")
  }

  
  
  data$mean_ctrl_cont <- data[[mean_ctrl_cont]]
  data$SD_ctrl_cont <- data[[SD_ctrl_cont]]
  data$n_ctrl_cont <- data[[n_ctrl_cont]]
  data$mean_int_cont <- data[[mean_int_cont]]
  data$SD_int_cont <- data[[SD_int_cont]]
  data$n_int_cont <- data[[n_int_cont]]
  data$mean_comp_cont <- data[[mean_comp_cont]]
  data$SD_comp_cont <- data[[SD_comp_cont]]
  data$n_comp_cont <- data[[n_comp_cont]]
  #data$value <- data[[value]]
  data$outcome <- data[[outcome]]
  data$measure <- data[[paste0(outcome, "_measure_cont")]]
  data$value <- data[[paste0(outcome, "_value")]]
  data$positive_direction <- data[[paste0(outcome, "_positive_direction")]]
  
  
  # If doing medication analysis, and intervention is not medication, copy
  # medication values into intervention
  
  data <- data %>%
    dplyr::mutate(
      mean_int_cont = ifelse(
        intervention != current_intervention & 
          comparator == current_intervention, 
        mean_comp_cont, mean_int_cont),
      SD_int_cont = ifelse(
        intervention != current_intervention & 
          comparator == current_intervention, 
        SD_comp_cont, SD_int_cont),
      n_int_cont = ifelse(
        intervention != current_intervention & 
          comparator == current_intervention, 
        n_comp_cont, n_int_cont)
    )
  
  
  
  # if we are doing a subgroup analysis, we need to check if the subgroup
  # is in the comparator. If it is, we need to put the comparator (comp...)
  # outcomes into the intervention (int_...) outcomes.
  
  if (!comparator_vs_int & subgroup != "") {
    data$comp_is_int <-
      # test if comp is the intervention (i.e. subgroup) of interest
      data[paste0("comp_", subgroup)] == paste0("comp_", subgroup)
    
    data$mean_int_cont <- ifelse(
      data$comp_is_int,
      # copy comp 
      data$mean_comp_cont,
      # leave alone
      data$mean_int_cont
    )
    # same for SD  
    data$SD_int_cont <- ifelse(
      data$comp_is_int,
      # copy comp 
      data$SD_comp_cont,
      # leave alone
      data$SD_int_cont
    )
    # same for N
    data$n_int_cont <- ifelse(
      data$comp_is_int,
      # copy comp 
      data$n_comp_cont,
      # leave alone
      data$n_int_cont
    )    
    if(length(data$comp_is_int) > 0 && any(data$comp_is_int, na.rm = TRUE)) {
      cat("<h3> Possible previous subgroup problem now fixed. </h3><p>")
    }
  }
  
  
  
  orig_data <- data
  
  continuous_outcome <- outcome
  
  
  data <- data %>%
    dplyr::filter(
      !is.na(mean_ctrl_cont) & !is.na(SD_ctrl_cont) & !is.na(n_ctrl_cont) &
        !is.na(mean_int_cont) & !is.na(SD_int_cont) & !is.na(n_int_cont) &
        SD_ctrl_cont > 0 & SD_int_cont > 0 &
        n_ctrl_cont >= 2 & n_int_cont >= 2 # Basic conditions for SMD
    ) 
  
  if (nrow(data) == 0) {
    cat("<p>No data #1.<p>")
    return(NULL)
  }
  
  d_es <- metafor::escalc(
    m1i = mean_ctrl_cont,
    sd1i = SD_ctrl_cont,
    n1i = n_ctrl_cont,
    m2i = mean_int_cont,
    sd2i = SD_int_cont,
    n2i = n_int_cont,
    measure = "SMD", 
    slab = data$ID, 
    data = data,
    append = TRUE
  )
  
  d_es$comparator <- FALSE
  
  
  # doing comparator analysis, calculate effect sizes
  # and then join to d_es
  if (comparator_analysis) {
    data <- orig_data
    data$mean_comp_cont <- data[[glue::glue("{outcome}_mean_comp_cont")]]
    data$SD_comp_cont <- data[[glue::glue("{outcome}_SD_comp_cont")]]
    data$n_comp_cont <- data[[glue::glue("{outcome}_n_comp_cont")]]
    data$mean_ctrl_cont <- data[[mean_ctrl_cont]]
    data$SD_ctrl_cont <- data[[SD_ctrl_cont]]
    data$n_ctrl_cont <- data[[n_ctrl_cont]]
    data$outcome <- data[[outcome]]
    data$measure <- data[[paste0(outcome, "_measure_cont")]]
    data$value <- data[[paste0(outcome, "_value")]]
    data$positive_direction <- data[[paste0(outcome, "_positive_direction")]]
    
    
    data <- data %>%
      dplyr::filter(
        !is.na(mean_ctrl_cont) & !is.na(SD_ctrl_cont) & !is.na(n_ctrl_cont) &
          !is.na(mean_comp_cont) & !is.na(SD_comp_cont) & !is.na(n_comp_cont) &
          SD_ctrl_cont > 0 & SD_comp_cont > 0 &
          n_ctrl_cont >= 2 & n_comp_cont >= 2 # Basic conditions for SMD
      ) 
    
    if (nrow(data) > 0) {
      d_comp_es <- metafor::escalc(
        m1i = mean_ctrl_cont,
        sd1i = SD_ctrl_cont,
        n1i = n_ctrl_cont,
        m2i = mean_comp_cont,
        sd2i = SD_comp_cont,
        n2i = n_comp_cont,
        measure = "SMD", 
        slab = data$ID, 
        data = data,
        append = TRUE
      ) 
      d_comp_es$comparator <- TRUE # identify these as comparator
      d_comp_es <- d_comp_es %>%
        dplyr::filter(
          !(ID %in% d_es$ID)  # remove those that are already in ES
        )
      d_es <- dplyr::bind_rows(d_es, d_comp_es)
      
      d_es$ID <- ifelse(
        d_es$comparator == TRUE, 
        paste0(d_es$ID, " (C)"),
        d_es$ID
      )
    }
  }
  
  if (nrow(d_es) > 0) {
    d_es$ID <- paste0(d_es$ID, " ", d_es$int_label)
  }
  # Reverse continuous variables so that lower is always better
  d_es <- d_es %>%
    dplyr::mutate(
      reverse = (positive_direction == "higher" &
                   value == "Lower is better") | 
        (positive_direction == "lower" &
           value == "Higher is better"),
      yi = ifelse(
        reverse,
        d_es$yi * -1, d_es$yi
      )
    ) 
  
  
  # Order by int_label (which is blank if no label) and then by ID
  d_es <- d_es %>%
    dplyr::arrange(int_label, ID)
  
  if (nrow(d_es) == 0) {
    cat("No data to analyze")
    return(NULL)
  }
  
  if (nrow(d_es) > 0) {
    
    fit_ma <- metafor::rma(
      data = d_es,
      yi = yi,
      vi = vi,
      slab = ID, test = "knha")
    
    d_es %>%
      dplyr::mutate(se = sqrt(vi)) %>%
      dplyr::select(
        ID, measure,  
        mean_ctrl_cont, SD_ctrl_cont, n_ctrl_cont, 
        mean_int_cont, SD_int_cont, n_int_cont, 
        yi, se, study.design) %>%
      knitr::kable() %>% print()
    
    forest(
      fit_ma, xlab = "Standardized Mean Difference", 
      cex = 0.75, mlab = NULL, fonts = "Arial",
      main = glue::glue("{outcome}:{current_intervention} ({subgroup})")
    )
    
    broom::tidy(fit_ma, conf.int = TRUE) %>%
      dplyr::select(
        estimate, conf.low, conf.high
      ) %>%
      kable(digits = 2) %>%
      print()
    
    cat(
      glue::glue(
        "Number of studies = {nrow(d_es)}<p>
      Intervention sample =  {sum(d_es$n_int_cont,  na.rm = TRUE)}<p>
      Control sample = {sum(d_es$n_ctrl_cont,  na.rm = TRUE)} <p>
      Total sample = {sum(d_es$n_int_cont,  na.rm = TRUE) + sum(d_es$n_ctrl_cont,  na.rm = TRUE)}.<p>"
      )
    )
    
    
    coef(summary(fit_ma)) %>%
      as.data.frame() %>%
      # dplyr::select(estimate, ci.lb, ci.ub) %>%
      round(2)%>%
      knitr::kable()
    
    cat("<p>I-squared = ", summary(fit_ma)$I2 %>% round(), "% <p>")
    
    cat("<p>Convert Effect Size to Real Units<p>")
    
    std_effect <- fit_ma$b[1]
    
    mean_sd <- mean(d_es$SD_ctrl_cont, na.rm = TRUE)
    
    cat(glue::glue(
      "Estimated effect in original units = {round(std_effect * mean_sd, 2)}.<p>"
    ))
    
    
    
    if (nrow(d_es) > 2) {
      cat("<h5>Publication Bias Tests</h5>")
      cat("<h6>Begg (non-parametric)</h6>")
      ranktest(fit_ma) %>% capture.output() %>% cat()
      cat("<h6>Egger (parametric)</h6>")
      cat(regtest(fit_ma) %>% capture.output(), sep = "\n" )
      
      
      cat("<h6>Trim and Fill</h6>")
      
      # Line below breaks quarto
      #metafor::trimfill(fit_ma) %>% print()
      cat("Estimate and Cis<p>")
      coef(summary(trimfill(fit_ma))) %>%
        as.data.frame() %>%
        dplyr::select(estimate, ci.lb, ci.ub) %>%
        round(2) %>%
        knitr::kable() %>% print()
      
      StimMetaReg(d_es)
      AmphMphMetaReg(d_es)
      CombMedMetaReg(d_es)
      CombPsychMetaReg(d_es)

      
      
      do_dose_metaregression <- subgroup %in% c("MPH", "AMPH") 
      
      if (do_dose_metaregression) {
        cat("\n\nTrying to do dose-response metaregression.\n\n")
        
        d_es <- d_es %>% dplyr::filter(!is.na(dose))
      }
      
      if (nrow(d_es) < 3) {
        cat("Not enough studies for dose-response meta-regression.\n\n")
        do_dose_metaregression <- FALSE
      }
      
      if (do_dose_metaregression) {  
        cat("<h3> Doing dose metaregression</h3>")
        fit_ma_dr <- metafor::rma(
          data = d_es, yi = yi, vi = vi,
          slab = ID, test="knha", mods = ~dose
        )
        d_es %>%
          dplyr::mutate(se = sqrt(vi)) %>%
          dplyr::select(ID, measure, yi, se, study.design, dose) %>%
          knitr::kable(digits = 2) %>%
          print()
        
        coef(summary(fit_ma_dr)) %>% knitr::kable() %>% print()
        
      }
    }
  } 
}

