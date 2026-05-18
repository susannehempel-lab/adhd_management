CatMA <- function(
    data = data,
    label = "ID",
    outcome,
    comparator_analysis = TRUE,
    comparator_vs_int = comparator_vs_int,
    medication_analysis = FALSE   # if medication is TRUE, move comp to int
) {
  
  # Can't do both!
  if (comparator_vs_int) {
    comparator_analysis <- FALSE
  }
  
  # save original data
  orig_data <- data
  
  n_ctrl_categ     <-  glue::glue("{outcome}_n_ctrl_categ")
  counts_ctrl_categ <- glue::glue("{outcome}_counts_ctrl_categ")
  n_int_categ <-       glue::glue("{outcome}_n_int_categ")
  counts_int_categ <-  glue::glue("{outcome}_counts_int_categ")
  n_comp_categ      <- glue::glue("{outcome}_n_comp_categ")
  counts_comp_categ <- glue::glue("{outcome}_counts_comp_categ")
  
  
  # if doing medication analysis, copy the comp numbers into int, if 
  # intervention is not categorical.
  

  data[[n_int_categ]] <-
    ifelse(
      data$intervention != current_intervention & 
        data$comparator == current_intervention, 
      data[[n_comp_categ]], data[[n_int_categ]])
  data[[counts_int_categ]] <-
    ifelse(
      data$intervention != current_intervention & 
        data$comparator == current_intervention, 
      data[[counts_comp_categ]], data[[counts_int_categ]])
  
  
  
  data$ai <- data[[counts_ctrl_categ]]
  data$bi <- data[[n_ctrl_categ]] - data[[counts_ctrl_categ]]
  data$ci <- data[[counts_int_categ]]
  data$di <- data[[n_int_categ]] - data[[counts_int_categ]]
  data$value <- data[[paste0(outcome, "_value_categ")]]
  data$positive_direction_categ <- data[[paste0(outcome, "_positive_direction_categ")]]
  
  if (comparator_vs_int) {
    data$ai <- data[[counts_comp_categ]] 
    data$bi <- 
      data[[n_comp_categ]] - data[[counts_comp_categ]] 
  }
  
  if (subgroup != "" & !comparator_vs_int) {
    data$comp_is_int <-
      # test if comp is the intervention (i.e. subgroup) of interest
      data[paste0("comp_", subgroup)] == paste0("comp_", subgroup)
    
    data$ci <- ifelse(
      data$comp_is_int,
      # copy comp 
      data[[counts_comp_categ]],
      # leave alone
      data$ci
    )
    # same for SD  
    data$di <- ifelse(
      data$comp_is_int,
      # copy comp 
      data[[n_comp_categ]] - data[[counts_comp_categ]],
      # leave alone
      data$di
    )
    if(length(data$comp_is_int) > 0 && any(data$comp_is_int, na.rm = TRUE)) {
      cat("<h3> Possible previous subgroup problem now fixed. </h3><p>")
    }
  }

  orig_data <- data
  
  categorical_outcome <- outcome
  
  data$outcome <- data[[outcome]]
  data$measure <- data[[paste0(outcome, "_measure_categ")]]
  data <- data #%>% 
  #dplyr::filter(ai > 0 & ci > 0)  # Example: Ensure cells for RR are not zero
  
  
  data <- data %>% 
    dplyr::filter(!is.na(ai) & !is.na(bi) & !is.na(ci) & !is.na(di))
  
  
  cat("Analyzing ", nrow(data), "studies.<p>")
  if (nrow(data) == 0) {
    cat("<p>No data.<p>")
    return(NULL)
  }
  
  
  d_es <- metafor::escalc(
    ai = ai,
    bi = bi,
    ci = ci,
    di = di,
    measure = "RR", slab = ID,
    data = data,
    append = TRUE
  )
  
  # Reverse outcome variables so that lower is always better
  d_es <- d_es %>%
    dplyr::mutate(
      reverse = (positive_direction_categ == "higher" &
                   value == "Lower is better") | 
        (positive_direction_categ == "lower" &
           value == "Higher is better"),
      yi = ifelse(
        reverse,
        d_es$yi * -1, d_es$yi
      )
    ) 
  
  categorical_outcome <- outcome
  
  data$outcome <- data[[outcome]]
  data$measure <- data[[paste0(outcome, "_measure_categ")]]
  
  
  
  cat("Analyzing ", nrow(data), "studies.<p>")
  if (nrow(data) == 0) {
    cat("<p>No data.<p>")
    return(NULL)
  }
  
  d_es$comparator <- FALSE
  
  d_es <- d_es %>%
    dplyr::filter(!is.na(yi))
  
  # Order by int_label (which is blank if no label) and then by ID
  d_es <- d_es %>%
    dplyr::arrange(int_label, ID)
  
  
  # doing comparator analysis, calculate effect sizes
  # and then join to d_es
  if (comparator_analysis) {
    data <- orig_data
    
    data$ai <- data[[glue::glue("{outcome}_counts_ctrl_categ")]]
    data$bi <- 
      data[[glue::glue("{outcome}_n_ctrl_categ")]] - 
      data[[glue::glue("{outcome}_counts_ctrl_categ")]]
    data$ci <-  data[[glue::glue("{outcome}_counts_comp_categ")]]
    data$di <- data[[glue::glue("{outcome}_n_comp_categ")]] - 
      data[[glue::glue("{outcome}_counts_comp_categ")]]
    
    #data$value <- data[[value]]
    data$outcome <- data[[outcome]]
    data$measure <- data[[paste0(outcome, "_measure_categ")]]
    data <- 
      data %>%
      dplyr::filter(!is.na(ai) & !is.na(bi) & !is.na(ci) & !is.na(di))
    
    if (nrow(data) > 0) {
      d_comp_es <- metafor::escalc(
        ai = ai,
        bi = bi,
        ci = ci,
        di = di,
        measure = "RR", slab = ID, data = data,
        append = TRUE
      ) %>%
        dplyr::filter(!is.na(yi))
      
      # Reverse outcome variables so that lower is always better
      d_comp_es <- d_comp_es %>%
        dplyr::mutate(
          reverse = (positive_direction_categ == "higher" &
                       value == "Lower is better") | 
            (positive_direction_categ == "lower" &
               value == "Higher is better"),
          yi = ifelse(
            reverse,
            d_comp_es$yi * -1, d_es$yi
          )
        ) 
      
      d_comp_es <- d_comp_es %>%
        dplyr::filter(
          !(ID %in% d_es$ID)  # remove those that are already in ES
        )
      if (nrow(d_comp_es) > 0) {
        d_comp_es$comparator <- TRUE # identify these as comparator
        d_es <- dplyr::bind_rows(d_es, d_comp_es)
        
        d_es$ID <- ifelse(
          d_es$comparator, 
          glue::glue("{d_es$ID} (C)"), 
          d_es$ID
        )
      }
    }
  }
  
  
  if (nrow(d_es) > 0) {
    d_es$ID <- paste0(d_es$ID, " ", d_es$int_label)
  }
  
  cat("\n### Control Types\n\n") 
  
  # 2. Print Table (with blank lines before/after)
  d_es %>% dplyr::group_by(control) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    knitr::kable() %>%
    print()
  
  cat("\n\n") # Ensure space after table
  
  if (nrow(d_es) > 0) {
    fit_ma <- metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID, test="knha"
    )
    
    # 3. Print Meta-Analysis Table
    d_es %>%
      dplyr::mutate(se = sqrt(vi)) %>%
      dplyr::select(ID, measure, yi, se, study.design) %>%
      knitr::kable(digits = 2) %>%
      print()
    
    cat("\n\n") # Space after table
    
    # 4. Forest Plot
    forest(
      fit_ma, atransf = exp, 
      xlab = "Relative Risk", cex = 0.75, mlab = NULL,
      main = glue::glue("{outcome}:{current_intervention} ({subgroup})"))
    
    # 5. Summary Statistics (Using explicit newlines instead of <br> inside glue)
    n_int_categ <- glue::glue('{outcome}_n_int_categ')
    n_ctrl_categ <- glue::glue('{outcome}_n_ctrl_categ')
    
    cat("\n<br>\n") # Visual break
    cat(glue::glue("Number of studies = {nrow(d_es)}"), "<br>\n")
    cat(glue::glue("Intervention sample = {sum(d_es[[n_int_categ]], na.rm = TRUE)}"), "<br>\n")
    cat(glue::glue("Control sample = {sum(d_es[[n_ctrl_categ]], na.rm = TRUE)}"), "<br>\n")
    cat(glue::glue("Total sample = {sum(d_es[[n_int_categ]], na.rm = TRUE) + sum(d_es[[n_ctrl_categ]], na.rm = TRUE)}"), "\n\n")
    
    
    # 6. Relative Risk Table
    cat("### Relative Risk Estimate\n\n")
    
    coef(summary(fit_ma)) %>%
      as.data.frame() %>%
      round(2)%>%
      exp() %>%
      knitr::kable() %>% 
      print()
    
    cat("\n\n")
    cat("I-squared = ", summary(fit_ma)$I2 %>% round(), "%\n\n")
    
    if (nrow(d_es) > 2) {
      cat("##### Publication Bias Tests\n\n")
      
      cat("###### Begg (non-parametric)\n")
      cat("<pre>")
      ranktest(fit_ma) %>% capture.output() %>% cat(sep = "\n")
      cat("</pre>\n")
      
      cat("###### Egger (parametric)\n")
      cat("<pre>")
      result <- try(
        regtest(fit_ma) %>% capture.output() %>% cat(sep = "\n")
      )
      cat("</pre>\n")
      
      if (inherits(result, "try-error")) {
        warning("Regtests() did not converge and is skipped")
      } 
      
      cat("###### Trim and Fill\n\n")
      
      trimfill_result <- try(metafor::trimfill(fit_ma), silent = TRUE)
      
      if (inherits(trimfill_result, "try-error")) {
        warning("Trim and fill failed to converge")
      } else {
        cat("Estimate and CIs\n\n")
        
        coef(summary(trimfill(fit_ma))) %>%
          as.data.frame() %>%
          dplyr::select(estimate, ci.lb, ci.ub) %>%
          dplyr::mutate(
            estimate = exp(estimate),
            ci.lb = exp(ci.lb),
            ci.ub = exp(ci.ub)
          ) %>%
          knitr::kable(digits = 2) %>% 
          print()
        
        cat("\n\nTrimFill Relative Risk Estimate:", 
            round(exp(coef(trimfill(fit_ma, stepadj=0.5, maxiter=10000)))), "\n\n")
      }
      cat("<h6>int_stimulant vs int_nonstimulant meta-regression</h6>")
      
      StimMetaReg(d_es)
      AmphMphMetaReg(d_es)
      CombMedMetaReg(d_es)
      CombPsychMetaReg(d_es)
      
      
      # do dose response for int_MPH and int_AMPH subgroups  
      
      do_dose_metaregression <- subgroup %in% c("MPH", "AMPH") &
        outcome %in% c("AEs", "symptom", "BP")
      
      if (do_dose_metaregression) {
        cat("\n\nTrying to do dose-response metaregression.\n\n")
        
        d_es <- d_es %>% dplyr::filter(!is.na(dose))
      }
      
      if (nrow(d_es) < 3) {
        cat("Not enough studies for dose-response meta-regression.\n\n")
        do_dose_metaregression <- FALSE
      }
      
      if (do_dose_metaregression) {  
        fit_ma_dr <- metafor::rma(
          data = d_es, yi = yi, vi = vi,
          slab = ID, test="knha", mods = ~dose
        )
        d_es %>%
          dplyr::mutate(se = sqrt(vi)) %>%
          dplyr::select(ID, measure, yi, se, study.design, dose) %>%
          knitr::kable(digits = 2) %>%
          print()
        cat("\n\n")
        coef(summary(fit_ma_dr)) %>% knitr::kable() %>% print()
        
        cat("\n\n")
      }
    }
  } else {
    return("No data for meta-analysis.<p>")
  }
  invisible(fit_ma)
}

