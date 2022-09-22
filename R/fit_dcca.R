fit_dcca <-
  function(data_pollen,
           data_levels) {
    sp <-
      data_pollen %>%
      as.data.frame() %>%
      column_to_rownames("sample_id") %>%
      round(., digits = 2)

    env <- data_levels$age

    # This should be the location where 'canoco.exe' file is located
    output <-
      dcca_select_can(
        path = "TBD", # here is a needed path ot the package internal files
        resp = sp,
        pred_var = env,
        base = "xx",
        downweight = FALSE
      )

    return(output)
  }
