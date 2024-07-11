compute_wear_time <- function(start, end, fumonths){
  ## My take on reading CGM data and computing wear time
  dt_wear <- lapply(1:nrow(dt_cgm), \(x) {
    print(x)

    dt_dex <- read_dexcom(file_path = glue::glue('{path}/{dt_cgm$files[x]}'),
                          pid = dt_cgm$record_id[x],
                          start = dt_cgm$consent_date[x],
                          end = dt_cgm$study_end_date[x])

    #read_dexcom output has different format then as in guideline doc
    #assuming cgm is equivalent to bg_value_num with high/low tags converted to numerics

    if (nrow(dt_dex$cgm > 0)) {
      ncgm <- dt_dex$cgm %>%
        transmute(date = lubridate::as_date(time)) %>%
        count(date)

      tibble(
        date = seq.Date(dt_cgm$consent_date[x]+1, dt_cgm$study_end_date[x]-1, by = 'days'),
        day_number = difftime(date, min(date), units = 'day') %>% as.numeric()) %>%
        left_join(ncgm, by = join_by(date)) %>% #here
        mutate(record_id = dt_cgm$record_id[x],
               n = ifelse(is.na(n), 0, n))
    } else {
      tibble(
        date = seq.Date(dt_cgm$consent_date[x]+1, dt_cgm$study_end_date[x]-1, by = 'days'),
        day_number = difftime(date, min(date), units = 'day') %>% as.numeric()) %>%
        mutate(record_id = dt_cgm$record_id[x],
               n = 0)
    }
  }) %>% bind_rows() #here


  dt_gg <-  dt_wear %>%
    group_by(record_id) %>%
    mutate(rank = sum(n)) %>%
    ungroup() %>%
    mutate(wear = pmin(1, n/288), #
           rank = factor(-rank) %>% as.numeric()) %>%
    arrange(rank)

  lbl <- dt_gg %>%
    select(rank, record_id) %>%
    distinct()
}
