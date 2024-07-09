plot_agp <- function(dt_agp, months){
  dt_agp %>%
    filter(inter %in% months) %>%
    mutate(inter = factor(inter, levels = months),
           bg_level = forcats::fct_rev(bg_level),
           bg_level = forcats::fct_recode(
             bg_level,
             '<54 mg/dL' = '[0,54)',
             '54-69 mg/dL' = '[54,70)',
             '70-180 mg/dL' = '[70,181)',
             '181-250 mg/dL' = '[181,251)',
             '>250 mg/dL' = '[251,Inf]'),
           txt = paste0(round(100*bg_level_dist,1), '%')) %>%
    arrange(inter, desc(bg_level)) %>%
    group_by(inter) %>%
    mutate(cumpct = cumsum(bg_level_dist)) %>%
    ungroup() %>%
    ggplot() +
    geom_col(aes(x = inter, y = bg_level_dist, fill = bg_level), color = 'black') +
    # geom_text(aes(x = inter, y = cumpct, label = txt)) +
    labs(x = 'Time (months)',
         y = '% Glucose Range',
         fill = '') +
    scale_fill_manual(values = c('#fd8d3c', '#fecc5c', '#a6d96a', '#d7191c', '#a6611a')) +
    scale_y_continuous(labels = scales::percent)
}

#where is bg_level_dist defined???
#check against tir definitions

