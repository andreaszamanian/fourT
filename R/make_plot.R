make_plot <- function(df){
  df %>%
    dplyr::mutate(range = as.factor(df$range),
                  range = forcats::fct_recode(
                    range,
                    '<54 mg/dL' = '1',
                    '54-69 mg/dL' = '2',
                    '70-180 mg/dL' = '3',
                    '181-250 mg/dL' = '4',
                    '>250 mg/dL' = '5'),
                  range = forcats::fct_rev(range),
                  txt = paste0(round(100 * bg_level_dist, 1), '%')) %>%
    dplyr::mutate(cumpct = cumsum(bg_level_dist)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(inter), y = bg_level_dist, fill = range)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 12, margin = ggplot2::margin(t = -10), color = "black"),
                   axis.text.y = ggplot2::element_blank()) +
    ggplot2::geom_col(position = "stack", color = 'white') +
    ggplot2::geom_text(ggplot2::aes(label = txt), position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::geom_text(ggplot2::aes(label = paste("n =", n), y = 1), vjust = -0.5, color = "black", size = 3) +  # Add n values above bars
    ggplot2::labs(x = '', y = '% Glucose Range', fill = 'Glucose Range') +
    ggplot2::scale_fill_manual(values = scales::alpha(c('#6d071a', '#d7191c', '#4cbb17', '#fecc5c', '#fd8d3c'), alpha = 0.7)) +
    ggplot2::scale_y_continuous(labels = scales::percent)
}

