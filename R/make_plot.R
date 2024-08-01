make_plot <- function(df){
  df <- df %>%
    dplyr::mutate(range = as.character(df$range)) #move this outside of the make_plot() function

  print(df %>%
          dplyr::mutate(range = as.factor(df$range),
                        range = forcats::fct_recode(
                          range,
                          '<54 mg/dL' = '1',
                          '54-69 mg/dL' = '2',
                          '70-180 mg/dL' = '3',
                          '181-250 mg/dL' = '4',
                          '>250 mg/dL' = '5'),
                        range = forcats::fct_rev(range),
                        txt = paste0(round(100*bg_level_dist,1), '%')) %>%
          dplyr::mutate(cumpct = cumsum(bg_level_dist)) %>%
          dplyr::ungroup()%>%
          ggplot2::ggplot() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         axis.line = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank()) +
          ggplot2::geom_col(ggplot2::aes(x = "", y = bg_level_dist, fill = range), color = 'black') +
          ggplot2::geom_text(ggplot2::aes(x = "", y = cumpct, label = txt)) +
          ggplot2::labs(x = '',
                        y = '% Glucose Range',
                        fill = '') +
          ggplot2::scale_fill_manual(values = c('#6d071a', '#d7191c', '#4cbb17', '#fecc5c','#fd8d3c')) +
          ggplot2::scale_y_continuous(labels = scales::percent))
}
