map.data.distribution.by.var <-
  function(data_source,
           var,
           title = var,
           coord_long,
           coord_lat,
           custom_palette,
           palette_set = "Set1") {

    #' @title Plot geographic distribution

    #' @param data_source Data.frame with data

    #' @param var Character. Name of the variable which should be used to
    #' colour-group data

    #' @param title Optional. Name of the variable to display

    #' @param coord_long Optional. Vector containing min and max longitude
    #' to display

    #' @param coord_lat Optional. Vector containing min and max latitude
    #' to display

    #' @param custom_palette Optional. Named vector containing colour for levels
    #' of `var`

    #' @param palette_set if `custom_palette` is not specified, function will
    #' create new palette using `RColorBrewer::brewer.pal`. `palette_set` can
    #' specify which general palette to use.

    #' @return ggplot with the geographic distribution of data

# #
#     if(missing(custom_palette)){
#
#       var_list <-
#         data_source %>%
#         dplyr::distinct(get(var)) %>%
#         purrr::pluck(1)
#
#       var_list_length <- length(var_list)
#
#       get.palette.set <-
#         grDevices::colorRampPalette(
#           RColorBrewer::brewer.pal(min(max(var_list_length,3),8), palette_set))
#
#       custom_palette <- get.palette.set(length(var_list))
#
#       names(custom_palette) <- var_list
#
#     }
#
#
#     if(missing(coord_long)){
#       xmin <-  min(data_source$long)
#       xmax <-  max(data_source$long)
#     } else {
#       xmin <-  min(coord_long)
#       xmax <-  max(coord_long)
#     }
#
#     if(missing(coord_lat)){
#       ymin <-  min(data_source$lat)
#       ymax <-  max(data_source$lat)
#     } else {
#       ymin <-  min(coord_lat)
#       ymax <-  max(coord_lat)
#     }
#
#     p_1 <-
#       data_source %>%
#       ggplot2::ggplot(
#         ggplot2::aes(
#           x = long,
#           y = lat,
#           col = get(var))) +
#       ggplot2::borders(
#         fill = map_color_fill,  #[Config]
#         colour = map_color_border) +  #[Config]
#       ggplot2::geom_point(
#         size = point_size,  #[Config]
#         shape = 1) +
#       ggplot2::geom_point(
#         size = 1,
#         shape = 20) +
#       ggplot2::coord_quickmap(
#         ylim = c(ymin, ymax),
#         xlim = c(xmin, xmax)) +
#       ggplot2::scale_color_manual(values = custom_palette)+
#       ggplot2::scale_fill_manual(values = custom_palette)+
#       ggplot2::theme(
#         legend.title = element_blank(),
#         plot.title = element_text(size = text_size * 1.2),  #[Config]
#         legend.position = "bottom") +
#       ggplot2::labs(
#         title = title,
#         caption = paste0("Distribution of the sequences (N = ",nrow(data_source),")"),
#         x = "Longitude",
#         y = "Latitude")+
#       ggplot2::guides(
#         colour = ggplot2::guide_legend(
#           override.aes = list(
#             size = point_size,  #[Config]
#             shape = 19),
#           nrow = 3))
#
#     xbp <-
#       data_source %>%
#       ggplot2::ggplot(
#         ggplot2::aes(
#           x = long,
#           fill = get(var))) +
#       ggplot2::geom_histogram(
#         binwidth = 5,
#         size = line_size,  #[Config]
#         color = gray_dark,  #[Config]
#         alpha = 0.3) +
#       ggplot2::scale_color_manual(values = custom_palette)+
#       ggplot2::scale_fill_manual(values = custom_palette)+
#       ggplot2::theme(
#         legend.position = "none")+
#       ggplot2::labs(
#         x = "Longitude",
#         y = "Number of sequences")
#
#     ybp <-
#       data_source %>%
#       ggplot2::ggplot(
#         ggplot2::aes(
#           x = lat,
#           fill = get(var))) +
#       ggplot2::geom_histogram(
#         binwidth = 5,
#         size = line_size,  #[Config]
#         color = gray_dark,  #[Config]
#         alpha = 0.3) +
#       ggplot2::scale_color_manual(values = custom_palette)+
#       ggplot2::scale_fill_manual(values = custom_palette)+
#       ggplot2::theme(
#         legend.position = "none")+
#       ggplot2::labs(
#         x = "Latitude",
#         y = "Number of sequences")
#
#
#     p_2 <-
#       data_source %>%
#       dplyr::group_by(get(var)) %>%
#       dplyr::summarise(N = dplyr::n()) %>%
#       dplyr::rename(!!var := `get(var)`) %>%
#       ggplot2::ggplot(
#         ggplot2::aes(
#           y = N,
#           x = get(var),
#           label  = N,
#           fill = get(var))) +
#       ggplot2::geom_bar(
#         stat = "identity",
#         color = gray_dark,  #[Config]
#         size = line_size,  #[Config]
#         alpha = 0.3) +
#       ggplot2::geom_text(vjust = 0, nudge_y = 0.5) +
#       ggplot2::scale_color_manual(values = custom_palette)+
#       ggplot2::scale_fill_manual(values = custom_palette)+
#       ggplot2::theme(
#         legend.position = "none",
#         axis.text.x = ggplot2::element_text(
#           angle = 45,
#           hjust = 1),
#         axis.title.x = ggplot2::element_blank()) +
#       ggplot2::labs(
#         x = var,
#         y = "Number of sequences")
#
#     # construct figure
#     global_fig <-
#       ggpubr::ggarrange(
#         p_2, xbp, ybp,
#         nrow = 3,
#         heights = c(1, 1, 1))
#
#     ggpubr::ggarrange(
#       p_1, global_fig,
#       nrow = 1,
#       widths = c(1, 0.4))
  }
