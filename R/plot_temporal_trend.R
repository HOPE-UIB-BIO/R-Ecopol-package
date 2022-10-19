
#' @title Plot temporal trend
#' @param data_source DESCRIPTION.
#' @param x_var Character. Name of the X-variable
#' @param x_var_name Character. label of the X-variable
#' @param y_var Character. Name of the Y-variable
#' @param y_var_name Character. label of the Y-variable
#' @param sel_type DESCRIPTION.
#' @param display_error DESCRIPTION.
#' @param y_limits DESCRIPTION.
#' @param sel_y_trans DESCRIPTION.
#' @param sel_x_trans DESCRIPTION.
#' @param x_ticks DESCRIPTION.
#' @param y_ticks DESCRIPTION.
#' @param n_y_ticks DESCRIPTION.
#' @param show_rmse DESCRIPTION.
#' @param show_bin_summary DESCRIPTION.
#' @param deafult_color_line DESCRIPTION.
#' @param default_color_rmse_highlight DESCRIPTION.
#' @param default_color_summary_fill DESCRIPTION.
#' @param default_text_size DESCRIPTION.
plot_temporal_trend <-
    function(data_source,
             x_var = "age",
             x_var_name = "Time (ka BP)",
             y_var = "var",
             y_var_name = "Estimate",
             sel_type = c("single", "group"),
             group_var = NULL,
             display_error = FALSE,
             y_limits = NULL,
             sel_y_trans = "identity",
             sel_x_trans = "reverse",
             x_ticks = seq(0, 12e3, 4e3),
             y_ticks = seq(0, 1, 0.5),
             n_y_ticks = 3,
             show_rmse = TRUE,
             show_bin_summary = TRUE,
             summary_bin_size = 1e3,
             deafult_color_line = "black",
             default_color_rmse_highlight = "red",
             default_color_summary_line = "blue",
             default_color_summary_fill = "lightblue",
             group_color_pallete = NULL,
             line_size = 1,
             line_alpha = 1,
             error_alpha = 0.2,
             summary_line_size = 0.5,
             summary_alpha = 0.5,
             default_text_size = 16) {
        if (
            is.null(data_source) | missing(data_source)
        ) {
            message("'data_source' is missing. Not plotting.")
            return(NULL)
        }


        # detect limits in the data
        if (
            missing(y_limits) ||
                is.null(y_limits) == TRUE ||
                is.na(y_limits) == TRUE
        ) {
            y_limits <-
                vector(mode = "numeric", length = 2)

            y_limits_raw <-
                data_source %>%
                dplyr::select(
                    dplyr::any_of(
                        ifelse(display_error, c("var", "upr", "lwr"), "var")
                    )
                ) %>%
                unlist()

            y_limits[1] <-
                min(y_limits_raw) * 0.9

            y_limits[2] <-
                max(y_limits_raw) * 1.1
        }

        # create automaticly y-ticks
        if (
            any(y_ticks == "auto" | missing(y_ticks))
        ) {
            y_ticks <-
                seq(
                    from = min(y_limits),
                    to = max(y_limits),
                    length.out = n_y_ticks
                )
        }

        # create a common visualisatin style for all figures
        p0 <-
            data_source %>%
            ggplot2::ggplot(
                ggplot2::aes(
                    x = get(x_var),
                    y = get(y_var)
                )
            ) +
            ggplot2::theme_classic() +
            ggplot2::labs(
                x = x_var_name,
                y = y_var_name
            ) +
            ggplot2::scale_x_continuous(
                breaks = x_ticks,
                labels = x_ticks / 1e3,
                trans = sel_x_trans
            ) +
            ggplot2::scale_y_continuous(
                limits = y_limits,
                breaks = y_ticks,
                trans = sel_y_trans,
                labels = scales::label_number(accuracy = 0.1)
            ) +
            ggplot2::theme(
                text = ggplot2::element_text(
                    size = default_text_size
                ),
                axis.text.x = ggplot2::element_text(
                    color = "black",
                    size = default_text_size
                ),
                axis.text.y = ggplot2::element_text(
                    color = "black",
                    size = default_text_size
                ),
                axis.title = ggplot2::element_text(
                    color = "black",
                    size = default_text_size,
                    margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
                ),
                legend.position = "none"
            )

        if (
            sel_type == "single"
        ) {
            if (
                display_error == TRUE
            ) {
                p1 <-
                    p0 +
                    ggplot2::geom_ribbon(
                        data = data_source,
                        ggplot2::aes(
                            ymin = lwr,
                            ymax = upr,
                        ),
                        fill = deafult_color_line,
                        colour = NA,
                        alpha = error_alpha
                    )
            } else {
                p1 <-
                    p0
            }

            res <-
                p1 +
                ggplot2::geom_line(
                    data = data_source,
                    size = line_size,
                    color = deafult_color_line
                )
        } else if (
            sel_type == "group"
        ) {
            if (
                display_error == TRUE
            ) {
                p1 <-
                    p0 +
                    ggplot2::geom_ribbon(
                        data = data_source,
                        ggplot2::aes(
                            fill = get(group_var),
                            ymin = lwr,
                            ymax = upr,
                        ),
                        colour = NA,
                        alpha = error_alpha
                    )
            } else {
                p1 <-
                    p0
            }

            if (
                show_rmse == TRUE
            ) {
                data_seq <-
                    data_source %>%
                    dplyr::group_by(get(x_var)) %>%
                    dplyr::mutate(
                        var_diff = var - mean(var, na.rm = TRUE)
                    ) %>%
                    dplyr::group_by(get(group_var)) %>%
                    dplyr::mutate(
                        col_var = mean(var_diff^2) %>%
                            sqrt()
                    ) %>%
                    dplyr::ungroup()

                p2 <-
                    p1 +
                    ggplot2::geom_line(
                        data = data_seq,
                        ggplot2::aes(
                            x = get(x_var),
                            group = get(group_var),
                            color = col_var
                        ),
                        alpha = line_alpha,
                        size = line_size
                    ) +
                    ggplot2::scale_color_gradient(
                        high = default_color_rmse_highlight,
                        low = deafult_color_line
                    )
            } else {
                p2 <-
                    p1 +
                    ggplot2::geom_line(
                        data = data_source,
                        ggplot2::aes(
                            x = get(x_var),
                            col = get(group_var),
                        ),
                        alpha = line_alpha,
                        size = line_size
                    )
            }

            if (
                show_bin_summary == TRUE
            ) {

                # Data for boxplot (mean of each time BIN for each dataset)
                data_boxplot <-
                    data_source %>%
                    util_add_age_bin(
                        bin_size = summary_bin_size
                    ) %>%
                    dplyr::group_by(get(group_var), BIN) %>%
                    dplyr::summarise(
                        .groups = "drop",
                        var = mean(var)
                    ) %>%
                    dplyr::rename(
                        !!group_var := `get(group_var)`
                    )

                res <-
                    p2 +
                    ggplot2::geom_violin(
                        data = data_boxplot,
                        ggplot2::aes(
                            x = BIN,
                            group = BIN
                        ),
                        col = NA,
                        fill = default_color_summary_fill,
                        alpha = summary_alpha
                    ) +
                    ggplot2::geom_boxplot(
                        data = data_boxplot,
                        ggplot2::aes(
                            x = BIN,
                            group = BIN
                        ),
                        col = default_color_summary_line,
                        fill = default_color_summary_fill,
                        alpha = summary_alpha,
                        width = summary_bin_size / 3,
                        size = summary_line_size,
                        outlier.shape = NA
                    )
            } else {
                res <-
                    p2
            }
        }
        return(res)
    }
