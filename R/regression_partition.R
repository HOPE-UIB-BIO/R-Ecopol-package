#' @title Partition data using regression tree
#' @param data_source Data frame with the variables to partition
#' @param var Name (in quotes) of variable to partition
#' @param age_var Name (in quotes) of variable which define the age of samples
#' @description Partition chronologically ordered data using regression trees
#' @export
regression_partition <-
    function(data_source,
             var = "",
             age_var = "") {
        util_check_class("data_source", "data.frame")

        util_check_class("var", "character")

        util_check_class("age_var", "character")

        util_check_col_names("data_source", c(var, age_var))

        rpart_result <-
            mvpart::rpart(
                formula = get(var) ~ get(age_var),
                method = "anova",
                data = data_source
            )

        cp_table <- rpart_result$cptable

        if (
            nrow(cp_table) > 1
        ) {
            pruned_tree <-
                mvpart::prune(
                    tree = rpart_result,
                    cp = cp_table[which.min(cp_table[, "xerror"]), "CP"]
                )
        } else {
            pruned_tree <-
                mvpart::prune(
                    tree = rpart_result,
                    cp = 0
                )
        }

        rpart_partitions <-
            data_source %>%
            dplyr::mutate(
                partition = util_renumber_groups(pruned_tree$where))

        rpart_groups <-
            rpart_partitions$partition %>%
            unique() %>%
            length()

        # wrapper to prevent any message
        capture.output(
            change_points_age <-
                as.data.frame(summary(pruned_tree)$splits) %>%
                purrr::pluck("index"),
            file = "NUL"
        )

        result <-
            list(
                rpart_result = rpart_result,
                pruned_tree = pruned_tree,
                rpart_groups = rpart_groups,
                rpart_change_points = change_points_age,
                rpart_partitions = rpart_partitions
            )

        return(result)
    }
