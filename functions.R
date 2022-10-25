library(patchwork)


# Colors ----------------------------------------------------------------------|
pal <- c("#D3AB9E", "#EAC9C1", "#EBD8D0", "#FFFBFF", "#FEFEFF")
tbl_bg_color <- "#FFFBFF"




#' Display table in html format.
#'
#' @param df web data set.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples display_tibble(df = train)
#' 
display_tibble <- function(df) {
  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(
      header = \(.x) axis_label(.x),
      headerStyle = list(fontSize = "15px",
                         color = "#858585"),
      format = reactable::colFormat(separators = TRUE)
    ),
    columns = list(
      sales_per_visit = reactable::colDef(
        format = reactable::colFormat(digits = 2, 
                                      separators = TRUE, 
                                      prefix = "$")
      )
    ),
    bordered = TRUE,
    theme = reactable::reactableTheme(color = "#8A8A8A",
                                      backgroundColor = tbl_bg_color)
  )
}



#' Detect Outlier
#'
#' @param df data.frame
#' @param variable numeric: a variable from the data 'df'.
#' @param table bool: True to return the output as a data.frame.
#' @param type return the output in form of a named vector.
#'
#' @details type can be any of 'strong_lower' - strong lower outlier, 'lower' - lower outlier,
#' 'upper' - upper outlier, 'strong_upper' - strong upper outlier.
#' 
#' @example get_outlier(df = train, variable = sales_per_visit, )
get_outlier <- function(df, variable, table = FALSE, type) { 
  
  f_tbl <- df |> 
    dplyr::summarise(
      `strong lower outlier` = quantile({{ variable }}, 0.25) - IQR({{ variable }})*3,
      
      `lower outlier` = quantile({{ variable }}, 0.25) - IQR({{ variable }})*1.5,
      
      `upper outlier` = IQR({{ variable }})*1.5 + quantile({{ variable }}, 0.75),
      
      `stong upper outlier` = IQR({{ variable }})*3 + quantile({{ variable }}, 0.75)
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything(), 
                        names_to = "outlier", 
                        values_to = "value")
  
  if (table) {
    return(f_tbl)
    
  } else  {
    outliers_names <- c("strong_lower", "lower", "upper", "stong_upper")
    
    outliers <- f_tbl[["value"]]
    
    names(outliers) <- outliers_names
    
    if (!missing(type)) {
      if (all(type %in% outliers_names)) {
        
        as.vector(outliers[type])
        
      } else {
        stop(glue::glue("Invalid `type`, `type` must be any of {paste(outliers_names, collapse = ', ')}"))
      }
      
    } else {
      return(outliers)
    }
  }
}



#' Filter out outliers.
#'
#' @param df data.frame
#' @param var numeric: a variable from the data 'df'.
#' @param outlier string: the type of outlier to filter. see get_outlier() functions
#' for more details.
#'
#' @example filter_outlier(train, sales_per_visit, "strong_upper")
#' 
filter_outlier <- function(df, var, outlier) {
  if (length(outlier) == 1) {
    if (outlier %in% c("strong_lower", "lower")) {
      f_tbl <- df %>%  
        dplyr::filter({{ var }} > get_outlier(., {{ var }}, F, outlier))
      
    } else if (outlier %in% c("upper", "stong_upper")) {
      f_tbl <- df %>% 
        dplyr::filter({{ var }} < get_outlier(., {{ var }}, F, outlier))
      
    } else stop("`outlier` should be any of [strong_lower, lower] or [upper, stong_upper]")
    
  } else if (length(outlier) == 2) {
    if (all(outlier %in% c("strong_lower", "stong_upper"))) {
      outlier[[1]] <- "strong_lower"; outlier[[2]] <- "stong_upper"
      
    } else if (all(outlier %in% c("strong_lower", "upper"))) {
      outlier[[1]] <- "strong_lower"; outlier[[2]] <- "upper"
      
    } else if (all(outlier %in% c("lower", "upper"))) {
      outlier[[1]] <- "lower"; outlier[[2]] <- "upper"
      
    } else if (all(outlier %in% c("lower", "stong_upper"))) {
      outlier[[1]] <- "lower"; outlier[[2]] <- "stong_upper"
      
    } else stop("`outlier` should be any of c([strong_lower, lower], [upper, stong_upper])")
    
    f_tbl <- df %>% 
      dplyr::filter(dplyr::between({{ var }},
                                   get_outlier(., {{ var }}, type = outlier[[1]]),
                                   get_outlier(., {{ var }}, type = outlier[[2]])))
    
  } else stop("length of argument `outlier` must not be greater than two")
  
  return(f_tbl)
}



#' Filter out outliers for two variables.
#'
#' @param df data.frame
#' @param var_x numeric: a variable from the data 'df'.
#' @param var_y numeric: a variable from the data 'df'.
#' @param outlier string: the type of outlier to filter see get_outlier.
#'
#' @example filter_outlier2(train, sales_per_visit, purchase_visits, c(lower, upper))
#' 
filter_outlier2 <- function(df, var_x, var_y, outlier) {
  error_ex <- 
    paste("example:\n`list(var='both', outlier=list(x=c('slo', 'suo'),",
          "y='uo')`\n","or `list(var = 'x', outlier = c('lo', 'uo')`", sep = "")
  
  if (!is.list(outlier)) {      
    stop("argument `outlier` must be a list with var and outlier names\n",
         error_ex, sep = "")
  }
  if (!all(names(outlier) %in% c("var", "outlier"))) {
    stop("only names such as var and outlier can be used to create the list\n",
         error_ex, sep = "")
  }
  if (!is.list(outlier$outlier)) {    
    if (!all(names(outlier$outlier) %in% c("x", "y"))) {
      stop("only names such as x and y can be used to create the outlier list\n",
           error_ex, sep = "")
    }
  }
  
  if (outlier$var == "x") {
    f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier)
    
  } else if (outlier$var == "y") {
    f_tbl <- filter_outlier(df, {{ var_y }}, outlier$outlier)
    
  } else if (outlier$var == "both") {
    
    if (typeof(outlier$outlier) == "character") {
      f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier) %>% 
        filter_outlier({{ var_y }}, outlier$outlier)
      
    } else if (typeof(outlier$outlier) == "list") {
      f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier$x) %>% 
        filter_outlier({{ var_y }}, outlier$outlier$y)
      
    } else {
      stop("a vector of max 2 or a list of max 2 can only be assgined to outlier")
    }
    
  }
  return(f_tbl)
}



#' Numeric Descriptive Summary
#'
#' @param df data.frame.
#' @param var numeric variable from the data 'df'.
#' @param groups group settings for summarise function.
#' @param pivot TRUE to make the output in a long format, FALSE for wide format.
#'
#' @example numeric_summary(data, sales_per_visit)
#' 
numeric_summary <- function(df, var, 
                            include_count = TRUE, groups = "drop_last", 
                            pivot = FALSE) {
 
  f_tbl <- df |> 
    dplyr::summarise(count   = n(),
                     minimum = min({{ var }}),
                     Q25     = quantile({{ var }}, 0.25),
                     mean    = mean({{ var }}),
                     median  = median({{ var }}),
                     Q75     = quantile({{ var }}, 0.75),
                     maximum = max({{ var }}),  
                     sum     = sum({{ var }}), .groups = groups)
  
  f_tbl <- if (include_count) f_tbl else select(f_tbl, -count)
  
  if (pivot) {
    f_tbl <- f_tbl |> 
      tidyr::pivot_longer(cols = count:sum, 
                          names_to  = "statistics", 
                          values_to = "value")
    return(f_tbl)
    
  } else {
    return(f_tbl)
  }
}



#' @description clean string labels.
#'
#' @param label string. labels to rename.
#'
#' @example axis_label("sales_per_visit")
#' 
axis_label <- function(label) { 
  label |> 
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
}



plt_chr_count <- function(df, chr_var) {
  ggplot(df, aes(x = {{chr_var}})) +
    geom_bar() +
    theme_minimal() +
    ggtitle(label = ensym(chr_var)) +
    ylab(" ")
}



#' Numeric Distribution
#'
#' @param df data.frame
#' @param num_var numeric: a variable to summarise from the data 'df'.
#' @param colors vector: colors for the plot.
#' @param outlier vector: filter out outliers in the data. any of "strong lower", "lower",
#' :upper", "strong_upper". 
#' @param typ string: type of output plt - plot, tbl - table.
#' 
#' @return numeric descriptive summary tibble.
#' 
#' @example numeric_distribution(train, sales_per_visit)
#'
numeric_distribution <- function(df, num_var, 
                                 colors, outlier, binw = 30, 
                                 p_tl = NULL, type = "plot") {
  
  if (!missing(outlier)) {
    f_df <- filter_outlier(df, {{ num_var }}, outlier)
    
  } else {
    f_df <- df
  }
  
  if (missing(colors)) {
    colors <- c("#D3AB9E", "#EBD8D0")
    
  } else {
    colors
  }
  
  if (!is.null(p_tl)) {
    p_l <- p_tl
    
  } else {
    p_l <- axis_label(dplyr::ensym(num_var))
  }
  
  txt_color <- "#878787"
  bg_color  <- "#FEFEFF"
  
  f_plt_1 <- f_df |> 
    ggplot2::ggplot(ggplot2::aes(y = {{ num_var }})) +
    ggplot2::geom_boxplot(color = colors[[1]], fill = colors[[2]]) +
    ggplot2::labs(y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(color = txt_color),
                   plot.background = ggplot2::element_rect(color = bg_color,
                                                           fill  = bg_color))
  f_plt_2 <- f_df |> 
    ggplot2::ggplot(aes({{ num_var }})) +
    ggplot2::geom_histogram(binwidth = binw, fill = colors[[1]]) +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = "", y = "Count") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_text(color = txt_color),
                   axis.title.y = ggplot2::element_text(color = txt_color),
                   plot.background = ggplot2::element_rect(color = bg_color,
                                                           fill  = bg_color))
  
  f_plt <- (f_plt_1+f_plt_2) +
    patchwork::plot_layout(widths = c(1.2, 1.8)) +
    patchwork::plot_annotation(title = paste("Distribution Of", p_l),
                               theme = ggplot2::theme(plot.title = ggplot2::element_text(color = txt_color)))
  
  switch(type,
         table = numeric_summary(f_df, {{ num_var }}, pivot = TRUE),
         plot  = f_plt)
}




#' Numeric variable relationship plot matrix.
#'
#' @param df data.frame
#' @param x_var numeric variable to draw other numeric variables in the data with.
#'
#' @return ggplot2 graph.
#' @export
#'
#' @examples relationship_plot(train, "sales_per_visit")
#' 
relationship_plot <- function(df, x_var) {
  bg_color <- "#FFFBFF"
  
  empty_plt <- ggplot2::ggplot() + 
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"))
  
  col_names <- names(df)
  
  plt <- lapply(
    col_names[col_names != x_var],
    function(.y) {
      
      f_plt <- df |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .data[[.y]])) +
        ggplot2::geom_point(alpha = 0.5, color = "#D3AB9E") +
        ggplot2::labs(x = NULL, y = NULL, subtitle = axis_label(.y)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.text.x  = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.y  = ggplot2::element_text(colour = "#8F8F8F"),
                       plot.subtitle = ggplot2::element_text(colour = "#696969"),
                       plot.title = ggplot2::element_text(color = "#878787"),
                       plot.background = ggplot2::element_rect(fill = bg_color,
                                                               color = bg_color)) 
    })
  
  (plt[[1]] + plt[[2]]) / (plt[[3]] + plt[[4]]) / (plt[[5]] + empty_plt) +
    patchwork::plot_annotation(title = axis_label(x_var),
                               theme = ggplot2::theme(plot.title = ggplot2::element_text(color = "#4F4F4F")))
}



#' Segment data using rfm value.
#'
#' @param column numeric variable to use in separating the data.
#' @param label labels to be given to each separate group.
#'
#' @return a vector.
#' @export
#'
#' @examples mutate(df, r = rfm_cut(sales_per_visit, seq(4, 1)))
#' 
rfm_cut <- function(column, label) {
  dplyr::case_when(
    dplyr::between(column, -1, quantile(column, 0.25)) ~ label[1], 
    dplyr::between(column, quantile(column, 0.25), quantile(column, 0.5)) ~ label[2],
    dplyr::between(column, quantile(column, 0.5), quantile(column, 0.75)) ~ label[3],
    dplyr::between(column, quantile(column, 0.75), max(column)) ~ label[4]
  )
}



#' Add customer segment variable.
#'
#' @param df web data set.
#'
#' @return a tibble with new variables 'rfm' and 'customer_segment'.
#' @export 
#'
#' @examples clean_add_variable(df = train)
#' 
clean_add_variable <- function(df) {
  df |>
    janitor::clean_names() |>
    dplyr::mutate(
      r_score = rfm_cut(days_since_purchase, seq(4, 1)),
      f_score = rfm_cut(purchase_visits, 1:4),
      m_score = rfm_cut(sales_per_visit, 1:4),
      rfm_score = r_score + f_score + m_score,
      customer_segment = dplyr::case_when(
        rfm_score >= 3 & rfm_score < 6 ~ "Bottom Segment",
        rfm_score >= 6 & rfm_score < 9 ~ "Middle Segment",
        rfm_score >= 9 ~ "Top Segment",
        TRUE ~ "unknown Segment"
      )
    ) |>
    dplyr::select(-c(r_score, f_score, m_score))
}



#' Segment summary for all numerical variable in the data.
#'
#' @param df data.frame
#'
#' @return a reactable HTLM widget.
#' @export
#'
#' @examples customer_segment_summary_table(df = train)
#' 
customer_segment_summary_table <- function(df) {
  c_names <- names(df)
  c_names <- c_names[!c_names %in% c("rfm_score", "customer_segment")]
  
  top_lvl <- dplyr::tibble(Variable = c_names) |>
    dplyr::mutate(Variable = axis_label(Variable))
  
  sub_lvl <- map_dfr(
    c_names,
    function(.x) {
      df |>
        dplyr::group_by(customer_segment) |>
        numeric_summary(.data[[.x]]) |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 2))) |>
        dplyr::mutate(Variable = axis_label(.x)) |>
        dplyr::select(-count) |>
        dplyr::rename_with(axis_label)
    }
  ) 
  
  reactable::reactable(
    data = top_lvl,
    
    theme = reactableTheme(
      color = "#6E6E6E",
      cellPadding = 12,
      headerStyle = list(fontSize = "25px",
                         color = "#5C5C5C"),
      backgroundColor = tbl_bg_color
    ),
    bordered = TRUE,
    
    details = function(index) {
      sub_level <- sub_lvl[sub_lvl$Variable == top_lvl$Variable[index], ]
  
      d_pre <- if (top_lvl$Variable[index] == "Sales Per Visit") "$" else NULL
      
      reactable::reactable(
        data = sub_level,
        
        defaultColDef = reactable::colDef(format = reactable::colFormat(separators = TRUE,
                                                                        prefix = d_pre),
                                          minWidth = 85,
                                          style = list(color = "#7A7A7A")),
        theme = reactable::reactableTheme(color = "#6E6E6E",
                                          backgroundColor = tbl_bg_color),
        columns = list(
          `Customer Segment` = reactable::colDef(
            minWidth = 150,
            style = list(borderRight = "2px solid #DDDDDD",
                         color = "#616161"),
            format = reactable::colFormat(prefix = NULL)
          ),
          Sum = reactable::colDef(minWidth = 100),
          Variable = reactable::colDef(show = FALSE)
        )
      )
    }
  )
}



#' HTML widget table.
#'
#' @param df a data.frame.
#' @param model the type of model.
#'
#' @return reactable HTML widget
#' @export
#'
#' @examples cross_valid_tibble(fitted_model_cv_table, "dt")
#' 
cross_valid_tibble <- function(df, model) {
  
  ed_cols <- list(
    mean = reactable::colDef(format = reactable::colFormat(digits = 3)),
    std_err = reactable::colDef(format = reactable::colFormat(digits = 4))
  )
  
  if (model == "dt") {
    ed_cols$cost_complexity <- reactable::colDef(
      format = reactable::colFormat(digits = 5)
      )
    
  } else if (model == "rf") {
    ed_cols <- ed_cols
    
  } else if (model == "default") {
    ed_cols <- ed_cols
  }
  
  reactable::reactable(
    data = df,
    columns = ed_cols,
    bordered = TRUE,
    defaultPageSize = 5,
    theme = reactable::reactableTheme(color = "#8A8A8A",
                                      headerStyle = list(color = "#777777",
                                                         fontSize = "17px"))
  ) 
}



#' HTML widget table.
#'
#' @param df a data.frame
#'
#' @return reactable HTML widget
#' @export
#'
#' @examples  tune_tibble(fitted_tunned_model_table)
#' 
tune_tibble <- function(df) {
  reactable::reactable(
    data = df,
    
    columns = list(
      cost_complexity = reactable::colDef(format = reactable::colFormat(digits = 5)),
      mean = reactable::colDef(format = reactable::colFormat(digits = 3)),
      std_err = reactable::colDef(format = reactable::colFormat(digits = 4))
    ),
    
    bordered = TRUE,
    
    theme = reactable::reactableTheme(color = "#8A8A8A",
                                      headerStyle = list(color = "#777777",
                                                         fontSize = "17px"))
  ) 
}