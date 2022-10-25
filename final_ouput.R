# libraries ====================================================================
library(tidyverse)
library(tidymodels)
library(skimr)
library(reactable)
library(vip)
library(vetiver)

# Reason for the analysis ======================================================
# the .. company is trying to know the potential amount each customer will spend 
# in the future given the amount of sales it has achieved over the past from it 
# black Friday deals so as to can make provisions for further attractive deals 
# for both returning and new customers to improve revenue.


# new ------------------|--------------|
# Company A wants to predict future sales on its clothing merchandise, using some
# of the activities of its customers leading up to a purchase or not. 
# The Frame of the problem =====================================================
# Given that the problem requires the prediction of sales per visit
# and sales is a continuous numeric data type and also the past records of sales
# is available to us, we will be using a supervised regression model to predict
# future sales.
# Performance Measure ==========================================================
# Root Mean Square Error (RMSE) which is a typical performance measure for
# regression problems among others will be use in measuring how well the model
# performed. NOTE that the RMSE gives an [idea] of how much error the model 
# typically makes in its predictions, with a higher [weight] for large errors.

# New ------------------|------------------|
# Model performance will be measured using both Root Mean Square Error (RMSE)
# and Mean Absolute Error (MAE). NOTE that both RMSE and MAE gives an [idea] of
# how much error the model typically makes in its predictions, with a higher
# [weight] for large errors.

# Data =========================================================================
train <- read_delim("data/clothing_store_PCA_training", delim = ",")

test <- read_delim("data/clothing_store_PCA_test", delim = ",")



c_test_df <- read_delim("C:/Users/AYOMIDE/Documents/DataG/s data/clothing_store_PCA_test",
                        delim = ",")

c_train_df <- read_delim("C:/Users/AYOMIDE/Documents/DataG/s data/clothing_store_PCA_training",
                        delim = ",")


# c_df <- lapply(c("clothing_store_PCA_training", "clothing_store_PCA_test"),
#                function(.x) {
#                  read_delim(glue::glue("C:/Users/AYOMIDE/Documents/DataG/s data/{.x}"),
#                             delim = ",")
#                })

c_df <- bind_rows(c_df[[1]], c_df[[2]]) |>
  janitor::clean_names()



train <- clean_add_variable(train) 

# Dictionary ===================================================================
# days since purchase :: number of days that have pasted after a purchase was made.
# Purchase visits :: number of visit where a purchase was made.
# days in file :: number of days such customer have been with the firm.
# days between purchases :: average number of days between purchases.
# diff_items_purchased :: the total number of unique cloths items purchased.
# sales per visit :: average sales per customer visit.

# additional columns to create ::
# segment days since purchase to recent, not recent, sleeping , gone ...
# CLV with days in file
# large purchaser, small purchaser for purchase visit.



# Data Inspection ==============================================================
glimpse(c_df) 

skimr::skim(c_df)

# There are no missing values in the data (n_missing column) and the average
# sales per visit is 113.59 which is mostly influenced by the outliers in the data
# (inline histogram & p50).




# Other Useful variables =======================================================
# RFM score and customer segment variables will be added to the training data 
# which will help in improving the model prediction.

# c_df <- c_df |>
#   mutate(r_score = rfm_cut(days_since_purchase, seq(4, 1)),
#          f_score = rfm_cut(purchase_visits, 1:4),
#          m_score = rfm_cut(sales_per_visit, 1:4),
#          rfm_score = r_score + f_score + m_score,
#          customer_segment = case_when(rfm_score >= 3 & rfm_score < 6 ~ "Low Value Customers",
#                                       rfm_score >= 6 & rfm_score < 9 ~ "Moderate Value Customers",
#                                       rfm_score >= 9 ~ "High Value Customers",
#                                       TRUE ~ "unknown Customers")) |>
#   select(-c(r_score, f_score, m_score))

# Data Exploration =============================================================
# Distributions --------------------------------------------------------|
# NOTE THIS SHOULD BE A PANEL
numeric_distribution(train, sales_per_visit, type = "plot") 
# majority of customers spent less than 500 on an average for all visits and there
# is a huge outlier close to 2,000.

# removing outlier
numeric_distribution(train, sales_per_visit, type = "plot",
                         outlier = c("lower", "upper")) 




numeric_distribution(c_df, days_since_purchase, type = "plot",
                         p_tl = "Days Since Last Purchase")
# the right skewed plot shows that more customers have made purchases recently
# with a minimum of 1 day and an average of 127. days of course it was influenced
# by some large outliers by customers who have not made any purchase within the
# last 11 months.



numeric_distribution(c_df, purchase_visits, type = "plot",
                         p_tl = "Number Of Visits With At least A Purchase")
# the count of days when a visit ended in a purchase shows that majority on an
# average made a purchase at least 5 times on different visits.


numeric_distribution(c_df, days_on_file, type = "plot",
                         p_tl = "Total Number Of Days Since Enrollment")
# there is a different twist to the number of days customers have spent with the
# business as the plot show that majority have spent more time with the business
# with a maximum of 717 days (more that 2 years) and an average of 437 days.


numeric_distribution(c_df, diff_items_purchased, type = "plot",
                         p_tl = "Total Number Of Unique Items Purchased") 
# customers have purchased a median of 9 unique cloth items from the business
# and a maximum of 743 different cloth items [suspecting a bulk purchase here.]



# Relationship --------------------------------------------------------|
s_cdf <- slice(c_df, 1:300)

relationship_plot(s_cdf, "sales_per_visit")
relationship_plot(s_cdf, "purchase_visits")
relationship_plot(s_cdf, "diff_items_purchased")
relationship_plot(c_df, "days_since_purchase")



# correlation ----------------------------------------------------------|
c_df |> 
  rename_with(axis_label) |>
  select(where(is.numeric)) |>
  cor() |>
  reshape2::melt() |> 
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(height = 0.8, width = 0.8) +
  geom_text(aes(label = round(value, 2)), size = 3, color = "#000000") +
  scale_fill_gradient2(low = "#E39774", mid = "#EBD8D0", high = "#8B786D") +
  theme_minimal() +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 8, 
                                   margin = margin(-3, 0, 0, 0),
                                   angle = 25, 
                                   hjust = 1),
        axis.text.y = element_text(size = 8, 
                                   margin = margin(0, -3, 0, 0)),
        panel.grid.major = element_blank())


# Number of unique items purchased is highly correlated with purchase visits
# also days between purchase is moderately correlated with number of days since
# purchase.


# split data ===================================================================
# the data will be divided into a training set which holds 75% of the total data
# and a test set that will contain the remaining the split will be done using
# a stratified sampling based on the newly created customer segment so that each
# set will have a random sample close to that of the population (the entire data set)

# set.seed(1211)
# 
# c_split <- initial_split(c_df, prop = 0.7, strata = customer_segment)
# c_train <- training(c_split)
# c_test  <- testing(c_split)


# customer segment exploration =================================================
train |>
  count(customer_segment, sort = TRUE, name = "count") |>
  mutate(percentage = round(proportions(count)*100, 2)) |>
  
  ggplot(aes(x = fct_rev(fct_reorder(customer_segment, count)), y = count)) +
  geom_col(fill = "#D3AB9E") +
  geom_text(aes(label = glue::glue("{percentage}%"), vjust = 2),
            color = "#4A4A4A") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = NULL) +
  ggtitle("Number of Customer In Each Segment") +
  theme_minimal() +
  theme(plot.title = element_text(color = "#888888"),
        axis.text = element_text(color = "#919191"),
        plot.background = element_rect(fill = "#FFFBFF",
                                       color = "#FFFBFF"))



customer_segment_summary_table(c_df)

# recipe =======================================================================
# Feature Scaling will be performed in order to transform numerical attributes
# to be on similar scales.

# Since most Machine Learning algorithms prefer to work with numbers, we will
# convert the customer segment categories from text to numbers.

# ------------------------------------------------------------------------->>
# c_res <- recipe(sales_per_visit ~ ., data = c_train) |>
#   step_normalize(all_numeric_predictors()) |>
#   step_dummy(all_nominal_predictors())

# -------------------------------------------------------------------------->>
c_res <- recipe(sales_per_visit ~ ., data = train) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())


# Model ========================================================================
# Given that the outcome is a continuous data type, the kind of model that will
# be used will be dealing with a regression problem. I will start with three models
# which are linear regression, decision tree and a random forest model.

lr_mdl <- linear_reg() |>
  set_engine("lm")


dt_mdl <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

rf_mdl <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression")


# workflow =====================================================================
# Creating a workflow pipeline which combines both feature engineering and
# the model specification together. this will be done for each model.

lr_wf <- workflow() |>
  add_recipe(c_res) |>
  add_model(lr_mdl)

dt_wf <- workflow() |>
  add_recipe(c_res) |>
  add_model(dt_mdl)

rf_wf <- workflow() |>
  add_recipe(c_res) |>
  add_model(rf_mdl)


# cross validation =============================================================
# in order to better evaluate the model performance the training set will be split
# further into smaller training set and a validation set, the models will then be
# trained against the smaller training set and evaluate them against the validation set.
# note that there will be 10 separate folds (test set & validation set)
set.seed(1122)
# c_folds <- vfold_cv(c_train, v = 10, strata = "customer_segment")

c_folds <- vfold_cv(train, v = 10, strata = "customer_segment")


# Fit ==========================================================================
doParallel::registerDoParallel()
lr_fit <- lr_wf |>
  fit_resamples(resamples = c_folds, 
                metrics = metric_set(rmse, rsq, mae),
                control = control_resamples(save_pred = TRUE, verbose = FALSE))




doParallel::registerDoParallel()
dt_fit <- dt_wf |>
  fit_resamples(resamples = c_folds, 
                metrics = metric_set(rmse, rsq, mae),
                control = control_resamples(save_pred = TRUE, verbose = FALSE))




doParallel::registerDoParallel()
rf_fit <- rf_wf |>
  fit_resamples(resamples = c_folds, 
                metrics = metric_set(rmse, rsq, mae),
                control = control_resamples(save_pred = TRUE, verbose = FALSE))




# Collecting model metrics -----------------------------------------------------
collect_metrics(lr_fit) |> cross_valid_tibble("default")
collect_metrics(dt_fit) |> cross_valid_tibble("default")
collect_metrics(rf_fit) |> cross_valid_tibble("default")


# Evaluating the performance of the three models we can see that the random forest
# made the least error when predicting sales per visit with RMSE of 46.0 and
# MAE of 22.2 way better than decision tree with RMSE of 58.1 and MAE of 34.8
# linear regression performed worst of all three models so i will drop it as
# further improve the model.

# hyper-parameter tuning -------------------------------------------------------
# The model performance can be improved further by feeding the best parameter to
# the model. A grid search will be used to save time and reduce errors.

# Creating a new tunable model for both decision tree and random forest model.

# Decision tree ---------------------------------------------------|
# Experimenting on three decision tree hyperparameters which are::
# cost complexity :- the cost parameter (cp) used by CART model
# tree depth :- The maximum depth of the decision tree.
# min n :- The minimum number of data points required for a node to be split
# further

dt_t_mdl <- decision_tree(cost_complexity = tune(),
                          tree_depth = tune(),
                          min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

# Update workflow model
dt_wf <- update_model(dt_wf, dt_t_mdl)


set.seed(1221)
doParallel::registerDoParallel()
dt_tune <- tune_grid(dt_wf,
                     resamples = c_folds,
                     grid = 10,
                     metrics = metric_set(mae))

collect_metrics(dt_tune) |> cross_valid_tibble("dt")

show_best(dt_tune, metric = "mae")

autoplot(dt_tune)

select_best(dt_tune, metric = "mae")
# Based on the average MAE of all 10 split there is an increase in the model
# performance which have reduced the error from 33.4 to 20.1. let see how the
# random forest model performs.

# Random forest --------------------------------------------------------|
# Experimenting on two random forest hyperparameters which are::
# mtry :- Number of predictors that will be randomly sampled at each split when
# creating the tree model.
# min_n :- The minimum number of data points required for a node to be split
# further

rf_t_mdl <- rand_forest(mtry = tune(),
                        trees = 1000,
                        min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

# Update workflow model
rf_wf <- update_model(rf_wf, rf_t_mdl)


set.seed(1221)
doParallel::registerDoParallel()
rf_tune <- tune_grid(rf_wf,
                     resamples = c_folds,
                     grid = 5,
                     metrics = metric_set(mae))

collect_metrics(rf_tune) |> cross_valid_tibble("rf")

show_best(rf_tune, metric = "mae")

autoplot(rf_tune)

select_best(rf_tune, metric = "mae")
# the random forest model have the best performance so far, with the lowest error
# of 20.4 on and average for all 10 split.

# feature importance ===========================================================

imp_spec <- rf_mdl |>
  finalize_model(select_best(rf_tune, metric = "mae")) |>
  set_engine("ranger", importance = "permutation")

workflow() |>
  add_recipe(c_res) |>
  add_model(imp_spec) |>
  fit(train) |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(alpha = 0.9, fill = "#D3AB9E")) +
  ggtitle("Feature Importance") +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#888888"),
        axis.title.x = element_text(color = "#8F8F8F"),
        axis.text = element_text(color = "#8F8F8F"))




f <- workflow() |>
  add_recipe(c_res) |>
  add_model(imp_spec) |>
  fit(train) 

f |> extract_fit_parsnip()  |>
  vip(aesthetics = list(alpha = 0.9, fill = "#D3AB9E")) +
  ggtitle("Feature Importance") +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#888888"),
        axis.title.x = element_text(color = "#8F8F8F"),
        axis.text = element_text(color = "#8F8F8F"))


# finalize workflow ============================================================
# going forward i will use the random forest to predict sales per visit on the
# test set to see it the model generalize well enough.

final_rf <- rf_wf |>
  finalize_workflow(select_best(rf_tune, metric = "mae"))

# last fit ---------------------------------------------------------------------
# c_fit <- last_fit(final_rf, c_split, metrics = metric_set(mae))

test <- clean_add_variable(test)

# c_fit <- last_fit(final_rf, c_split, metrics = metric_set(mae))

fit_wf <- fit(final_rf, train) 

sales_prediction <- predict(fit_wf, test)

f <- sales_prediction |>
  mutate(actual = test$sales_per_visit)

mae(f, truth = actual, estimate = .pred)

# evaluation -------------------------------------------------------------------
# Checking metric
collect_metrics(c_fit)



# Collecting Predictions
pred <- collect_predictions(c_fit)

# Table Summary -----------------------------------------------------|
# numeric_summary(pred, .pred, FALSE) |>
#   mutate(variable = "Prediction") |>
#   bind_rows(numeric_summary(pred, sales_per_visit, FALSE) |>
#             mutate(variable = "Actual")) |>
#   relocate(variable, .before = 1) |>
#   mutate(across(where(is.numeric), ~round(.x, 2))) |>
#   rename_with(axis_label) |>
#   reactable(
#     theme = reactableTheme(color = "#6E6E6E"),
#     defaultColDef = colDef(format = colFormat(separators = TRUE,
#                                               prefix = "$")),
#     columns = list(
#       Variable = colDef(style = list(color = "#636363",
#                                      fontWeight = "bold"),
#                         format = colFormat(prefix = NULL))
#     )
#   )

numeric_summary(f, .pred, FALSE) |>
  mutate(variable = "Prediction") |>
  bind_rows(numeric_summary(f, actual, FALSE) |> mutate(variable = "Actual")) |>
  relocate(variable, .before = 1) |>
  mutate(across(where(is.numeric), ~round(.x, 2))) |>
  rename_with(axis_label) |>
  
  reactable(
    theme = reactableTheme(color = "#919191",
                           headerStyle = list(color = "#858585")),
    defaultColDef = colDef(format = colFormat(separators = TRUE,
                                              prefix = "$")),
    columns = list(
      Variable = colDef(style = list(color = "#777777",
                                     fontWeight = "bold"),
                        format = colFormat(prefix = NULL))
    )
  )

# Plot --------------------------------------------------------------|
# collect_predictions(c_fit) |>
#   ggplot(aes(sales_per_visit, .pred)) +
#   geom_abline(lty = 2, color = "grey69") +
#   geom_point(color = "#858585") +
#   scale_x_continuous(labels = scales::label_dollar()) +
#   scale_y_continuous(labels = scales::label_dollar()) +
#   theme_minimal() +
#   labs(x = "Actual", y = "Prediction", 
#        title = "Actual Vs Predicted Sales Per Visit") +
#   theme(axis.title = element_text(color = "#5E5E5E"),
#         plot.title = element_text(color = "#5E5E5E"))

# collect_predictions(c_fit) |>
f |>
  ggplot(aes(actual, .pred)) +
  geom_abline(lty = 2, color = "#D3AB9E") +
  geom_point(color = "#D3AB9E") +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(labels = scales::label_dollar()) +
  theme_minimal() +
  labs(x = "Actual", y = "Prediction", 
       title = "Actual Vs Predicted Sales Per Visit") +
  theme(axis.title = element_text(color = "#5E5E5E"),
        plot.title = element_text(color = "#5E5E5E"))

# The final model shows smaller error for sales per visit below $300 but a higher
# error for sales above that amount. also the model could not predict sales
# above $716.9




# store model ------------------------------------------------------------------
mdl_v <- vetiver::vetiver_model(model = fit_wf, model_name = "sales_random_forest")

model_board <- pins::board_folder("model_workflow", versioned = TRUE)

vetiver::vetiver_pin_write(model_board, mdl_v)





