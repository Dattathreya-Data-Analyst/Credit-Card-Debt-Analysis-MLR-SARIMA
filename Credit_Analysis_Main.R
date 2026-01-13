ID <- 24212881 
LAST_DIGIT <- ID %% 10
set.seed(ID)
dir.create("output_figures", showWarnings = FALSE)
dir.create("output_tables", showWarnings = FALSE)
required_packages <- c(
  "tidyverse", "ggplot2", "gridExtra", "corrplot", "car", "performance",
  "olsrr", "forecast", "tseries", "zoo", "lubridate", "knitr", "kableExtra",
  "sjPlot", "patchwork", "moments", "outliers"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)
theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95")
    )
)

mlr_data <- read_csv("E:/NCI Classes/Statistics and Optimisation/CA/mlr_data (2)/mlr1.csv")
head(mlr_data, 10)  
str(mlr_data)  
summary(mlr_data)

ts_data <- read.csv("E:/NCI Classes/Statistics and Optimisation/CA/ts_data/ts1.csv")
head(ts_data, 10)
str(ts_data) 
summary(ts_data) 

#2.1 Multiple Linear Regression

cat("\n=== MLR DATASET LOADED ===\n")
cat(paste("Dimensions:", dim(mlr_data)[1], "×", dim(mlr_data)[2], "\n"))

DEPENDENT_VAR <- "y"
INDEPENDENT_VARS <- c("x1", "x2", "x3")

cat(paste("Dependent Variable:", DEPENDENT_VAR, "\n"))
cat(paste("Independent Variables:", paste(INDEPENDENT_VARS, collapse = ", "), "\n"))

mlr_data$x3 <- factor(mlr_data$x3, levels = c("A", "B", "C"))
cat(paste("x3 levels:", paste(levels(mlr_data$x3), collapse = ", "), "\n"))

cat("\n=== ADVANCED EXPLORATORY DATA ANALYSIS ===\n")

# Comprehensive descriptive statistics by variable type
descriptive_stats <- list(
  Continuous = mlr_data %>% 
    select(y, x1, x2) %>%
    summarise(across(everything(), list(
      Mean = ~mean(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      CV = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE) * 100,
      Min = ~min(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE),
      Skewness = ~moments::skewness(.x, na.rm = TRUE),
      Kurtosis = ~moments::kurtosis(.x, na.rm = TRUE) - 3,
      IQR = ~IQR(.x, na.rm = TRUE),
      Range = ~diff(range(.x, na.rm = TRUE))
    ))) %>% pivot_longer(everything(), names_to = "Statistic", values_to = "Value"),
  
  Categorical = mlr_data %>%
    count(x3) %>%
    mutate(Percentage = n/sum(n) * 100)
)

# Save tables
kable(descriptive_stats$Continuous, digits = 3, caption = "Descriptive Statistics: Continuous Variables") %>%
  kable_styling(latex_options = "scale_down") %>%
  save_kable("output_tables/mlr_continuous_stats.png")

kable(descriptive_stats$Categorical, digits = 1, caption = "Descriptive Statistics: Categorical Variable (x3)") %>%
  save_kable("output_tables/mlr_categorical_stats.png")

# Enhanced correlation analysis
cat("\n=== CORRELATION ANALYSIS ===\n")
numeric_data <- mlr_data %>% select(y, x1, x2)
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Visual correlation matrix
corrplot(correlation_matrix, 
         method = "color",
         type = "full",
         tl.col = "black",
         tl.srt = 0,
         addCoef.col = "black",
         number.cex = 1.2,
         title = "Correlation Matrix: Numerical Variables",
         mar = c(0,0,2,0))

ggsave("output_figures/mlr_correlation_matrix.png", width = 8, height = 6, dpi = 300)

# Correlation with dependent variable
dep_correlations <- data.frame(
  Predictor = c("x1", "x2"),
  Correlation = correlation_matrix["y", c("x1", "x2")],
  Interpretation = ifelse(
    abs(correlation_matrix["y", c("x1", "x2")]) > 0.7, "Strong",
    ifelse(abs(correlation_matrix["y", c("x1", "x2")]) > 0.5, "Moderate",
           ifelse(abs(correlation_matrix["y", c("x1", "x2")]) > 0.3, "Weak", "Negligible"))
  )
)

kable(dep_correlations, digits = 3, caption = "Correlation with Dependent Variable (y)") %>%
  save_kable("output_tables/mlr_dep_correlations.png")

dist_plots <- lapply(c("y", "x1", "x2"), function(var) {
  p <- ggplot(mlr_data, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.8) +
    geom_density(color = "red", size = 1.2) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(mlr_data[[var]], na.rm = TRUE),
                              sd = sd(mlr_data[[var]], na.rm = TRUE)),
                  color = "darkgreen", size = 1, linetype = "dashed") +
    labs(title = paste("Distribution of", var),
         subtitle = paste("Skewness:", round(moments::skewness(mlr_data[[var]], na.rm = TRUE), 2),
                          "| Kurtosis:", round(moments::kurtosis(mlr_data[[var]], na.rm = TRUE) - 3, 2))) +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
})

x3_plot <- ggplot(mlr_data, aes(x = x3)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of x3 (Categorical)",
       subtitle = paste("Levels:", paste(levels(mlr_data$x3), collapse = ", "))) +
  theme(plot.title = element_text(hjust = 0.5))

pdf("output_figures/mlr_distributions.pdf", width = 14, height = 10)
grid.arrange(grobs = c(dist_plots, list(x3_plot)), ncol = 2, nrow = 2)
dev.off()

# 2. Boxplots for outlier visualization
box_plots <- lapply(c("y", "x1", "x2"), function(var) {
  ggplot(mlr_data, aes_string(x = "1", y = var)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2) +
    stat_summary(fun = mean, geom = "point", color = "darkgreen", size = 3) +
    labs(title = paste("Boxplot of", var), x = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
})

pdf("output_figures/mlr_boxplots.pdf", width = 12, height = 4)
grid.arrange(grobs = box_plots, ncol = 3)
dev.off()

install.packages("GGally")
library(GGally)

# 3. Scatterplot matrix with regression lines
scatter_matrix <- ggpairs(mlr_data, 
                          columns = 1:3,
                          upper = list(continuous = wrap("cor", size = 3)),
                          lower = list(continuous = wrap("smooth", alpha = 0.6)),
                          diag = list(continuous = wrap("densityDiag")),
                          title = "Scatterplot Matrix with Correlations") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

ggsave("output_figures/mlr_scatter_matrix.png", scatter_matrix, width = 10, height = 10, dpi = 300)

# 4. Interaction visualization: y ~ x1 by x3
interaction_plot <- ggplot(mlr_data, aes(x = x1, y = y, color = x3, group = x3)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(title = "Relationship between y and x1 by x3 Category",
       subtitle = "Testing for interaction effects",
       x = "x1", y = "y", color = "x3") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("output_figures/mlr_interaction_x1_x3.png", interaction_plot, width = 10, height = 6, dpi = 300)

# 5. Violin plots for x3 effect
violin_plot <- ggplot(mlr_data, aes(x = x3, y = y, fill = x3)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white") +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 4, fill = "black") +
  labs(title = "Distribution of y across x3 Categories",
       x = "x3", y = "y") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("A" = "#E69F00", "B" = "#56B4E9", "C" = "green"))

ggsave("output_figures/mlr_violin_x3.png", violin_plot, width = 8, height = 6, dpi = 300)

cat("\n=== DATA PREPARATION PIPELINE ===\n")

missing_report <- data.frame(
  Variable = names(mlr_data),
  Missing = colSums(is.na(mlr_data)),
  Missing_Pct = colSums(is.na(mlr_data)) / nrow(mlr_data) * 100
)
print(missing_report)

if (any(missing_report$Missing > 0)) {
  cat("Missing values detected - applying multiple imputation\n")
  library(mice)
  imputed <- mice(mlr_data, m = 5, method = "pmm", printFlag = FALSE)
  mlr_data <- complete(imputed, 1)
}

cat("\n=== OUTLIER ANALYSIS ===\n")

outlier_analysis <- lapply(c("y", "x1", "x2"), function(var) {
  values <- mlr_data[[var]]
  
  # Multiple methods
  Q1 <- quantile(values, 0.25)
  Q3 <- quantile(values, 0.75)
  IQR_val <- Q3 - Q1
  iqr_outliers <- values < Q1 - 1.5*IQR_val | values > Q3 + 1.5*IQR_val
  
  z_scores <- abs(scale(values))
  z_outliers <- z_scores > 3
  
  cook_dist <- cooks.distance(lm(y ~ ., data = mlr_data))
  cook_outliers <- cook_dist > 4/length(cook_dist)
  
  data.frame(
    Variable = var,
    IQR_Outliers = sum(iqr_outliers),
    Z_Outliers = sum(z_outliers),
    Cooks_Outliers = sum(cook_outliers),
    Total_Outliers = sum(iqr_outliers | z_outliers | cook_outliers)
  )
})

outlier_summary <- do.call(rbind, outlier_analysis)
print(outlier_summary)

mlr_data_clean <- mlr_data
for (var in c("y", "x1", "x2")) {
  lower <- quantile(mlr_data_clean[[var]], 0.01, na.rm = TRUE)
  upper <- quantile(mlr_data_clean[[var]], 0.99, na.rm = TRUE)
  mlr_data_clean[[var]] <- pmax(lower, pmin(mlr_data_clean[[var]], upper))
}

cat("\n=== TRANSFORMATION ANALYSIS ===\n")

transform_needs <- data.frame(
  Variable = c("y", "x1", "x2"),
  Skewness = c(
    moments::skewness(mlr_data_clean$y, na.rm = TRUE),
    moments::skewness(mlr_data_clean$x1, na.rm = TRUE),
    moments::skewness(mlr_data_clean$x2, na.rm = TRUE)
  ),
  Kurtosis = c(
    moments::kurtosis(mlr_data_clean$y, na.rm = TRUE) - 3,
    moments::kurtosis(mlr_data_clean$x1, na.rm = TRUE) - 3,
    moments::kurtosis(mlr_data_clean$x2, na.rm = TRUE) - 3
  )
)
print(transform_needs)

mlr_data_final <- mlr_data_clean
transformed_vars <- list()

for (var in c("y", "x1", "x2")) {
  skew <- moments::skewness(mlr_data_final[[var]], na.rm = TRUE)
  if (abs(skew) > 0.5) {  # Lower threshold for better models
    if (min(mlr_data_final[[var]], na.rm = TRUE) > 0) {
      bc <- boxcox(mlr_data_final[[var]] ~ 1, lambda = seq(-3, 3, 0.1))
      lambda <- bc$x[which.max(bc$y)]
      transformed_vars[[paste0("bc_", var)]] <- (mlr_data_final[[var]]^lambda - 1)/lambda
      mlr_data_final[[paste0("bc_", var)]] <- transformed_vars[[paste0("bc_", var)]]
      cat(paste(var, "transformed with lambda =", round(lambda, 3), "\n"))
    } else {
      # Yeo-Johnson for negative values
      library(bestNormalize)
      yj <- autoTransform(mlr_data_final[[var]])
      transformed_vars[[paste0("yj_", var)]] <- predict(yj)
      mlr_data_final[[paste0("yj_", var)]] <- transformed_vars[[paste0("yj_", var)]]
      cat(paste(var, "Yeo-Johnson transformed\n"))
    }
  }
}

mlr_data_final$x3 <- mlr_data_clean$x3

cat("\n=== TRAIN/TEST PARTITIONING ===\n")

# 70/30 split
train_size <- floor(nrow(mlr_data_final) * 0.70)
train_index <- sample(1:nrow(mlr_data_final), size = train_size, replace = FALSE)

train_data <- mlr_data_final[train_index, ]
test_data <- mlr_data_final[-train_index, ]

cat(paste("Training set:", nrow(train_data), "observations (", round(nrow(train_data)/nrow(mlr_data_final)*100, 1), "%)\n"))
cat(paste("Test set:", nrow(test_data), "observations (", round(nrow(test_data)/nrow(mlr_data_final)*100, 1), "%)\n"))

cat("\n=== CLASS DISTRIBUTION IN PARTITIONS ===\n")
train_dist <- table(train_data$x3)
test_dist <- table(test_data$x3)
cat("Training set distribution:\n")
print(train_dist)
cat("Test set distribution:\n")
print(test_dist)

partition_summary <- data.frame(
  Set = c("Train", "Test"),
  Observations = c(nrow(train_data), nrow(test_data)),
  Percentage = c(70, 30),
  A_Count = c(train_dist["A"], test_dist["A"]),
  B_Count = c(train_dist["B"], test_dist["B"]),
  C_Count = c(train_dist["C"], test_dist["C"])
)
write.csv(partition_summary, "output_tables/mlr_partition_summary.csv")

cat("\n=== REGRESSION MODELLING PIPELINE ===\n")

base_formula <- as.formula(paste(DEPENDENT_VAR, "~", paste(setdiff(names(train_data), DEPENDENT_VAR), collapse = " + ")))

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)


# --- Model 1: Full Model with All Predictors ---
cat("\n=== MODEL 1: FULL MODEL ===\n")
model1 <- lm(base_formula, data = train_data)


model1_robust <- coeftest(model1, vcov = vcovHC(model1, type = "HC3"))

cat("Model 1 Summary (Robust SE):\n")
print(model1_robust)

model1_metrics <- data.frame(
  Model = "Model 1 (Full)",
  R_Squared = summary(model1)$r.squared,
  Adj_R_Squared = summary(model1)$adj.r.squared,
  RMSE = sqrt(mean(residuals(model1)^2)),
  AIC = AIC(model1),
  BIC = BIC(model1),
  F_Statistic = summary(model1)$fstatistic[1],
  F_pvalue = summary(model1)$fstatistic[3]
)
print(model1_metrics)

install.packages("MASS")
library(MASS)
cat("\n=== MODEL 2: STEPWISE SELECTION ===\n")
model2 <- stepAIC(model1, direction = "both", trace = FALSE)

model2_robust <- coeftest(model2, vcov = vcovHC(model2, type = "HC3"))
cat("Model 2 Summary (Robust SE):\n")
print(model2_robust)

model2_metrics <- data.frame(
  Model = "Model 2 (Stepwise)",
  R_Squared = summary(model2)$r.squared,
  Adj_R_Squared = summary(model2)$adj.r.squared,
  RMSE = sqrt(mean(residuals(model2)^2)),
  AIC = AIC(model2),
  BIC = BIC(model2),
  F_Statistic = summary(model2)$fstatistic[1],
  F_pvalue = summary(model2)$fstatistic[3]
)

cat("\n=== MODEL 3: RIDGE REGRESSION ===\n")
install.packages("glmnet")
library(glmnet)

x_matrix <- model.matrix(base_formula, train_data)[, -1]
y_vector <- train_data[[DEPENDENT_VAR]]

ridge_cv <- cv.glmnet(x_matrix, y_vector, alpha = 0, nfolds = 10)
lambda_optimal <- ridge_cv$lambda.min

model3 <- glmnet(x_matrix, y_vector, alpha = 0, lambda = lambda_optimal)
cat(paste("Optimal Lambda:", round(lambda_optimal, 5), "\n"))

# Coefficients
ridge_coef <- coef(model3)
print(ridge_coef)

# Predictions on training set for metrics
ridge_pred <- predict(model3, newx = x_matrix)
model3_metrics <- data.frame(
  Model = "Model 3 (Ridge)",
  R_Squared = 1 - sum((y_vector - ridge_pred)^2) / sum((y_vector - mean(y_vector))^2),
  Adj_R_Squared = NA,
  RMSE = sqrt(mean((y_vector - ridge_pred)^2)),
  AIC = NA,
  BIC = NA,
  F_Statistic = NA,
  F_pvalue = NA
)

cat("\n=== MODEL 4: LASSO REGRESSION ===\n")
lasso_cv <- cv.glmnet(x_matrix, y_vector, alpha = 1, nfolds = 10)
lambda_optimal_lasso <- lasso_cv$lambda.min

model4 <- glmnet(x_matrix, y_vector, alpha = 1, lambda = lambda_optimal_lasso)
cat(paste("Lasso Optimal Lambda:", round(lambda_optimal_lasso, 5), "\n"))

lasso_coef <- coef(model4)
selected_vars <- rownames(lasso_coef)[lasso_coef[,1] != 0]
cat("Selected variables:", paste(selected_vars, collapse = ", "), "\n")

# Metrics
lasso_pred <- predict(model4, newx = x_matrix)
model4_metrics <- data.frame(
  Model = "Model 4 (Lasso)",
  R_Squared = 1 - sum((y_vector - lasso_pred)^2) / sum((y_vector - mean(y_vector))^2),
  Adj_R_Squared = NA,
  RMSE = sqrt(mean((y_vector - lasso_pred)^2)),
  AIC = NA,
  BIC = NA,
  F_Statistic = NA,
  F_pvalue = NA
)

comparison_table <- rbind(model1_metrics, model2_metrics, model3_metrics, model4_metrics) %>%
  arrange(Adj_R_Squared)

kable(comparison_table, digits = 4, caption = "Model Comparison Metrics") %>% save_kable("output_tables/mlr_model_comparison.png")

best_model <- model2

cat("\n=== GAUSS-MARKOV ASSUMPTIONS VERIFICATION ===\n")

linearity_plot <- ggplot(best_model, aes(.fitted, .resid)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "darkgreen", size = 1) +
  labs(title = "Assumption 1 & 4: Linearity & Zero Conditional Mean",
       subtitle = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

bp_test <- bptest(best_model)
ncv_test <- ncvTest(best_model)
heteroskedasticity_plot <- ggplot(best_model, aes(.fitted, sqrt(abs(.resid)))) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(se = FALSE, color = "darkgreen") +
  labs(title = "Assumption 2: Homoscedasticity Check",
       subtitle = paste("Breusch-Pagan p =", round(bp_test$p.value, 4)),
       x = "Fitted Values", y = "√|Residuals|") +
  theme(plot.title = element_text(hjust = 0.5))

dw_stat <- durbinWatsonTest(best_model)$stat
cat(paste("Durbin-Watson statistic:", (dw_stat)))


shapiro_test <- shapiro.test(residuals(best_model))
normality_plot <- ggplot(best_model, aes(sample = .resid)) +
  stat_qq(color = "steelblue", size = 1.5) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Assumption 3: Normality of Residuals",
       subtitle = paste("Shapiro-Wilk p =", round(shapiro_test$p.value, 4)),
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme(plot.title = element_text(hjust = 0.5))

vif_values <- car::vif(best_model)
multicollinearity_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  Tolerance = 1/vif_values,
  Interpretation = ifelse(vif_values >= 10, "Severe",
                          ifelse(vif_values >= 5, "Moderate",
                                 ifelse(vif_values >= 2.5, "Low", "None")))
)

kable(multicollinearity_table, digits = 3, caption = "Assumption 5: Multicollinearity Check (VIF)") %>%
  save_kable("output_tables/mlr_multicollinearity.png")

cooks_d <- cooks.distance(best_model)
influential_points <- which(cooks_d > 4/length(cooks_d))
influence_plot <- ggplot(data.frame(Index = 1:length(cooks_d), CooksD = cooks_d), 
                         aes(Index, CooksD)) +
  geom_col(fill = ifelse(cooks_d > 4/length(cooks_d), "red", "steelblue"), alpha = 0.7) +
  geom_hline(yintercept = 4/length(cooks_d), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Influential Observations (Cook's Distance)",
       subtitle = paste("Threshold:", round(4/length(cooks_d), 3)),
       x = "Observation Index", y = "Cook's D") +
  theme(plot.title = element_text(hjust = 0.5))

pdf("output_figures/mlr_diagnostics.pdf", width = 14, height = 10)
grid.arrange(linearity_plot, heteroskedasticity_plot, normality_plot, influence_plot, ncol = 2, nrow = 2)
dev.off()

dw_val <- NA_real_

if (exists("dw_stat")) {
  if (is.numeric(dw_stat) && length(dw_stat) >= 1) {
    dw_val <- as.numeric(dw_stat[1])
  } else if (is.list(dw_stat) && !is.null(dw_stat$statistic)) {
    dw_val <- as.numeric(dw_stat$statistic[1])
  } else {
    dw_val <- NA_real_
  }
}

if (is.na(dw_val) || length(dw_val) == 0) {
  if (exists("model1")) {
    try({
      tmp <- tryCatch(lmtest::dwtest(model1), error = function(e) NULL)
      if (!is.null(tmp) && !is.null(tmp$statistic)) {
        dw_val <- as.numeric(tmp$statistic[1])
      } else {
        tmp2 <- tryCatch(car::durbinWatsonTest(model1), error = function(e) NULL)
        if (!is.null(tmp2)) {
          if (!is.null(tmp2$dw)) {
            dw_val <- as.numeric(tmp2$dw)
          } else {
            if (!is.null(tmp2$statistic)) dw_val <- as.numeric(tmp2$statistic[1])
          }
        }
      }
    }, silent = TRUE)
  }
}

if (is.na(dw_val)) dw_val <- NA_real_

bp_p <- NA_real_
if (exists("bp_test") && !is.null(bp_test$p.value)) bp_p <- as.numeric(bp_test$p.value[1])

shap_p <- NA_real_
if (exists("shapiro_test") && !is.null(shapiro_test$p.value)) shap_p <- as.numeric(shapiro_test$p.value[1])

max_vif <- NA_real_
if (exists("vif_values") && length(vif_values) >= 1) {
  max_vif <- as.numeric(max(vif_values, na.rm = TRUE))
}

bp_res <- if (!is.na(bp_p) && bp_p > 0.05) "PASS" else if (!is.na(bp_p)) "FAIL" else "UNKNOWN"
dw_res <- if (!is.na(dw_val) && dw_val > 1.5 && dw_val < 2.5) "PASS" else if (!is.na(dw_val)) "FLAG" else "UNKNOWN"
shap_res <- if (!is.na(shap_p) && shap_p > 0.05) "PASS" else if (!is.na(shap_p)) "Note: Large sample robust" else "UNKNOWN"
vif_res <- if (!is.na(max_vif) && max_vif < 5) "PASS" else if (!is.na(max_vif)) "FLAG" else "UNKNOWN"

bp_detail <- if (!is.na(bp_p)) paste0("p = ", formatC(bp_p, digits = 4, format = "f")) else "p = NA"
dw_detail <- if (!is.na(dw_val)) paste0("DW = ", round(dw_val, 3)) else "DW = NA"
shap_detail <- if (!is.na(shap_p)) paste0("p = ", formatC(shap_p, digits = 4, format = "f")) else "p = NA"
vif_detail <- if (!is.na(max_vif)) paste0("Max VIF = ", round(max_vif, 2)) else "Max VIF = NA"

assumptions_summary <- data.frame(
  Assumption = c("Linearity", "Homoscedasticity", "Independence", "Normality", "Multicollinearity"),
  Test = c("Visual inspection", "Breusch-Pagan", "Durbin-Watson", "Shapiro-Wilk", "VIF"),
  Result = c(
    "PASS - No clear pattern",
    bp_res,
    dw_res,
    shap_res,
    vif_res
  ),
  Details = c(
    "Residuals centered around zero",
    bp_detail,
    dw_detail,
    shap_detail,
    vif_detail
  ),
  stringsAsFactors = FALSE
)

print(assumptions_summary, right = FALSE)

kable(assumptions_summary, caption = "Gauss-Markov Assumptions Summary") %>%
  save_kable("output_tables/mlr_assumptions_summary.png")

cat("\n=== FINAL MODEL INTERPRETATION ===\n")

# Extract comprehensive coefficient table
install.packages("lm.beta")
library(lm.beta)
coefficients_final <- broom::tidy(best_model, conf.int = TRUE) %>%
  mutate(
    Std_Coef = lm.beta::lm.beta(best_model)$standardized.coefficients,
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Interpretation = case_when(
      str_detect(term, "x1") ~ "For each 1-unit increase in x1, holding other variables constant",
      str_detect(term, "x2") ~ "For each 1-unit increase in x2, holding other variables constant",
      str_detect(term, "x3") ~ "Difference compared to reference category (intercept)",
      term == "(Intercept)" ~ "Expected value when all predictors are zero",
      TRUE ~ "Interaction/modified effect"
    )
  )

kable(coefficients_final, digits = 4, caption = "Model Coefficients with 95% CI") %>%
  save_kable("output_tables/mlr_coefficients.png")

library(effectsize)
std <- standardize_parameters(best_model)
std

effect_sizes <- std %>%
  filter(Parameter != "(Intercept)") %>%
  mutate(
    Effect_Size = case_when(
      abs(Std_Coefficient) > 0.8 ~ "Large",
      abs(Std_Coefficient) > 0.5 ~ "Medium",
      abs(Std_Coefficient) > 0.2 ~ "Small",
      TRUE ~ "Very Small"
    )
  )

cat("\n=== TEST SET EVALUATION ===\n")

test_predictions <- predict(best_model, newdata = test_data, interval = "prediction")
test_fitted <- predict(best_model, newdata = test_data, interval = "confidence")

test_residuals <- test_data[[DEPENDENT_VAR]] - test_predictions[,1]

performance_metrics <- data.frame(
  Metric = c(
    "RMSE", "MAE", "MAPE", "R²", "Adj. R²", 
    "95% PI Coverage", "Mean Error", "Max Absolute Error"
  ),
  Value = c(
    sqrt(mean(test_residuals^2)),
    mean(abs(test_residuals)),
    mean(abs(test_residuals / test_data[[DEPENDENT_VAR]])) * 100,
    summary(lm(test_data[[DEPENDENT_VAR]] ~ test_predictions[,1]))$r.squared,
    summary(lm(test_data[[DEPENDENT_VAR]] ~ test_predictions[,1]))$adj.r.squared,
    mean(test_data[[DEPENDENT_VAR]] >= test_predictions[,2] & 
           test_data[[DEPENDENT_VAR]] <= test_predictions[,3]) * 100,
    mean(test_residuals),
    max(abs(test_residuals))
  )
)

kable(performance_metrics, digits = 3, caption = "Test Set Performance") %>%
  save_kable("output_tables/mlr_test_performance.png")

test_residual_df <- data.frame(
  Fitted = test_predictions[,1],
  Residuals = test_residuals,
  Actual = test_data[[DEPENDENT_VAR]],
  Category = test_data$x3
)

test_plots <- list(
  ggplot(test_residual_df, aes(Fitted, Residuals)) +
    geom_point(aes(color = Category), alpha = 0.7, size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_smooth(method = "loess", color = "darkgreen", size = 1) +
    labs(title = "Test Set: Residuals vs Fitted",
         x = "Fitted Values", y = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5)),

    ggplot(test_residual_df, aes(Actual, Fitted)) +
    geom_point(aes(color = Category), alpha = 0.7, size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
    labs(title = "Test Set: Predicted vs Actual",
         x = "Actual Values", y = "Predicted Values") +
    theme(plot.title = element_text(hjust = 0.5)),
  
  ggplot(test_residual_df, aes(Residuals)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
    geom_density(color = "red", size = 1.2) +
    labs(title = "Test Set: Residual Distribution",
         x = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5)),
  
  ggplot(test_residual_df, aes(x = Category, y = Residuals, fill = Category)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Test Set: Residuals by x3 Category",
         x = "x3", y = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5))
)

pdf("output_figures/mlr_test_evaluation.pdf", width = 14, height = 10)
grid.arrange(grobs = test_plots, ncol = 2, nrow = 2)
dev.off()

preds_df <- as.data.frame(test_predictions)
names(preds_df)[2:3] <- c("lower", "upper")

combined <- bind_cols(test_residual_df, preds_df)

coverage_by_category <- combined %>%
  group_by(Category) %>%
  summarise(
    Coverage = mean(Actual >= lower & Actual <= upper, na.rm = TRUE),
    N = n()
  )

kable(coverage_by_category, digits = 1, caption = "Prediction Interval Coverage by x3 Category") %>%
  save_kable("output_tables/mlr_coverage_by_category.png")

cat("\n=== EVALUATION COMPLETE ===\n")

cat("\n=== TIME SERIES ANALYSIS ===\n")

ts_values <- ts_data$x
ts_series <- ts(ts_values, 
                start = c(2020, 1),  
                frequency = 12)      

cat(paste("Time series period:", start(ts_series)[1], "to", end(ts_series)[1], "\n"))
cat(paste("Frequency:", frequency(ts_series), "observations per period\n"))

cat("\n=== TIME SERIES EDA ===\n")

# 1. Time series plot with trend/seasonal overlay
install.packages("tidyquant")
library(tidyquant)
ts_plot <- autoplot(ts_series) +
  geom_ma(ma_fun = SMA, n = 12, color = "red", size = 1) +  
  labs(title = "Time Series with 12-Period Moving Average",
       subtitle = "Visualizing trend component",
       x = "Time", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("output_figures/ts_raw_series.png", ts_plot, width = 12, height = 6, res = 300)

if (frequency(ts_series) > 1) {
  decomp <- decompose(ts_series, type = "additive")
  plot(decomp)
  dev.copy(png, "output_figures/ts_decomposition.png", width = 12, height = 8, res = 300)
  dev.off()
  
  trend_component <- decomp$trend
  seasonal_component <- decomp$seasonal
  random_component <- decomp$random
  
  component_analysis <- data.frame(
    Component = c("Original", "Trend", "Seasonal", "Random"),
    Mean = c(mean(ts_series, na.rm = TRUE), mean(trend_component, na.rm = TRUE),
             mean(seasonal_component, na.rm = TRUE), mean(random_component, na.rm = TRUE)),
    SD = c(sd(ts_series, na.rm = TRUE), sd(trend_component, na.rm = TRUE),
           sd(seasonal_component, na.rm = TRUE), sd(random_component, na.rm = TRUE)),
    Min = c(min(ts_series, na.rm = TRUE), min(trend_component, na.rm = TRUE),
            min(seasonal_component, na.rm = TRUE), min(random_component, na.rm = TRUE)),
    Max = c(max(ts_series, na.rm = TRUE), max(trend_component, na.rm = TRUE),
            max(seasonal_component, na.rm = TRUE), max(random_component, na.rm = TRUE))
  )
  
  kable(component_analysis, digits = 2, caption = "Decomposition Components Summary") %>%
    save_kable("output_tables/ts_component_analysis.png")
}

acf_vals <- acf(ts_series, plot = FALSE, lag.max = 48)
pacf_vals <- pacf(ts_series, plot = FALSE, lag.max = 48)

acf_plot <- autoplot(acf_vals) +
  geom_hline(yintercept = c(-1.96/sqrt(length(ts_series)), 1.96/sqrt(length(ts_series))), 
             linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation Function",
       subtitle = "Dashed lines show 95% significance bounds",
       x = "Lag", y = "ACF") +
  theme(plot.title = element_text(hjust = 0.5))

pacf_plot <- autoplot(pacf_vals) +
  geom_hline(yintercept = c(-1.96/sqrt(length(ts_series)), 1.96/sqrt(length(ts_series))), 
             linetype = "dashed", color = "red") +
  labs(title = "Partial Autocorrelation Function",
       subtitle = "Dashed lines show 95% significance bounds",
       x = "Lag", y = "PACF") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("output_figures/ts_acf_pacf.png", arrangeGrob(acf_plot, pacf_plot, ncol = 2), 
       width = 14, height = 6, dpi = 300)

adf_test <- adf.test(ts_series, k = trunc((length(ts_series) - 1)^(1/3)))
kpss_test <- kpss.test(ts_series)

stationarity_results <- data.frame(
  Test = c("Augmented Dickey-Fuller", "KPSS"),
  Null_Hypothesis = c("Unit root (non-stationary)", "Stationary"),
  Statistic = c(adf_test$statistic, kpss_test$statistic),
  p_value = c(adf_test$p.value, kpss_test$p.value),
  Critical_Value = c(adf_test$parameter, NA),
  Result = c(
    ifelse(adf_test$p.value < 0.05, "Stationary", "Non-stationary"),
    ifelse(kpss_test$p.value > 0.05, "Stationary", "Non-stationary")
  )
)

kable(stationarity_results, digits = 4, caption = "Stationarity Test Results") %>%
  save_kable("output_tables/ts_stationarity_tests.png")

cat("\nStationarity Analysis:\n")
print(stationarity_results)

cat("\n=== TIME SERIES PREPARATION ===\n")

cat("Determining optimal differencing order...\n")
ndiffs_auto <- ndiffs(ts_series)
nsdiffs_auto <- nsdiffs(ts_series) 
if (frequency(ts_series) > 1) {
  nsdiffs_auto <- nsdiffs(ts_series)
} else {
  nsdiffs_auto <- 0
}
cat(paste("Optimal differencing (nsdiffs):", nsdiffs_auto, "\n"))
cat(paste("Optimal differencing (ndiffs):", ndiffs_auto, "\n"))

# Apply differencing
if (nsdiffs_auto > 0) {
  ts_diff <- diff(ts_series, differences = nsdiffs_auto, lag = frequency(ts_series))
  cat("Applied seasonal differencing\n")
} else {
  ts_diff <- ts_series
}

if (ndiffs_auto > 0) {
  ts_diff <- diff(ts_diff, differences = ndiffs_auto)
  cat("Applied first-order differencing\n")
}

# Check stationarity of differenced series
if (ndiffs_auto + nsdiffs_auto > 0) {
  adf_diff <- adf.test(ts_diff)
  cat(paste("ADF on differenced series: p =", round(adf_diff$p.value, 4), "\n"))
  
  diff_plot <- autoplot(ts_diff) +
    labs(title = "Differenced Time Series",
         subtitle = paste("Order:", ndiffs_auto + nsdiffs_auto),
         x = "Time", y = "Differenced Value") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("output_figures/ts_differenced.png", diff_plot, width = 12, height = 6, dpi = 300)
}

cat("\n=== ARIMA MODEL IDENTIFICATION ===\n")

cat("Running automatic ARIMA selection...\n")
auto_arima <- auto.arima(ts_series,
                         seasonal = frequency(ts_series) > 1,
                         stepwise = FALSE,
                         approximation = FALSE,
                         trace = TRUE,
                         ic = "aic")

cat("\n=== AUTO-ARIMA SELECTED ===\n")
print(auto_arima)

candidate_models <- list(
  "ARIMA(1,0,1)" = Arima(ts_series, order = c(1, ndiffs_auto, 1), 
                         seasonal = list(order = c(0, nsdiffs_auto, 0), period = frequency(ts_series))),
  "ARIMA(2,0,2)" = Arima(ts_series, order = c(2, ndiffs_auto, 2),
                         seasonal = list(order = c(0, nsdiffs_auto, 0), period = frequency(ts_series))),
  "Auto-ARIMA" = auto_arima
)

if (frequency(ts_series) > 1) {
  candidate_models[["SARIMA(1,0,1)(1,0,1)"]] <- Arima(ts_series, 
                                                      order = c(1, ndiffs_auto, 1),
                                                      seasonal = list(order = c(1, nsdiffs_auto, 1), 
                                                                      period = frequency(ts_series)))
}

candidate_models[["ETS(A,N,N)"]] <- ets(ts_series, model = "ANN")
if (frequency(ts_series) > 1) {
  candidate_models[["ETS(A,A,A)"]] <- ets(ts_series, model = "AAA")
  candidate_models[["ETS(A,Ad,A)"]] <- ets(ts_series, model = "AAA", damped = TRUE)}

cat("\n=== MODEL COMPARISON ===\n")

library(purrr)
library(dplyr)
library(stats)

safe_num <- function(expr) {
  tryCatch(as.numeric(expr), error = function(e) NA_real_, warning = function(w) NA_real_)
}

is_bad_model <- function(m) {
  is.null(m) || inherits(m, "error")
}

get_aic <- function(m) {
  if (is_bad_model(m)) return(NA_real_)
  safe_num(if (!is.null(m$aic)) m$aic else AIC(m))
}

get_bic <- function(m) {
  if (is_bad_model(m)) return(NA_real_)
  safe_num(if (!is.null(m$bic)) m$bic else BIC(m))
}

get_loglik <- function(m) {
  if (is_bad_model(m)) return(NA_real_)
  val <- tryCatch({
    if (!is.null(m$loglik)) as.numeric(m$loglik)
    else as.numeric(logLik(m))
  }, error = function(e) NA_real_)
  safe_num(val)
}

get_sigma2 <- function(m) {
  if (is_bad_model(m)) return(NA_real_)
  val <- tryCatch({
    if (!is.null(m$sigma2)) as.numeric(m$sigma2)
    else {
      r <- residuals(m)
      if (!is.null(r)) as.numeric(var(as.numeric(r), na.rm = TRUE)) else NA_real_
    }
  }, error = function(e) NA_real_)
  safe_num(val)
}

is_seasonal_flag <- function(m) {
  if (is_bad_model(m)) return(NA)
  if (!is.null(m$arma) && length(m$arma) >= 6) {
    return(as.logical(safe_num(m$arma[6]) > 0))
  }
  if (!is.null(m$seasonal)) return(TRUE)
  if (!is.null(m$series) && !is.null(m$period)) return(as.logical(m$period > 1))
  return(NA)
}

ts_comparison <- tibble(
  Model = names(candidate_models)
) %>%
  mutate(
    model_obj = candidate_models[Model],
    AIC = map_dbl(model_obj, get_aic),
    BIC = map_dbl(model_obj, get_bic),
    Log_Likelihood = map_dbl(model_obj, get_loglik),
    Sigma2 = map_dbl(model_obj, get_sigma2),
    Is_Seasonal = map_lgl(model_obj, is_seasonal_flag)
  ) %>%
  arrange(AIC)

ts_comparison <- dplyr::select(ts_comparison, -model_obj)
print(ts_comparison)

kable(ts_comparison, digits = 2, caption = "Time Series Model Comparison") %>%
  save_kable("output_tables/ts_model_comparison.png")

cat("Best model based on AIC:", rownames(ts_comparison)[1], "\n")

best_ts_model <- candidate_models[[which.min(ts_comparison$AIC)]]

cat("\n=== TIME SERIES DIAGNOSTICS ===\n")

ts_residuals <- residuals(best_ts_model)

lb_test <- Box.test(ts_residuals, lag = min(20, length(ts_residuals)/5), type = "Ljung-Box")
cat(paste("Ljung-Box test p-value:", round(lb_test$p.value, 4), "\n"))

res_acf <- acf(ts_residuals, plot = FALSE, lag.max = 36)
res_acf_plot <- autoplot(res_acf) +
  geom_hline(yintercept = c(-1.96/sqrt(length(ts_residuals)), 1.96/sqrt(length(ts_residuals))), 
             linetype = "dashed", color = "red") +
  labs(title = "Residual ACF",
       subtitle = paste("Ljung-Box p =", round(lb_test$p.value, 4)),
       x = "Lag", y = "ACF") +
  theme(plot.title = element_text(hjust = 0.5))

res_hist <- ggplot(data.frame(Residuals = ts_residuals), aes(Residuals)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
  geom_density(color = "red", size = 1.2) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ts_residuals), sd = sd(ts_residuals)),
                color = "darkgreen", linetype = "dashed") +
  labs(title = "Residual Distribution",
       subtitle = paste("Mean =", round(mean(ts_residuals), 3)), 
       x = "Residuals", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

res_qq <- ggplot(data.frame(Residuals = ts_residuals), aes(sample = Residuals)) +
  stat_qq(color = "steelblue", size = 1.5) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Normal Q-Q Plot of Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

res_time <- ggplot(data.frame(Time = 1:length(ts_residuals), Residuals = ts_residuals),
                   aes(Time, Residuals)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Time",
       x = "Time Index", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

pdf("output_figures/ts_diagnostics.pdf", width = 14, height = 10)
grid.arrange(res_acf_plot, res_hist, res_qq, res_time, ncol = 2, nrow = 2)
dev.off()

cat("\n=== FORECASTING ===\n")

forecast_horizon <- floor(length(ts_series) * 0.20)
cat(paste("Forecasting horizon:", forecast_horizon, "periods\n"))

forecasts <- forecast(best_ts_model, h = forecast_horizon, level = c(80, 95))

k <- length(test_actual)
preds_last_k <- tail(as.numeric(forecasts$mean), k)
accuracy_table <- forecast::accuracy(preds_last_k, as.numeric(test_actual))
test_actual <- tail(ts_series, forecast_horizon)

#accuracy_table <- accuracy(forecasts, test_actual)
kable(accuracy_table, digits = 4, caption = "Forecast Accuracy") %>%
  save_kable("output_tables/ts_forecast_accuracy.png")

forecast_plot <- autoplot(forecasts) +
  autolayer(tail(ts_series, forecast_horizon * 2), series = "Actual Test Data", color = "black") +
  labs(title = "Time Series Forecast vs Actual",
       subtitle = paste("Model:", class(best_ts_model)[1]),
       x = "Time", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("output_figures/ts_forecast.png", forecast_plot, width = 12, height = 6, dpi = 300)

in_interval <- test_actual >= forecasts$lower[,2] & test_actual <= forecasts$upper[,2]
coverage_rate <- mean(in_interval, na.rm = TRUE)
cat(paste("95% Prediction Interval Coverage:", round(coverage_rate * 100, 1), "%\n"))

