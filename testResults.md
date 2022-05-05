# test results

## Twelve Mile 1, Functional Group 1

#### KS Test
> print(KS_results)
[[1]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.2, p-value = 0.9883
alternative hypothesis: two-sided


[[2]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.2, p-value = 0.9883
alternative hypothesis: two-sided


[[3]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.4, p-value = 0.4005
alternative hypothesis: two-sided


[[4]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.4, p-value = 0.4005
alternative hypothesis: two-sided


[[5]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.3, p-value = 0.7591
alternative hypothesis: two-sided

#### Chi-Squared

> print(chi_squared_results)
[[1]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 40.4, df = 25, p-value = 0.02652


[[2]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 29, df = 20, p-value = 0.08776


[[3]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 36.25, df = 30, p-value = 0.2


[[4]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 26.333, df = 20, p-value = 0.1551


[[5]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.6, df = 25, p-value = 0.1699



## Twelve Mile, Functional Group 2

### KS
print(KS_results)
[[1]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.84615, p-value = 0.001859
alternative hypothesis: two-sided


[[2]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.76923, p-value = 0.006244
alternative hypothesis: two-sided


[[3]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.76923, p-value = 0.006244
alternative hypothesis: two-sided


[[4]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.76923, p-value = 0.006244
alternative hypothesis: two-sided


[[5]]

        Two-sample Kolmogorov-Smirnov test

data:  prediction_cdf and validation_cdf
D = 0.76923, p-value = 0.006244
alternative hypothesis: two-sided

### Chi-squared

> print(chi_squared_results)
[[1]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.582, df = 20, p-value = 0.04796


[[2]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.346, df = 16, p-value = 0.01215


[[3]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.346, df = 16, p-value = 0.01215


[[4]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.135, df = 12, p-value = 0.001878


[[5]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.135, df = 12, p-value = 0.001878


> print(chi_squared_results)
[[1]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.582, df = 20, p-value = 0.04796


[[2]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.346, df = 16, p-value = 0.01215


[[3]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.346, df = 16, p-value = 0.01215


[[4]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.135, df = 12, p-value = 0.001878


[[5]]

        Pearson's Chi-squared test

data:  aggregated_results$validation_counts and aggregated_results$predicted_counts
X-squared = 31.135, df = 12, p-value = 0.001878