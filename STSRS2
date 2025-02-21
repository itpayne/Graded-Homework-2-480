set.seed(881)
> data <- read.csv("California2006GOVfv.csv")
> data$strata_var <- (data$Votes + 1) / (data$NValid + 1)
> View(data)
> data$stratum <- cut(data$strata_var, 
+                     breaks = quantile(data$strata_var, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 
+                     include.lowest = TRUE, labels = FALSE)
> strata_summary <- data %>% 
+     group_by(stratum) %>% 
+     summarise(N = n(),
+               mean_fraud = mean(Nfraudmean, na.rm = TRUE),
+               sd_fraud = sd(Nfraudmean, na.rm = TRUE))
> strata_summary <- strata_summary %>% 
+     mutate(sample_size = pmin(round((N * sd_fraud) / sum(N * sd_fraud) * 100), N))
> sampled <- data %>%
+     group_by(stratum) %>%
+     sample_n(size = pmin(10, n()), replace = FALSE)
> est_mean <- sum(sampled$Nfraudmean * sampled$NValid) / sum(sampled$NValid)
> actual_bound <- 1.96 * sd(sampled$Nfraudmean, na.rm = TRUE) / sqrt(nrow(sampled))
> est_mean
[1] 11.92877
> actual_bound
[1] 9.353219
> nrow(sampled)
[1] 50
