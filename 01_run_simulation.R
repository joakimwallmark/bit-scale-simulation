library(dplyr)
library(tibble)
library(purrr)
library(mirt)
library(bitscale)

# Simulation Parameters
num_replications <- 4000
items_vec <- c(20, 40, 80, 160)
true_thetas <- seq(-3, 3, length.out = 101)
theta_clip_range <- c(-6, 6)

simulate_for_items <- function(nitems, num_replications) {
  cat(paste0("\n\n========================================\n"))
  cat(paste0("Starting simulation for: ", nitems, " items\n"))
  cat(paste0("========================================\n"))
  
  cat("-> Generating true model and parameters... ")
  
  # Define True Item Parameters and create model object
  a <- matrix(rlnorm(nitems, meanlog = 0, sdlog = 0.5))
  d <- matrix(rnorm(nitems, 0, 1))
  data_full <- simdata(a, d, 10000, itemtype = "2PL")
  pars <- mirt(data_full, itemtype = "2PL", pars = "value", verbose = FALSE)
  pars$value[pars$name == "a1"] <- as.vector(a)
  pars$est[pars$name == "a1"] <- FALSE
  pars$value[pars$name == "d"] <- as.vector(d)
  pars$est[pars$name == "d"] <- FALSE
  true_model <- mirt(data_full, 1, itemtype = "2PL", pars = pars, verbose = FALSE)
  
  # Calculate true bit scores for the fixed grid
  true_bit_scores <- as.numeric(bit_scores(true_model, true_thetas))
  
  cat("Done.\n")
  cat(paste0("-> Starting ", num_replications, " replications:\n"))
  
  # Define the single replication work
  simulate_replication <- function(idx){
    # Print progress every 100 iterations
    if (idx == 1 || idx %% 100 == 0) {
      cat(paste0("   Processing replication ", idx, " / ", num_replications, "...\n"))
      flush.console() # Force R to update the console immediately
    }
    
    # Generate data based on fixed item parameters and the grid of true thetas
    sim_data <- simdata(a, d, Theta = true_thetas, itemtype = "2PL")
    
    # Estimate scores (ML)
    fs <- fscores(true_model, response.pattern = sim_data, method = "ML", 
                  full.scores.SE = TRUE, verbose = FALSE)
    
    # Calculate Bit scores based on the ML theta estimates
    bit_s <- bit_scores(true_model, fs)
    
    list(
      theta = fs[,1],
      bit = bit_s[, 1],
      theta_se = fs[,2],
      bit_se = bit_s[, 2]
    )
  }
  
  # Run replications sequentially using map
  sim_results <- map(1:num_replications, simulate_replication)
  cat("-> Replications finished. processing results...\n")
  
  # Extract matrices of results
  theta_mat <- do.call(rbind, map(sim_results, "theta"))
  bit_mat <- do.call(rbind, map(sim_results, "bit"))
  theta_se_mat <- do.call(rbind, map(sim_results, "theta_se"))
  bit_se_mat <- do.call(rbind, map(sim_results, "bit_se"))
  
  # Handle infinite ML estimates for Theta
  theta_mat[theta_mat == -Inf] <- theta_clip_range[1]
  theta_mat[theta_mat == Inf]  <- theta_clip_range[2]
  
  # Calculate Theta Metrics
  theta_bias <- colMeans(theta_mat) - true_thetas
  theta_se_sim <- apply(theta_mat, 2, sd)
  theta_rmse <- sqrt(colMeans((theta_mat - matrix(true_thetas, nrow = num_replications, ncol = length(true_thetas), byrow = TRUE))^2))
  
  # Calculate Bit Metrics
  bit_bias <- colMeans(bit_mat) - true_bit_scores
  bit_se_sim <- apply(bit_mat, 2, sd)
  bit_rmse <- sqrt(colMeans((bit_mat - matrix(true_bit_scores, nrow = num_replications, ncol = length(true_bit_scores), byrow = TRUE))^2))
  
  # Structure Output DataFrames
  df_theta <- tibble(
    true_theta = rep(true_thetas, 3),
    metric_value = c(theta_bias, theta_se_sim, theta_rmse),
    metric = factor(rep(c("Bias", "SE", "RMSE"), each = length(true_thetas)))
  )
  
  df_bit <- tibble(
    true_bit = rep(true_bit_scores, 3),
    metric_value = c(bit_bias, bit_se_sim, bit_rmse),
    metric = factor(rep(c("Bias", "SE", "RMSE"), each = length(true_bit_scores)))
  )
  
  df_theta_se <- tibble(
    true_theta = rep(true_thetas, 2),
    SE_value = c(colMeans(theta_se_mat, na.rm = TRUE), theta_se_sim),
    type = factor(rep(c("Theoretical SE", "Empirical SE"), each = length(true_thetas)))
  )
  
  df_bit_se <- tibble(
    true_bit = rep(true_bit_scores, 2),
    SE_value = c(colMeans(bit_se_mat, na.rm = TRUE), bit_se_sim),
    type = factor(rep(c("Theoretical SE", "Empirical SE"), each = length(true_bit_scores)))
  )
  
  list(df_theta = df_theta, df_bit = df_bit, df_theta_se = df_theta_se, df_bit_se = df_bit_se)
}

set.seed(127)
cat("Starting Global Simulation Loop...\n")
all_results <- map(
  items_vec, 
  ~{
    res <- simulate_for_items(.x, num_replications)
    list(
      df_theta    = res$df_theta |> mutate(items = .x),
      df_bit      = res$df_bit |> mutate(items = .x),
      df_theta_se = res$df_theta_se |> mutate(items = .x),
      df_bit_se   = res$df_bit_se |> mutate(items = .x)
    )
  }
)

# Combine results
cat("\nCombining all results...\n")
df_theta_all <- map_df(all_results, "df_theta")
df_bit_all <- map_df(all_results, "df_bit")
df_theta_se_all <- map_df(all_results, "df_theta_se")
df_bit_se_all <- map_df(all_results, "df_bit_se")

# Save
cat("Saving to RDS...\n")
saveRDS(df_theta_all, "df_theta_all.rds")
saveRDS(df_bit_all, "df_bit_all.rds")
saveRDS(df_theta_se_all, "df_theta_se_all.rds")
saveRDS(df_bit_se_all, "df_bit_se_all.rds")
