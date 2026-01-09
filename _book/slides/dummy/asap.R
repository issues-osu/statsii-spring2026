# Disable scientific notation to ensure large numbers are displayed in full (not in exponential form)
options(scipen = 999)

# Function to simulate a single jury selection
jury_experiment <- function(n_jurors = 12, total_population = 1000, Black_threshold = 90) {
  # Randomly select `n_jurors` from a total population of `total_population`
  # Each number represents a juror, where values below `Black_threshold` indicate Black jurors
  sample(1:total_population, size = n_jurors, replace = TRUE)
}

# Function to run multiple jury selections
simulate_juries <- function(n_simulations, n_jurors = 12, total_population = 1000, Black_threshold = 90) {
  # Repeat the `jury_experiment` function `n_simulations` times, storing results in a matrix
  many_juries <- replicate(n_simulations, jury_experiment(n_jurors, total_population, Black_threshold))
  
  # Convert the matrix to a data frame where:
  # - Each row represents one simulated jury selection
  # - Each column corresponds to one of the 12 selected jurors in that jury
  jury_df <- as.data.frame(t(many_juries))
  
  # Count the number of Black jurors in each jury
  # Black jurors are those whose values are less than `Black_threshold` (9% of total population)
  jury_df$no_Black_jurors <- rowSums(jury_df < Black_threshold)
  
  # Return the dataframe containing all jury simulations and their respective Black juror counts
  return(jury_df)
}

# Define different sample sizes representing increasing numbers of jury selections
# This allows us to see how the sampling distribution stabilizes with larger sample sizes
sample_sizes <- c(10, 50, 100, 1000, 100000, 1000000)

# Compute the expected number of Black jurors per 12-person jury
# Since 9% of the total population is Black, we expect 12 * 0.09 = 1.08 Black jurors on average
expected_black_jurors <- 12 * 0.09

# Set up the plotting layout for 6 histograms arranged in a 2-row by 3-column grid
par(mfrow=c(2,3))

# Loop over different sample sizes to run simulations and generate histograms
for (n_sim in sample_sizes) {
  
  # Run the simulation for `n_sim` jury selections, each consisting of 12 jurors
  jury_df <- simulate_juries(n_simulations = n_sim, n_jurors = 12, total_population = 1000, Black_threshold = 90)
  
  # Generate a histogram for the number of Black jurors per 12-person jury
  hist(jury_df$no_Black_jurors, 
       breaks=seq(0, 12, by=1),  # Bins for the histogram (0 to 12 Black jurors)
       freq=FALSE,               # Display relative frequency instead of counts
       col="blue",               # Use blue color for bars
       main=paste("Black Jurors in 12-Person Jury\n", format(n_sim, big.mark=","), " Samples"), 
       xlab="Number of Black Jurors", 
       ylab="Probability", 
       yaxs="i", xaxs="i")
  
  # Add a vertical red dashed line to indicate the expected value of Black jurors (1.08)
  abline(v = expected_black_jurors, col="red", lwd=2, lty=2)
}
