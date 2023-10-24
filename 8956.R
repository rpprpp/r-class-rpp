library(animation)

growhw <- function(growth.rate, plot_N1 = FALSE, plot_N2 = FALSE) {
  num_gen <- 30
  N1 <- numeric(num_gen)
  N2 <- numeric(num_gen)
  generation <- 1:num_gen
  N1[1] <- 10
  N2[1] <- 12
  K1 <- 200
  K2 <- 200
  a12 <- 1.5
  a21 <- 1.5
  
  for (i in 2:num_gen) {
    N1[i] <- N1[i - 1] + (growth.rate * N1[i - 1] * (K1 - (N1[i - 1] - a12 * N2[i - 1]) / K1))
    N2[i] <- N2[i - 1] + (growth.rate * N2[i - 1] * (K2 - (N2[i - 1] - a21 * N2[i - 1]) / K2))
    
    # Ensure populations do not become negative
    N1[i] <- ifelse(N1[i] < 0, 0, N1[i])
    N2[i] <- ifelse(N2[i] < 0, 0, N2[i])
  }
  
  # Set an appropriate y-axis limit range
  ylim_range <- c(0, 250)  # Adjust based on your data
  



  if (N1[1] > 2) {
    plot(generation, N1, type = "b", ylim = c(0, min(c(K1, K2)), ylab = "N1"))
  } else {
    plot(generation, N2, type = "b", ylim = c(0, min(c(K1, K2)), ylab = "N2"))
  }
print(N2[1])

if (N2[1] > 0) {
  lines(N2 ~ generation, type = "b", col = 2)
}

}

# Create GIF animations
saveGIF({
  for (i in seq(0.01, 0.5, by = 0.01)) {
    growhw(i, plot_N1 = TRUE)
  }
}, movie.name = "N1.gif")

saveGIF({
  for (i in seq(0.01, 0.5, by = 0.05)) {
    growhw(i, plot_N2 = TRUE)
  }
}, movie.name = "N2.gif")

saveGIF({
  for (i in seq(0.01, 0.5, by = 0.05)) {
    growhw(i, plot_N1 = TRUE, plot_N2 = TRUE)
  }
}, movie.name = "N1_and_N2.gif")
