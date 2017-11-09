source('maxent_recover.R')

glossy <- readRDS("../../resources/glossy_data_frame.rds")
XX <- assignAndGroupByCell(glossy)

XX$lon <- NULL
XX$lat <- NULL
XX$cell <- NULL

# # normalize, but skip the probablity
XX <- apply(XX, 2, rescale)

total_sample_count = nrow(XX)

initial_sample_count = 100
num_steps = 4
n_iter = 10

step_size = floor(total_sample_count / num_steps)

kl_divergence1 <- array(0, num_steps)
kl_divergence2 <- array(0, num_steps)
kl_divergence_com <- array(0, num_steps)
kl_idx <- 1

sample_counts <- floor(seq(from = initial_sample_count, to = total_sample_count, length.out = num_steps))

n_features <- ncol(XX)

for (sample_count in sample_counts) {
  for (it in 1:n_iter) {
    cat(sprintf("KL idx = %d, %d samples, iteration %d \n", kl_idx, sample_counts[kl_idx], it))
    X <- XX[sample(1:nrow(XX), total_sample_count),]
    
    # X <- apply(X, 2, rescale)
    
    # create random weights
    lambdas_base = as.vector(runif(n_features))
    
    # create π
    pi_base <- softmax(X, lambdas_base)
    
    # Sample from the multinomial distribution
    res <- rmultinom(1, size = sample_count, prob = pi_base)[,1]
    
    pi_tilde <- res / sample_count
    
    lambdas <- maxent(X, pi_tilde)
    
    pi_hat <- softmax(X, lambdas)
    kl <- KL.plugin(pi_base, pi_hat)
  
    kl_divergence1[kl_idx] <- kl_divergence1[kl_idx] + kl
  }
  
  kl_divergence1[kl_idx] <- kl_divergence1[kl_idx] / n_iter
  
  kl_idx <- kl_idx + 1
}

# filename_kl <- paste("KL_Divergence_", format(Sys.Date(), "%m_%d_%y_"), total_sample_count, "_", length(kl_divergence), ".pdf", sep = "")
kl_dataframe <- data.frame(x = sample_counts, kl1 = kl_divergence1)

#kl <- 
ggplot(kl_dataframe) +
  geom_line(aes(x = x, y = kl1, color = 'KL Divergence 1'))  +
  geom_point(aes(x = x, y = kl1, color = 'KL Divergence 1')) +
  xlab("Number of samples") +
  scale_y_continuous("KL(π, ^π)") +
  labs(title = "KL Divergence between π, ^π") +
  theme(plot.title = element_text(hjust = 0.5))

#print(kl)
#ggsave(filename_kl)