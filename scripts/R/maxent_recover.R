source("includes.R")

# lambdas <- as.matrix(lambdas)
#' return q_lambda
#' @param X matrix of feature values
#' @param lambda the feature weight vector
calcQL <- function(X, lambda) {
  nom <- exp((X %*% lambda))
  Z <- sum(nom)
  return(nom / Z)
}

#' return weightd expected value
#' @param dist the probability distribution
#' @param fi feature values
calcWeightExp <- function(dist, fi) {
  return(as.vector(dist) %*% as.vector(fi))
}

#' calculate weight increases
#' @param q_l q lambda distribution
#' @param p_t the empirical distribution
#' @param fi the feature values
calcAlpha <- function(q_l, p_t, fi) {
  a <- calcWeightExp(p_t, fi)
  b <- calcWeightExp(q_l, fi)
  
  alpha_1 = log(((a - 0.1) * (1 - b)) / ((1 - a + 0.1) * b))
  alpha_2 = log(((a + 0.1) * (1 - b)) / ((1 - a - 0.1) * b))
  
  alpha_ = log(((a) * (1 - b)) / ((1 - a) * b))
  
  ret <- c(alpha_1, alpha_2, alpha_)
  return(ret)
}

#' calculate weights for maxent
#' @param newX data samples
#' @param pi_tilde the empirical distribution to approximate
#' @return the generated lambdas, the generated distribution, and the kl divergence
calcMaxent <- function(newX, pi_base, pi_tilde){
  lambdas = vector(mode = 'numeric', ncol(newX))
  rounds <- 0
  n_features <- ncol(newX)
  step = 0.1
  
  calculateLogLoss <- function(lambdas) {
    QL <- calcQL(newX, lambdas)
    log_loss <- calcWeightExp(pi_tilde, -log(QL))
    return(log_loss)
  }
  
  while (rounds < 1000) {
    log_loss <- calculateLogLoss(lambdas)
    starting_log_loss <- log_loss
    
    for (feature in 1:n_features) {
      # cat(sprintf("Round #%d \tFeature #%d \t i=%d\n", rounds, f, i))
      temp_lambdas <- lambdas
      temp_lambdas[feature] <- lambdas[feature] - step
      log_loss_ <- calculateLogLoss(temp_lambdas)
      
      if (log_loss_ < log_loss) {
        lambdas <- temp_lambdas

        if (abs(log_loss - log_loss_) < 0.000001) {
          break
        }
        
        log_loss <- log_loss_
        next  
      }
      
      temp_lambdas <- lambdas
      temp_lambdas[feature] <- lambdas[feature] + step
      log_loss_ <- calculateLogLoss(temp_lambdas)
      
      if (log_loss_ < log_loss) {
        lambdas <- temp_lambdas

        if (abs(log_loss - log_loss_) < 0.000001) {
          break
        }
        
        log_loss <- log_loss_
        
      }
      
    }
    
    if (abs(log_loss - starting_log_loss) < 0.00001) {
      break
    }
    
    rounds <- rounds + 1
  }

  nominator <- exp((newX %*% lambdas))
  Z <- sum(nominator)
  pi_hat <- nominator / Z
  kl <- KL.plugin(pi_base, pi_hat)
  retList = list("lambdas" = lambdas, "pi_hat" = pi_hat, "kl" = kl)
  return(retList)
}
# Load a raster so cells are available
raster <- raster("../../resources/bio7_13.tif")

# APPEND A RESPONSE COLUMN TO THE POPULATION DATA
glossy_data_frame_glm$y <- 1

X <<- glossy_data_frame_glm

n_features <- ncol(X) - 1
#  X dataset (dataframe)
# row is a data point

n_samples = nrow(X)
n_samples_pos = sum(X$y)

# ALREADY_FOUND = FALSE
# if(ALREADY_FOUND == FALSE){
# group by cells but it might not be necessary
for (i in 1:nrow(X)) {
  X$cell[i] <- cellFromXY(raster, cbind(X$lon[i], X$lat[i]))
}

X <- group_by(X, cell) %>%
  summarize(
    # prob = sum(y) / n_samples_pos,
    lon = mean(lon),
    lat = mean(lat),
    bio1 = mean(bio1),
    bio2 = mean(bio2),
    bio3 = mean(bio3),
    bio4 = mean(bio4),
    bio5 = mean(bio5),
    bio6 = mean(bio6),
    bio8 = mean(bio8),
    bio9 = mean(bio9),
    bio10 = mean(bio10),
    bio11 = mean(bio11),
    bio12 = mean(bio12),
    bio13 = mean(bio13),
    bio14 = mean(bio14),
    bio15 = mean(bio15),
    bio16 = mean(bio16),
    bio17 = mean(bio17),
    bio18 = mean(bio18),
    bio19 = mean(bio19)
  )
saveRDS(X, "../../resources/dataWithCells.rds")
# }else{
#   X <<- readRDS("../../resources/dataWithCells.rds")
# }

# X$cell <- NULL
X$lon <- NULL
X$lat <- NULL

cells_array <- X$cell
X$cell <- NULL

lc <- findLinearCombos(X)
X[lc$remove] <- NULL

XX <- X

initial_pop = floor(n_samples / 5)
num_steps = 20
n_iter = 20
step_size = floor(initial_pop / num_steps)

kl_divergence <- array(0, num_steps)
kl_divergence_com <- array(0, num_steps)
kl_idx <- 1

sample_num <- seq(step_size, length.out = n_iter, by = step_size)

n_features <- ncol(XX)

for (multi_samp in sample_num) {
  weight_df = data.frame(matrix(ncol = 5, nrow = n_features))
  dist_df = data.frame(matrix(ncol = 4, nrow = initial_pop))
  zero <- vector(mode = "numeric", length = length(multi_samp))
  for (it in 1:n_iter) {
    cat(sprintf("KL idx = %d, %d samples, iteration %d \n", kl_idx, sample_num[kl_idx], it))
    X <- XX[sample(1:nrow(X), initial_pop, replace = FALSE),]

    # # normalize, but skip the probablity
    X <- apply(X, 2, rescale)

    # create random weights
    lambdas_base = as.vector(runif(n_features))
    
    # create π
    pi_base <- calcQL(X, lambdas_base)

    res <- rmultinom(1, size = multi_samp, prob = pi_base)

    newX <- X
    pi_tilde = vector(mode = 'numeric', initial_pop)

    j = 1
    for (i in 1:initial_pop) {
      if (res[i] > 0) {
        pi_tilde[i] = res[i] / multi_samp
      } else {
        pi_tilde[i] = 0.0
      }
    }
    
    ret <- calcMaxent(newX, pi_base, pi_tilde)
    kl_divergence[kl_idx] <- kl_divergence[kl_idx] + ret$kl

    #we now use the derived weights as the generating distribution
    lambdas_base = ret$lambdas
    
    # create π
    pi_base <- calcQL(X, lambdas_base)
    res <- rmultinom(1, size = multi_samp, prob = pi_base)

    newX <- X
    pi_tilde = vector(mode = 'numeric', initial_pop)

    j = 1
    for (i in 1:initial_pop) {
      if (res[i] > 0) {
        pi_tilde[i] = res[i] / multi_samp
      } else {
        pi_tilde[i] = 0.0
      }
    }
    
    ret <- calcMaxent(newX, pi_base, pi_tilde)
    kl_divergence_com[kl_idx] <- kl_divergence[kl_idx] + ret$kl
  }
  
  kl_divergence_com[kl_idx] <- kl_divergence[kl_idx] / n_iter
  kl_idx <- kl_idx + 1
}

filename_kl <- paste("KL_Divergence_", format(Sys.Date(), "%m_%d_%y_"), initial_pop, "_", length(kl_divergence), ".pdf", sep = "")
kl_dataframe <- data.frame(x = 1:length(kl_divergence) * step_size * initial_pop, kl = kl_divergence)

#kl <- 
ggplot(kl_dataframe) +
  geom_line(aes(x = x, y = kl ,color = 'KL Divergence'))  +
  geom_point(aes(x = x, y = kl, color = 'KL Divergence')) +
  xlab("Number of samples") +
  scale_y_continuous("KL(π, ^π)") +
  labs(title = "KL Divergence between π, ^π") +
  theme(plot.title = element_text(hjust = 0.5))

  #ggplot(kl_com_dataframe)+
  #  geom_line(aes(x=x, y=kl ,color='KL Divergence'))  +
  #  geom_point(aes(x = x, y = kl, color = 'KL Divergence')) +
  #  xlab("Number of samples") +
  #  scale_y_continuous("KL(π, ^π)") +
  #  labs(title = "KL Divergence between π, ^π") +
  #  theme(plot.title = element_text(hjust = 0.5))

#print(kl)
#ggsave(filename_kl)
