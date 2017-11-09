source("includes.R")

# lambdas <- as.matrix(lambdas)
#' return q_lambda
#' @param X matrix of feature values
#' @param lambda the feature weight vector
softmax <- function(x, lambda) {
  nom <- exp((x %*% lambda))
  z <- sum(nom)
  return(nom / z)
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
maxent <- function(X, pi_tilde, iterations = 1000){
  lambdas = vector(mode = 'numeric', ncol(X))
  i <- 0
  n_features <- ncol(X)
  step = 0.1
  
  calculateLogLoss <- function(lambdas) {
    QL <- softmax(X, lambdas)
    log_loss <- calcWeightExp(pi_tilde, -log(QL))
    return(log_loss)
  }
  
  for (i in 0:iterations) {
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
  }

  return(lambdas)
}

cellFromRaster <- function(lon, lat, raster) {
  if (length(lon) != length(lat)) stop("lon and lat vectors must be equal length!")
  
  cell <- array(0, length(lon))
  
  for (i in 1:length(lon)) {
    cell[i] <- cellFromXY(raster, cbind(lon[i], lat[i]))
  }
  
  return(cell)
}

assignAndGroupByCell <- function(glossy_data_frame) {
  raster <- raster("../../resources/bio7_13.tif")
  X <- glossy_data_frame
  
  X$cell <- cellFromRaster(X$lon, X$lat, raster)
  
  X <- group_by(X, cell) %>% summarise_all(mean)
    # summarize(
    #   lon = mean(lon),
    #   lat = mean(lat),
    #   bio1 = mean(bio1),
    #   bio2 = mean(bio2),
    #   bio3 = mean(bio3),
    #   bio4 = mean(bio4),
    #   bio5 = mean(bio5),
    #   bio6 = mean(bio6),
    #   bio8 = mean(bio8),
    #   bio9 = mean(bio9),
    #   bio10 = mean(bio10),
    #   bio11 = mean(bio11),
    #   bio12 = mean(bio12),
    #   bio13 = mean(bio13),
    #   bio14 = mean(bio14),
    #   bio15 = mean(bio15),
    #   bio16 = mean(bio16),
    #   bio17 = mean(bio17),
    #   bio18 = mean(bio18),
    #   bio19 = mean(bio19)
    # )
  
  return(X)
}

groupByCell <- function(data_frame) {
  grouped_data <- group_by(data_frame, cell) %>%
    summarize(
      # freq = nrow(.),
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
  
  return(grouped_data)
}
