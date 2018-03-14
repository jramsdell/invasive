library(raster)
library(caret)
library(gsubfn)
library(rgdal)
library(dplyr)

#devtools::install_github("ggrothendieck/gsubfn")

get_test_train <- function(df, pos_ratio, neg_ratio) {
  response_pos <- df %>% filter(present == "True") %>% sample_frac(pos_ratio)
  response_neg <- df %>% filter(present == "False") %>% sample_frac(neg_ratio)
  
  test_set <- response_pos %>% rbind(response_neg)
  train_set <- anti_join(df, test_set, by="id")
  list(test_set, train_set)
}

get_init_data <- function() {
  glossy <- readRDS("../../resources/new_glossy_data_frame.rds")
  colnames(glossy) <- gsub('_13', '', colnames(glossy), fixed=TRUE)
  glossy <- glossy %>% mutate(id = seq_along(present))
  
  rasters <- readRDS("../../resources/bio_13.rds")
  
  list(glossy, rasters)
}