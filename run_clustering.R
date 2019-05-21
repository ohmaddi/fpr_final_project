# This script runs a k-means analysis with a variable number of cluster centers.


# Set-up ------------------------------------------------------------------
library(here)
library(tidyverse)
library(factoextra)
library(janitor)
library(R.utils)

# Load data ---------------------------------------------------------------
pokemon_data <- clean_names(read.csv(here("pokemon.csv")))

## Create a data frame without missing values that contains the type1 labels
pokemon_data <- pokemon_data %>% 
    select(-percentage_male, -type2) %>%  
    filter(type1 %in% c("ghost", "fairy", "dragon")) %>%  # Let's limit this to a few pokemon
    mutate(type1 = droplevels(type1)) %>% # Get rid of unused factor levels
    drop_na() 

## Create data frame with "against" variables 
pokemon_against <- pokemon_data %>% 
    select(starts_with("against"), hp)

# Scale data --------------------------------------------------------------
pokemon_against <- pokemon_against %>% 
    scale() %>% 
    as.data.frame()

# Run K-means clustering --------------------------------------------------

## define function
run_clust <- function(df, k) {
    set.seed(100)
    rownames(df) <- c() # need to remove rownames in order to get atomic vector output for cluster membership data
    eclust(df,
           FUNcluster = 'kmeans',
           hc_metric = 'euclidean',
           k = k, nstart = 25, graph = TRUE)
}

if (!exists(here("kmeans"))){
    dir.create(here("kmeans"))
}


k <- 6 # max number of clusters

# run k-means clustering for all cluster numbers
kmeans <- map(2:k, ~run_clust(pokemon_against, .x))

# specify file names
filenames <- map_chr(2:k, ~paste(here("kmeans"), "/clust", .x, ".Rda", sep = ""))

# save files
walk2(kmeans, filenames, ~saveObject(.x, .y))
