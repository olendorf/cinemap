###################
## Automate package install and load

is_installed <- function(package_name) is.element(package_name, installed.packages()[,1])

# If a package is not installed, install it. Then load the package.
install_and_load <- function(package_name) {
  if(!is_installed(package_name)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

install_packages <- function(packages) {
  for(package in packages) {
    install_and_load(package)
  }
}


install_packages(c("rvest", "TMDb"))

api_key_v3 <- '90baee00115159ddf9966b23a1d51062'

master_data <- read.csv('data/master_data.csv', header = TRUE)

# query_results_en <- search_movie(api_key=api_key_v3, query="If cats disappeared from the world")
# query_results_romanized <- search_movie(api_key=api_key_v3, query="sekai kara neko ga kietanara")
# query_results_original <- search_movie(api_key=api_key_v3, query="世界から猫が消えたなら")

cinemap_brief <- master_data[1:50,]  # This is for development. Remove and substitute master_data later


cinemap_brief$year_released <- as.character(cinemap_brief$year_released)
cinemap_brief$film_title_en <- as.character(cinemap_brief$film_title_en)
cinemap_brief$film_title_romanji <- as.character(cinemap_brief$film_title_romanji)
cinemap_brief$film_title_original <- as.character(cinemap_brief$film_title_original)

# Initialize new columns with default values
cinemap_brief$en_hits <- 0
cinemap_brief$romanji_hits <- 0
cinemap_brief$original_lang_hits <- 0
cinemap_brief$tmdb_id <- ""

movie_data <- function(title, year){
  data <- list(num_results = 0, results = NULL)
  if(title != ""){
    query_result <- search_movie(api_key = api_key_v3, query = title, year = year)
    print(query_result$total_results)
    data$num_results <- query_result$total_results
    # Ignore results of 0 or with multiple returns as we cannot use them.
    if(query_result$total_results == 1) {
      data$results <- query_result
    }
  }
  return(data)
}

for(i in 1:length(cinemap_brief$film_title_en)) {
  print("checking english title")
  result <- movie_data(cinemap_brief[i,]$film_title_en, cinemap_brief[i,]$year_released)
  Sys.sleep(1)
  cinemap_brief[i,]$en_hits <- result$num_results
  if(is.null(result$results)) {
    print("checking romanji title")
    result <- movie_data(cinemap_brief[i,]$film_title_romanji, cinemap_brief[i,]$year_released)
    cinemap_brief[i,]$romanji_hits <- result$num_results
    Sys.sleep(1)
  }
  if(is.null(result$results)) {
    print("checking original title")
    result <- movie_data(cinemap_brief[i,]$film_title_original, cinemap_brief[i,]$year_released)
    cinemap_brief[1,]$original_lang_hits <- result$num_results
    Sys.sleep(1)
  }
}


