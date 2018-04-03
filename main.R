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

master_data <- read.csv('data/master_data.csv', header = TRUE, stringsAsFactors=FALSE)

# If you get HTTP error 429 you are making too many requests too quickly. Increasing
# the sleep time will fix that once its high enough at the cost of taking longer to
# retrieve the data.
sleep_time <- 0.5

japanese_char_regex <-  "[\u3000-\u303F]|[\u3040-\u309F]|[\u30A0-\u30FF]|[\uFF00-\uFFEF]|[\u4E00-\u9FAF]|[\u2605-\u2606]|[\u2190-\u2195]|\u203B/g"

cinemap_brief <- master_data[1:50,]  # This is for development. Remove and substitute master_data later

# Do some cleanup
cinemap_brief[cinemap_brief$release_date == "--",]$release_date = ""
cinemap_brief[cinemap_brief$production_studio == "--",]$production_studio = ""
cinemap_brief$production_studio <- gsub("/", "; ", cinemap_brief$production_studio)

# Initialize new columns with default values
cinemap_brief$tmdb_id <- ""
cinemap_brief$tmdb_poster_url <- ""
cinemap_brief$genres <- ""
cinemap_brief$spoken_languages <- ""
cinemap_brief$imdb_id <- ""
cinemap_brief$tmdb_cast <- ""
cinemap_brief$producer <- ""
cinemap_brief$tmdb_crew <- ""

# Utility functions to DRY up and promote readability
movie_data <- function(title, year){
  query_result <- list(total_results = 0, results = NULL)
  if(title != ""){
    query_result <- search_movie(api_key = api_key_v3, query = title, year = year)
  }
  return(query_result)
}


start_time <- Sys.time()
last_title <- ""
repeated_title = FALSE
for(i in 1:length(cinemap_brief$film_title_en)) {
  if(cinemap_brief[i,]$film_title_en != last_title) {
    repeated_title = FALSE
    last_title <- cinemap_brief[i,]$film_title_en  # Don't send requests for duplicate titles
    print(paste("Working on ", last_title))
    
    # Work through the various titles to get a hit.
    result <- movie_data(cinemap_brief[i,]$film_title_en, cinemap_brief[i,]$year_released)
    Sys.sleep(sleep_time)
    if(result$total_results != 1) {
      result <- movie_data(cinemap_brief[i,]$film_title_romanji, cinemap_brief[i,]$year_released)
      Sys.sleep(sleep_time)
    }
    if(result$total_results != 1) {
      result <- movie_data(cinemap_brief[i,]$film_title_original, cinemap_brief[i,]$year_released)
      Sys.sleep(sleep_time)
    }
  }
  else {
    repeated_title = TRUE
  }
  
  
  if(result$total_results == 1) {
    # Get the info returned in the basic query
    cinemap_brief[i,]$tmdb_id <- result$results$id
    if(cinemap_brief[i,]$film_title_original == "") {
      cinemap_brief[i,]$film_title_original = result$results$original_title
    }
    if(cinemap_brief[i,]$year_released == "") {
      cinemap_brief[i,]$year_released = regmatches(result$results$release_date, regexpr("[0-9]{4}", result$results$release_date))
    }
    if(cinemap_brief[i,]$release_date == "") {
      cinemap_brief[i,]$release_date = result$results$release_date
    }
    if(cinemap_brief[i,]$tmdb_poster_url == "") {
      cinemap_brief[i,]$tmdb_poster_url = result$results$poster_path
    }
    
    # Get the movie details provided if it hasn't been retrieved already
    if(!repeated_title) {
      details <- movie(api_key = api_key_v3, result$results$id)
      Sys.sleep(sleep_time)
    }
    if(cinemap_brief[i,]$genres == "") {
      cinemap_brief[i,]$genres = paste(details$genres$name, collapse = "; ")
    }
    if(cinemap_brief[i,]$spoken_languages == "") {
      cinemap_brief[i,]$spoken_languages = paste(details$spoken_languages$iso_639_1, collapse = "; ")
    }
    if(cinemap_brief[i,]$production_studio == "") {
      cinemap_brief[i,]$production_studio = paste(details$production_companies$name, collapse = "; ")
    }
    if(cinemap_brief[i,]$country_of_production == "") {
      cinemap_brief[i,]$country_of_production = paste(details$production_countries$iso_3166_1, collapse = "; ")
    }
    if(cinemap_brief[i,]$imdb_id == "") {
      cinemap_brief[i,]$imdb_id = details$imdb_id
    }
    if(cinemap_brief[i,]$budget == "") {
      cinemap_brief[i,]$budget = details$budget
    }
    
    
    # Get alt title info and update it if needed.
    if(!repeated_title)
    {
      alt_titles <- movie_alternative_title(api_key = api_key_v3, id = result$results$id)
      Sys.sleep(sleep_time)
    }
    if(cinemap_brief[i,]$film_title_romanji == "" || cinemap_brief[i,]$film_title_original == "")
    {
      # alt_titles <- movie_alternative_title(api_key = api_key_v3, id = result$results$id)
      # Sys.sleep(sleep_time)
      alt_titles <- alt_titles$titles$title[alt_titles$titles$iso_3166_1 == "JP"]
      if(cinemap_brief[i,]$film_title_romanji == "") {
        cinemap_brief[i,]$film_title_romanji = alt_titles[!grepl(japanese_char_regex, alt_titles)]
      }
      if(cinemap_brief[i,]$film_title_original == "") {
        cinemap_brief[i,]$film_title_original = alt_titles[grepl(japanese_char_regex, alt_titles)]
      }
    }
    
    # Get cast and crew
    if(!repeated_title)
    {
      cast_and_crew <- movie_credits(api_key = api_key_v3, id = result$results$id)
      Sys.sleep(sleep_time)
    }

    if(cinemap_brief[i,]$tmdb_cast == "") {
      cinemap_brief[i,]$tmdb_cast <- toJSON(cast_and_crew$cast)
    }
    if(cinemap_brief[i,]$director == "") {
      cinemap_brief[i,]$director <- paste(cast_and_crew$crew[ which(cast_and_crew$crew$job == "Director"),]$name, collapse =  "; ")
    }
    if(cinemap_brief[i,]$producer == "") {
      cinemap_brief[i,]$producer <- paste(cast_and_crew$crew[ which(cast_and_crew$crew$job == "Producer"),]$name, collapse =  "; ")
    }
    if(cinemap_brief[i,]$tmdb_crew == "") {
      cinemap_brief[i,]$tmdb_crew <- toJSON(cast_and_crew$crew)
    }
    
  }
}
end_time <- Sys.time()



print(end_time - start_time)

write.csv(cinemap_brief, file = "data/cinemap.csv")

#regmatches(string, regexpr(pattern, string)) 


