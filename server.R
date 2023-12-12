get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# Define custom IBCF function
myIBCF = function(user_ratings){
  # Download the S matrix from server
  S_downloaded = read.csv("https://raw.githubusercontent.com/Vivek2696/movie-recommender/main/S.csv", row.names = 1)
  S_online = as.matrix(S_downloaded)
  
  movie_id_names = paste("m",user_ratings$MovieID, sep = "")
  new_user = matrix(NA, nrow = 1, ncol = ncol(S_online))
  colnames(new_user) = colnames(S_online)
  new_user[1, movie_id_names] = user_ratings$Rating
  
  w = new_user
  wi_non_na = which(!is.na(w))
  wi_na = which(is.na(w))
  wi_pred = w
  
  for(i in 1:length(wi_na)){
    current_pred_index = wi_na[i]
    Si_non_na = as.vector(which(!is.na(S_online[current_pred_index, ])))
    intersected_non_na = intersect(wi_non_na, Si_non_na)
    
    if(length(intersected_non_na) > 0){
      numerator = sum(as.vector(S_online[current_pred_index, intersected_non_na]) * w[intersected_non_na])
      denominator = sum(as.vector(S_online[current_pred_index, intersected_non_na]))
      prediction = numerator / denominator
      wi_pred[current_pred_index] = prediction
    }
  }
  
  # removing the user rated movies from the predictions
  wi_pred[wi_non_na] = NA
  top_10_indices = order(wi_pred, decreasing = TRUE)[1:10]
  
  #converting back the indices to movie ids
  movies_pred = colnames(S_online[, top_10_indices])
  movies_pred_ids = as.numeric(gsub("\\D", "", movies_pred))
  return(movies_pred_ids)
}


# Define custom genre recommendation function
myGenreRecommender <- function(movies, ratings, user_genre){
  # Group by movies all the user ratings and rate all movies
  movie_ratings = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')
  
  # filter by genre
  movie_selections = movie_ratings[grep(user_genre, movie_ratings$Genres),]
  
  movies_by_genre = arrange(movie_selections, desc(ratings_per_movie))
  
  return(movies_by_genre)
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings_dat = ratings[(!is.na(ratings$Rating))&(ratings$Rating<=5),]

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      user_results = (1:10)/10
      user_predicted_ids = myIBCF(user_ratings)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # Calculate recommendations when input genre selected
  df_genre <- eventReactive(input$genrebtn, {
    withBusyIndicatorServer("genrebtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's genre from input
      user_genre <- input$user_genre
      
      # Get the genre and retrun to ui func
      recom_pop <- myGenreRecommender(movies, ratings_dat, user_genre)
      
    }) # still busy
    
  }) # clicked on button for genre-based recommendation
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # display the genre-based recommendations
  output$rec_genre_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_pop <- df_genre()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_pop$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_pop$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function for genre 
  
}) # server function