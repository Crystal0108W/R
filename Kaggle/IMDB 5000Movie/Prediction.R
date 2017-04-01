movie.df <- movie_metadata
str(movie.df)
movie.df$profit <- movie.df$gross - movie.df$budget
summary(movie.df$profit)
DirectorProfit.df <- movie.df[, c("director_name", "director_facebook_likes", "profit")]
