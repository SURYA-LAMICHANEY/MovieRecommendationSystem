movies<-read.csv("movies.csv",stringsAsFactors = FALSE)
ratings<-read.csv("ratings.csv")

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

#Data pre-processing
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genreList <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genreMatrix <- matrix(0,10330,18) 
genreMatrix[1,] <- genreList 
colnames(genreMatrix) <- genreList

#Iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmatCol = which(genreMatrix[1,] == genres2[i,c])
    genreMatrix[i+1,genmatCol] <- 1
  }
}

#Convert into dataframe
genreMatrix2 <- as.data.frame(genreMatrix[-1,], stringsAsFactors=FALSE)
for (c in 1:ncol(genreMatrix2)) {
  genreMatrix2[,c] <- as.integer(genreMatrix2[,c])
}

#Create a matrix to search for a movie by genre
years <- as.data.frame(movies$title, stringsAsFactors=FALSE)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

searchMatrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genreMatrix2)
colnames(searchMatrix) <- c("movieId", "title", "year", genreList)

write.csv(searchMatrix, "search.csv")
searchMatrix <- read.csv("search.csv", stringsAsFactors=FALSE)

#Searching for an action movie produced in 1995 for example:
subset(searchMatrix, Action == 1 & year == 1995)$title

#Creating a user profile
binaryratings <- ratings

for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

#Converting binaryratings matrix to correct format
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] 

#Removing rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId))
ratingmovieIds <- length(unique(ratings$movieId))
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL

#Removing rows that are not rated from genreMatrix2
genreMatrix3 <- genreMatrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genreMatrix3) <- NULL

#Calculating dot product for User Profiles
result = matrix(0,18,668) 
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genreMatrix3)){
    result[i,c] <- sum((genreMatrix3[,i]) * (binaryratings2[,c])) 
  }
}

#Converting to Binary scale
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

# The User-Based Collaborative Filtering Approach
ratingmatrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmatrix <- as.matrix(ratingmatrix[,-1]) 

ratingmatrix <- as(ratingmatrix, "realRatingMatrix")

similarityUsers <- similarity(ratingmatrix[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarityUsers)
image(as.matrix(similarityUsers), main = "User similarity")

similarityItems <- similarity(ratingmatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarityItems)
image(as.matrix(similarityItems), main = "Item similarity")

# Exploring values of ratings:
vectorRatings <- as.vector(ratingmatrix@data)
unique(vectorRatings)

tableRatings <- table(vectorRatings) 
tableRatings

# Visualizing the rating:
vectorRatings<-vectorRatings[vectorRatings!=0]
vectorRatings<-factor(vectorRatings)
qplot(vectorRatings)+ggtitle("Distribution of the ratings")

# Exploring viewings of movies:
viewsPerMovie <- colCounts(ratingmatrix)
tableViews <- data.frame(movie = names(viewsPerMovie),
                          views = viewsPerMovie) 
tableViews <- tableViews[order(tableViews$views, 
                                 decreasing = TRUE), ] 

ggplot(tableViews[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == tableViews$movie)$title) +
  ggtitle("Number of views of the top movies")

#Visualizing the matrix:
image(ratingmatrix, main = "Heatmap of the rating matrix")
image(ratingmatrix[1:10, 1:15], main = "Heatmap of the first rows and columns")
image(ratingmatrix[rowCounts(ratingmatrix) > quantile(rowCounts(ratingmatrix), 0.99),
                colCounts(ratingmatrix) > quantile(colCounts(ratingmatrix), 0.99)], 
      main = "Heatmap of the top users and movies")

#Normalizing the data
ratingmatrixNorm <- normalize(ratingmatrix)
image(ratingmatrixNorm[rowCounts(ratingmatrixNorm) > quantile(rowCounts(ratingmatrixNorm), 0.99),
                     colCounts(ratingmatrixNorm) > quantile(colCounts(ratingmatrixNorm), 0.99)], 
      main = "Heatmap of the top users and movies")

#Create UBFC Recommender Model
recommenderModel <- Recommender(ratingmatrix, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

modelDetails <- getModel(recommenderModel)
modelDetails$data

recom <- predict(recommenderModel, 
                 ratingmatrix[1], 
                 n=10) 
recom
recomList <- as(recom,"list")

#Obtain recommendations
recomResult <- matrix(0,10)
for (i in 1:10){
  recomResult[i] <- as.character(subset(movies,movies$movieId == as.integer(recomList[[1]][i]))$title)

}

# Evaluation
# evalScheme<-evaluationScheme(ratingmatrix,
#                               method="cross-validation",
#                               k=5,given=3,
#                               goodRating=5)
# evalResults<-evaluate(evalScheme,
#                       method = "IBCF",
#                       n=c(1,3,5,10,15,20))
# evalResult<-getConfusionMatrix(evalResults)[[1]]
