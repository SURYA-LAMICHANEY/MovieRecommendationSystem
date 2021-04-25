library(proxy)
library(recommenderlab)
library(reshape2)
setwd("C:/Users/Sikkim/Desktop/RecommendationSystem/")
movies<-read.csv("movies.csv",header = TRUE,stringsAsFactors = FALSE)
ratings<-read.csv("ratings.csv", header = TRUE)
movies2<-movies[-which((movies$movieId %in% ratings$movieId)==FALSE),]

movieRecommendation<-function(input1,input2,input3) {
  #input = "Gladiator (2000)"
  #input2 = "Aeon Flux (2005)"
  #input3 = "Alexander (2004)"
  row1<-which(movies2[,'title']==input1)
  row2<-which(movies2[,"title"]==input2)
  row3<-which(movies2[,"title"]==input3)
  
  userSelect<-matrix(NA,nrow(movies2))
  userSelect[row1]<-5
  userSelect[row2]<-5
  userSelect[row3]<-5
  userSelect<-t(userSelect)
  
  ratingmatrix<-reshape2::dcast(ratings,userId~movieId, value.var = "rating",na.rm=FALSE)
  ratingmatrix<-ratingmatrix[,-1]
  colnames(userSelect)<-colnames(ratingmatrix)
  ratingmatrix2<-rbind(userSelect,ratingmatrix)
  ratingmatrix2<-as.matrix(ratingmatrix2)
 
   #converting rating matrix into sparse matrix
  ratingmatrix2<-as(ratingmatrix2,"realRatingMatrix")
  ratingmatrix2norm<-normalize(ratingmatrix2)
  
   #Creating the recommender model
  suppressWarnings(
  recommenderModel <- Recommender(ratingmatrix2norm, method = "UBCF",param=list(method="Cosine",nn=30)))
  suppressDependencies(
  recom <- predict(recommenderModel,ratingmatrix2[1], n=10)) 
  recomList <- as(recom, "list")
  noResult <- data.frame(matrix(NA,1))
  recomResult <- data.frame(matrix(NA,10))
  if (as.character(recomList[1])=='character(0)'){
    noResult[1,1] <- "Not enough information on the movies you have selected. Please select different movies"
    colnames(noResult) <- "No Results"
    return(noResult) 
  } else {
    for (i in 1:length(recomList[[1]])){
      recomResult[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(recomList[[1]][i]))$title)
    }
    colnames(recomResult) <- "Recommended movies for you"
    return(recomResult)
  }
}
 