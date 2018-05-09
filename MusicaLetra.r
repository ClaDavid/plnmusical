library("gdata")
library("reshape2")
library("lattice")
library("rasterVis")

all.the.files <- list.files("Lyrics/Country",full=TRUE)
country <- lapply( all.the.files,  readLines)

all.the.files2 <- list.files("Lyrics/Indie",full=TRUE)
indie <- lapply( all.the.files2,  readLines)

all.the.files3 <- list.files("Lyrics/Pop",full=TRUE)
pop <- lapply( all.the.files3,  readLines)

all.the.files4 <- list.files("Lyrics/Rap",full=TRUE)
rap <- lapply( all.the.files4,  readLines)

all.the.files5 <- list.files("Lyrics/Rock",full=TRUE)
rock <- lapply( all.the.files5,  readLines)

#data <- readLines("Lyrics/LadyGagaAlejandro.txt")




musicaLetra <- function(data){
  
  data <- tolower(gsub("(?!')[[:punct:]]", "", data, perl=TRUE))

  stopwordsOnly <- function(data){
    stopwords <- readLines("stopwords.txt")
    #stopwords <- c("love", "yeah", "feel", "girl", "heart", "take", "life", "back", "never", "die", "away", "give", "time", "night", "day", "man", "dream", "world", "little", "baby")
    paste(intersect(strsplit(data, "\\s")[[1]], stopwords), collapse=" ")
  }

dataTransformada <- lapply(data,stopwordsOnly)
dataVetor <- unlist(dataTransformada, use.names=FALSE)
dataVetor <- dataVetor[!dataVetor %in% ""]
letraStopwords <- paste(dataVetor, collapse = ' ')


teste <- strsplit(letraStopwords, " ")
teste <- unlist(teste, use.names=FALSE)
#length(teste[[1]])
lista <- list()
for(i in 1:length(teste)){
  lista[[i]] <- list(teste[i], teste[i + 1])
}

seqStopwords <- lapply(lista, function(x) unlist(x, use.names = FALSE))

  return (seqStopwords)

}

countryList <- list()
indieList <- list()
popList <- list()
rapList <- list()
rockList <- list()

for(i in 1:10){
  countryList[[i]] <- musicaLetra(country[[i]])
  indieList[[i]] <- musicaLetra(indie[[i]])
  popList[[i]] <- musicaLetra(pop[[i]])
  rapList[[i]] <- musicaLetra(rap[[i]])
  rockList[[i]] <- musicaLetra(rock[[i]])
}

#entre os proprios generos
countryMatriz = matrix(0L, nrow = 10, ncol = 10)
indieMatriz = matrix(0L, nrow = 10, ncol = 10)
popMatriz = matrix(0L, nrow = 10, ncol = 10)
rapMatriz = matrix(0L, nrow = 10, ncol = 10)
rockMatriz = matrix(0L, nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    
      
      countryMatriz[i, j] = length(intersect(countryList[[i]], countryList[[j]]))
      indieMatriz[i, j] = length(intersect(indieList[[i]], indieList[[j]]))
      popMatriz[i, j] = length(intersect(popList[[i]], popList[[j]]))
      rapMatriz[i, j] = length(intersect(rapList[[i]], rapList[[j]]))
      rockMatriz[i, j] = length(intersect(rockList[[i]], rockList[[j]]))
      
    
  }
}

#entre os outros generos
countryIndie = matrix(0L, nrow = 10, ncol = 10)
countryPop = matrix(0L, nrow = 10, ncol = 10)
countryRap = matrix(0L, nrow = 10, ncol = 10)
countryRock = matrix(0L, nrow = 10, ncol = 10)
indiePop = matrix(0L, nrow = 10, ncol = 10)
indieRap = matrix(0L, nrow = 10, ncol = 10)
indieRock = matrix(0L, nrow = 10, ncol = 10)
popRap = matrix(0L, nrow = 10, ncol = 10)
popRock = matrix(0L, nrow = 10, ncol = 10)
rapRock = matrix(0L, nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    countryIndie[i, j] = length(intersect(countryList[[i]], indieList[[j]]))
    countryPop[i, j] = length(intersect(countryList[[i]], popList[[j]]))
    countryRap[i, j] = length(intersect(countryList[[i]], rapList[[j]]))
    countryRock[i, j] = length(intersect(countryList[[i]], countryList[[j]]))
    indiePop[i, j] = length(intersect(indieList[[i]], popList[[j]]))
    indieRap[i, j] = length(intersect(indieList[[i]], rapList[[j]]))
    indieRock[i, j] = length(intersect(indieList[[i]], rockList[[j]]))
    popRap[i, j] = length(intersect(popList[[i]], rapList[[j]]))
    popRock[i, j] = length(intersect(popList[[i]], rockList[[j]]))
    rapRock[i, j] = length(intersect(rapList[[i]], rockList[[j]]))
    
  }
}


#entre eles proprios
countryVetor <- sum(upperTriangle(countryMatriz))
indieVetor <- sum(upperTriangle(indieMatriz))
popVetor <- sum(upperTriangle(popMatriz))
rapVetor <- sum(upperTriangle(rapMatriz))
rockVetor <- sum(upperTriangle(rockMatriz))

#entre outros
countryIndieVetor <- sum(upperTriangle(countryIndie))
countryPopVetor <- sum(upperTriangle(countryPop))
countryRapVetor <- sum(upperTriangle(countryRap))
countryRockVetor <- sum(upperTriangle(countryRock))
indiePopVetor <- sum(upperTriangle(indiePop))
indieRapVetor <- sum(upperTriangle(indieRap))
indieRockVetor <- sum(upperTriangle(indieRock))
popRapVetor <- sum(upperTriangle(popRap))
popRockVetor <- sum(upperTriangle(popRock))
rapRockVetor <- sum(upperTriangle(rapRock))

#visualizar

plotagem <- function(matriz){
  my.theme <- BuRdTheme()
  my.min <- min(matriz, matriz)
  my.max <- max(matriz, matriz)
  my.at <- seq(my.min, my.max, length.out=length(my.theme$regions$col)-1)
  my.ckey <- list(at=my.at, col=my.theme$regions$col)
  levelplot(matriz, par.settings=my.theme, at=my.at, xlab = "", ylab = "")
}


