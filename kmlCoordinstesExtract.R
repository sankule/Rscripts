# extracting average coordinates from the KML files and replacing it with the CenterMarker and making a reference data with location and coordinate
library(sp)
library(maptools)
library(stringr)
setwd("/home/SWARIT/kmlfiles")
list.files()

rm(list=ls())
kmlfile <- "districts.kml"


options(digits=999)
cdata <- getKMLcoordinates(kmlfile, ignoreAltitude=TRUE)
new <- data.frame()
for(i in 1:length(cdata)){
  cdata[[i]] <- as.data.frame(cdata[[i]])
  new <- rbind(new, sapply(cdata[[i]], FUN = mean))
}
colnames(new) <- c("long", "lat")
new$coordinates <- paste(new$lat,new$long,sep = ",")
rm(cdata)
gc()

# Imputing average to kml ##
kml <- paste(readLines(kmlfile, encoding = "UTF-8"), collapse = " ")
kml <- gsub("[[:blank:]]+", " ", kml)

re <- "<coordinates> *([^<]+?) *<\\/coordinates>"
mtchs <- gregexpr(re, kml)[[1]]
mtchs
length(mtchs)
coordscount <- length(mtchs)

# trying for loop
if(length(mtchs)>1){
  for (i in 1:(length(mtchs))){
    substr(kml, (mtchs[i]-41), (mtchs[i] + (attr(mtchs, "match.length")[i] + 43))) <- 
      sub("<coordinates> *([^<]+?) *<\\/coordinates>",
          str_pad(paste0("<point><coordinates>", new$coordinates[i], "</coordinates></point>"), 
                  attr(mtchs, "match.length")[i] + 43 + 41, "right"),
          substr(kml, (mtchs[i]), (mtchs[i] + (attr(mtchs, "match.length")[i]))))
  }
}else{
  print("Pattern matching not found in present KML file")}

kml <- gsub("[[:blank:]]+", " ", kml)
re <- "<name> *([^<]+?) *<\\/name>"
mtchs <- gregexpr(re, kml)[[1]]
mtchs
length(mtchs)
gpnames <- as.character()
if(length(mtchs)>1){
  for (i in 3:(length(mtchs))){
    print(substr(kml, (mtchs[i]+6), (mtchs[i] + (attr(mtchs, "match.length")[i]-8))))
    gpnames <- rbind(gpnames, substr(kml, (mtchs[i]+6), (mtchs[i] + (attr(mtchs, "match.length")[i]-8))))
  }
}else{
  print("Pattern matching not found in present KML file")}

gpnames <- as.data.frame(gpnames)
gpnames$V1 <- gsub(" ", "", gpnames$V1, fixed = TRUE)

gpcoordinates <- cbind(gpnames, new$coordinates)
colnames(gpcoordinates) <- c("GramPanchayatNames", "coordinates")
gpcoordinates <- as.data.frame(gpcoordinates)
head(gpcoordinates,30)
save(gpcoordinates, file = "coordinates.dat")
write(kml, "kmlwithcentermarkers.kml")

