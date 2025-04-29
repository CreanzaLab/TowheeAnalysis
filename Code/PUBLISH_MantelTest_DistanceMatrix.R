######################### Mantel Test - Distance Matrix ########################
################################ Song Features #################################
#https://jkzorz.github.io/2019/07/08/mantel-test.html 

#load libraries
library(vegan)  
library(geosphere)  

#load data
rawdataforanalysis <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#to repeat on breeding season only:
#rawdataforanalysis <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#rawdataforanalysis <- rawdataforanalysis[,c(1,3:25)] 

#initialize dataframe to store results
mantel_results <- data.frame(SongFeature = character(), MantelStatistic = numeric(), PValue = numeric(), stringsAsFactors = FALSE)

#longitude and latitude
geo <- data.frame(rawdataforanalysis$Longitude, rawdataforanalysis$Latitude)

#calculate geographic distance matrix
d.geo <- distm(geo, fun = distGeo)
dist.geo <- as.dist(d.geo)

#list of song features to loop through
song_features <- c(colnames(rawdataforanalysis[,c(4:19)]))  # Add all song features you want to analyze

#loop through each song feature
for (feature in song_features) {
  #extract the song feature vector
  songfeature <- rawdataforanalysis[[feature]]
  
  #calculate Euclidean distance matrix for the song feature
  dist.songfeature <- dist(songfeature, method = "euclidean")
  
  #perform Mantel test
  mantel_result <- mantel(dist.geo, dist.songfeature, method = "spearman", permutations = 999, na.rm = TRUE)
  
  #store the results
  mantel_results <- rbind(mantel_results, data.frame(SongFeature = feature, MantelStatistic = mantel_result$statistic, PValue = mantel_result$signif))
}

print(mantel_results)


######################### Mantel Test - Distance Matrix ########################
############################## Genetic Distance ################################

#load libraries
library(seqinr)
library(geosphere)
library(vegan)

#load data
gendataforanalysis_longlat <- read.csv("~/PUBLISH_Genetic_Metadata_Spreadsheet.csv")
geo <- data.frame(gendataforanalysis_longlat$Longitude, gendataforanalysis_longlat$Latitude)
dist_geographic <- distm(geo, fun = distGeo)
dist_geographic <- as.dist(dist_geographic)

# Read the FASTA file
fasta_file <- "~/PUBLISH_aligned_fasta_spottedandeasterntowhee.fasta"
dist_genetic=dist.alignment(read.alignment(fasta_file,"fasta"))

mantel_test <- mantel(dist_genetic, dist_geographic, method = "spearman", permutations = 999)
print(mantel_test)
