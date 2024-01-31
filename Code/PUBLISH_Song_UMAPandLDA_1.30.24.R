library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
library(umap)
library(MASS)

#use this to test all song data, NOT including hybrid/unsure samples:
UMAPdataforanalysis <- read.csv("C:/../PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv")

#use this line to perform UMAP on all song data, including hybrid/unsure samples:
#UMAPdataforanalysis <- read.csv("C:/../PUBLISH_All_XCandML_songdata_with_overlapzone_withHybridUnsure_1.26.24.csv")


UMAPdataforanalysis[,c(4:19)] <- log(UMAPdataforanalysis[,c(4:19)]) #log transform song data

#this is centered (subtract mean so that new mean becomes zero) and scaled (divide by standard dev.)
#to correct for differences in measurements so scale no longer matters:
UMAPdataforanalysis[,c(4:19)] <- scale(UMAPdataforanalysis[4:19], center = TRUE, scale = TRUE)


################################################################################
############################### UMAP + LDA #####################################
#LDA is used only for song data that did NOT include hybrid/unsure samples
#skip this section if only mapping the data that includes hybrid/unsure samples

randomstate <- c(57,36,92,13,46,83,30,22,79,64) #random states to loop through in for loop
umap_lda_results <- vector()

for (i in 1:length(randomstate)) {
  towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i) #LDA = 84.4%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=25) #test neighbors #LDA = 84.5%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=50) #test neighbors #LDA = 84.3%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, min_dist=0.5) #test min dist #LDA = 83.3%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, min_dist=0.9) #test min dist #LDA = 82.5%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=50, min_dist=0.9) #test both #LDA = 80.7%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=50, min_dist=0.5) #test both #LDA = 82.6%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=25, min_dist=0.9) #test both #LDA = 82.2%
  #towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=i, n_neighbors=25, min_dist=0.5) #test both #LDA = 83.2%
  TowheeUMAP = towhee.umap$layout
  colnames(TowheeUMAP) = c("UMAP1","UMAP2")
  ALL_Syll_Loc_Data_XCandML_UMAP = cbind(UMAPdataforanalysis, TowheeUMAP)
  fit <- lda(Species ~ UMAP1 + UMAP2, data=ALL_Syll_Loc_Data_XCandML_UMAP,
             na.action="na.omit", CV=TRUE) #LDA
  ct <- table(ALL_Syll_Loc_Data_XCandML_UMAP$Species, fit$class)
  diag(prop.table(ct, 1))
  loop_results <- sum(diag(prop.table(ct)))
  umap_lda_results[i] <- loop_results
}


avg_lda <- mean(umap_lda_results) #average lda of umap using 10 different random states

################################################################################
#for UMAP of data that include hybrid/unsure samples, run this line instead of for loop above: 
#towhee.umap <- umap(UMAPdataforanalysis[,4:19])


################################################################################
################################## Plot UMAP ###################################

#create df with labels
towhee.labels <- as.factor(UMAPdataforanalysis$SpeciesOverlap)
towhee.labelsdf <- as.data.frame(towhee.labels)

#create df with UMAP projection results
layout <- towhee.umap$layout
layoutdf <- as.data.frame(layout)

#bind columns together from layout and towhee.labels
plotUMAPpoints <- cbind(towhee.labelsdf, layoutdf)

####################
UMAPplot <- ggplot(plotUMAPpoints, aes(V1, V2, col = towhee.labels, fill = towhee.labels), show.legend = TRUE) +
  geom_point(shape = 21, col = "black", size = 3.0, alpha = 0.8) +
  scale_fill_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC", "Hybrid/Unsure" = "black")) +
  labs(title = "UMAP visualization of the towhee song dataset") + 
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10), legend.position = "top") +
  theme_classic()
UMAPplot

#save PNG 950 x 550 #save PDF 9.5 x 5.5 


