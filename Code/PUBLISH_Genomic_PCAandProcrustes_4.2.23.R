################################################################################
################################################################################
####################### Principal Component Analysis ###########################
library(dplyr)
library(adegenet)
library(ggplot2)


snp <- fasta2genlight("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_aligned_fasta_spottedandeasterntowhee.fasta", snpOnly=T) #reads alignments with fasta format and extracts SNPs
#glPlot(snp)

pca <- glPca(snp, nf=10) #PCA for genlight object #nf is the number of principal components to keep

##calculate percent variance explained
eig <- pca[["eig"]] 
eig[1] / sum(eig) #PC1 explains 43.1%
eig[2] / sum(eig) #PC2 explains 23.2%
##

#barplot(100*pca$eig/sum(pca$eig), main="Eigenvalues", col=heat.colors(length(pca$eig))) #makes bar plot of eigenvalues 

#fast plot
#scatter(pca, psi='bottomright') #makes plot of each sample along with the eigenvalue barplot 


pca.dataset = as.data.frame(pca$scores) #making a dataframe of just the PCA scores of the genetic data


pca.dataset_genetics_towhee <- tibble::rownames_to_column(pca.dataset, "Sequence") # Apply rownames_to_column


Sequence_Species <- subset(data.frame(do.call('rbind', strsplit(as.character(pca.dataset_genetics_towhee$Sequence),'|',fixed=TRUE))), select = c(X1,X2))
colnames(Sequence_Species) <- c('Sequence', 'Species')


PCA_with_ID <- cbind(Sequence_Species, pca.dataset) #PC1:PC10 info + ID

genetics_towhee_metadata_import <- read.csv('C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_Genetic_Metadata_Spreadsheet.csv')
genetics_towhee_metadata <- subset(genetics_towhee_metadata_import, select = c(Sequence, Latitude, Longitude))

PCA_with_ID_and_Coords <- merge(PCA_with_ID, genetics_towhee_metadata) #PC1:PC10 info + ID + coordinates



PCA_genetic_ggplot <- ggplot(PCA_with_ID_and_Coords, aes(PC1, PC2, colour = Species)) + geom_point(size=3, alpha=0.7) +   
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) #plotting PC1 vs PC2
PCA_genetic_ggplot


################################################################################
################################################################################
########################## Procrustes Projection ###############################
library(MCMCpack)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(vegan)

ID_Coords <- subset(PCA_with_ID_and_Coords, select = c(Latitude, Longitude, Species))


PCA_genetics_results_PC1andPC2 <- subset(PCA_with_ID_and_Coords, select = c(PC1, PC2))


world <- ne_countries(scale = "medium", returnclass = "sf") #obtaining map data for plotting


gen_coord_matrix<-as.matrix(cbind(ID_Coords$Longitude,ID_Coords$Latitude)) #creates a matrix in order for the procrustes to run #similar to dataframe 
gen_PCA_matrix<-as.matrix(cbind(PCA_genetics_results_PC1andPC2$PC1,PCA_genetics_results_PC1andPC2$PC2))


pro_gen <- procrustes(gen_PCA_matrix,gen_coord_matrix,translation=TRUE,dilation=TRUE) #matrix to be transformed (PCA scores), target matrix (real coordinates)
#dilation refers to scaling (zoom in or out/make larger or smaller); translation refers to moving so they have same centroid
#it also rotates to minimize squared distance between the two matrices
protest(gen_coord_matrix, gen_PCA_matrix)



pro_gen <- pro_gen$X.new #grabbing your new "coordinates" (i.e. those determined by procrustes) 
pro_gen <- data.frame(pro_gen) #creating dataframe with coordinates #switching container format from matrix to dataframe
procrustes_genetics_final <- as.data.frame(cbind(pro_gen, ID_Coords$Species)) #binding coordinates with species info
colnames(procrustes_genetics_final) <- c('Longitude', 'Latitude', 'Species') #changing column names (just my preference)


procrustes_genetics_ggplot <- ggplot() +                                                 
  geom_sf(data = world, fill = "white") + 
  geom_point(data = procrustes_genetics_final, aes(x = Longitude, y = Latitude, colour = Species), size = 3.0, alpha = 0.8) +
  coord_sf(xlim = c(-130, -65), ylim = c(0, 55), expand = FALSE) +
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10)) +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) 
procrustes_genetics_ggplot

