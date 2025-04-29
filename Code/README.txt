README

This archive contains the data and code to run analyses of Eastern and Spotted Towhee songs.

PUBLISH_OverlapZone_LinePlot.R
This script plots the towhee recordings by longitude to visualize the region of overlap.

PUBLISH_HybridSightingsMap.R
This script generates a map of the sightings of hybrid/unsure towhees.

PUBLISH_GLM_GLMM_Wilcoxon.R
This script generates Generalized Linear Models to model the relationship between the song feature data and longitude, latitude, species classification, and file conversion status.

PUBLISH_LDA.R
This script trains a linear discriminant analysis on different subsets of the Eastern and Spotted towhee songs and tests them on either a subset of all data or a subset of the breeding season data.

PUBLISH_Song_PCAandLDA.R
This script generates principal components analyses of the song data and performs Procrustes analyses to determine whether the first two principal components are associated with geographic location (latitude, longitude). Additionally, it performs a linear discriminant analysis using PC1 and PC2 from the principal component analysis to best discriminate between the ‘Species’ classification of the song recordings.

PUBLISH_UMAP_LDA_ALLUnique.R
PUBLISH_UMAP_LDA_BreedingUnique.R
These scripts generate Uniform Manifold Approximation and Projections of the song data from either all samples or only samples from the breeding season and performs linear discriminant analysis to best discriminate between the ‘Species’ classification of the song recordings.

PUBLISH_RandomForest_TrainEdges_PredictOverlap.R
PUBLISH_RandomForest_TrainALL_PredictALL.R
These scripts build random forest models trained on different subsets of the Eastern and Spotted towhee songs and test them on either a subset of all data, the breeding season data, only the data from the zone of overlap, or the potential hybrid songs and songs for which the recordist was unsure whether the singer was an Eastern or Spotted towhee..

PUBLISH_GradientBoosting.R
This script builds a gradient boosting machine trained on different subsets of the Eastern and Spotted towhee songs and tests them on either a subset of all data or a subset of the breeding season data.

PUBLISH_Torch_Deep_Learning.R
This script builds deep learning binary classification models trained on different subsets of the Eastern and Spotted towhee songs and tests them on either a subset of all data or a subset of the breeding season data.

PUBLISH_Audio_Classification_CNN_Towhees.ipynb
This script builds a convolutional neural network trained on spectrograms of the Eastern and Spotted towhee songs and tests them on a subset of all data.

PUBLISH_Genomic_PCAandProcrustes.R
These script generates principal components analyses of the genetic data and performs Procrustes analyses to determine whether the first two principal components are associated with geographic location (latitude, longitude).

PUBLISH_MantelTest_DistanceMatrix.R
This script generates a Mantel test on the song feature distance matrix versus the geographic distance matrix of the Spotted and Eastern towhee song data. It also generates a Mantel test on the genetic distance matrix versus the geographic distance matrix of the aligned genetic sequences from the Spotted and Eastern towhee mitochondrial genomes.

SupplementalMethods_GLM_PC1andPC2_UMAP1and2_ConversionStatus.R
SupplementalMethods_LDA_ConversionStatus.R
SupplementalMethods_PCA_LDA_ConversionStatus.R
SupplementalMethods_UMAP_LDA_ConversionStatus.R
SupplementalMethods_RFM1_ConversionStatus.R
SupplementalMethods_RFM2_ConversionStatus.R
SupplementalMethods_GBM_ConversionStatus.R
These scripts are modified versions of our above analyses that incorporate conversion status as a variable.

SupplementalMethods_StackedBarCharts_ConversionStatus.R
This script plots the species distributions of different audio file properties to assess whether there is file-type bias or sampling rate conversion bias in our analyses.


These data files are called in the preceding scripts:
PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv
PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv
PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv
PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv
PUBLISH_HybridTowheeUniqueSightings.csv
PUBLISH_HybridTowheeUniqueSightingsBreedingSeason.csv
PUBLISH_Genetic_Metadata_Spreadsheet.csv
PUBLISH_aligned_fasta_spottedandeasterntowhee.fasta
