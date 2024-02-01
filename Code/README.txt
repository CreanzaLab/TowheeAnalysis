README

This archive contains the data and code to run analyses of Eastern and Spotted Towhee songs.

PUBLISH_Wilcoxon_3.31.23.R 
This script performs Wilcoxon rank-sum tests to compare song features between the two species.

PUBLISH_Towhee_Hybrid_Unsure_RFM_RScript.R
PUBLISH_Towhee_Hybrid_Unsure_RFM_breeding_RScript.R
These scripts perform machine learning analyses of the potential hybrid songs and songs for which the recordist was unsure whether the singer was an Eastern or Spotted towhee.

PUBLISH_SpearmanRankCorrelation_3.30.23.R
This script performs rank correlation tests to compare song features across latitude and longitude.

PUBLISH_Song_PCAandProcrustesandLDA_1.26.24.R
This script generates principal components analyses of the song data and performs Procrustes analyses to determine whether the first two principal components are associated with geographic location (latitude, longitude). Additionally, it performs a linear discriminant analysis using PC1 and PC2 from the principal component analysis to best discriminate between the ‘Species’ classification of the song recordings.

PUBLISH_Genomic_PCAandProcrustes_4.2.23.R
These script generates principal components analyses of the genetic data and performs Procrustes analyses to determine whether the first two principal components are associated with geographic location (latitude, longitude). 

PUBLISH_RandomForest_TrainBreedingEdges_PredictOverlap_4.7.23.R
PUBLISH_RandomForest_TrainALLEdges_PredictOverlap_4.7.23.R
PUBLISH_RandomForest_TrainALLBreeding_PredictALLBreeding_4.7.23.R
PUBLISH_RandomForest_TrainALL_PredictALL_4.7.23.R
These scripts build random forest models trained on different subsets of the Eastern and Towhee songs and test them on either a subset of all data, the breeding season data, or only the data from the zone of overlap.

PUBLISH_OverlapZone_LinePlot_4.1.23.R
This script plots the towhee recordings by longitude to visualize the region of overlap.

PUBLISH_HybridSightingsMap.R
This script generates a map of the sightings of hybrid/unsure towhees.

PUBLISH_Song_UMAPandLDA_1.30.24.R
PUBLISH_Song_UMAPandLDA_Breeding_2.1.24.R
These scripts generate Uniform Manifold Approximation and Projections of the song data from either all samples or only samples from the breeding season and performs linear discriminant analysis to best discriminate between the ‘Species’ classification of the song recordings.


These data files are called in the preceding scripts:
PUBLISH_Towhee_Hybrid_Unsure_Song_Data_forAnalysis.csv
PUBLISH_Hybrid_ML_FilestoAnalyze_FINAL.csv
PUBLISH_BreedingTowhee_Spearman_Wilcoxon_Results.xlsx
PUBLISH_ALLTowhee_Spearman_Wilcoxon_Results.xlsx
PUBLISH_HybridTowheeUniqueSightingsBreedingSeason.csv
PUBLISH_Genetic_Metadata_Spreadsheet.csv
PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_4.1.23.csv
PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_withHybridUnsure_2.1.23.csv
PUBLISH_breedingseasonONLY_XCandML_raw_songdata_4.1.23.csv
PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv
PUBLISH_All_XCandML_songdata_with_overlapzone_withHybridUnsure_2.1.24.csv
PUBLISH_All_XCandML_raw_songdata_3.31.23.csv
PUBLISH_ALL_Towhee_Metadata_3.8.23.csv
PUBLISH_aligned_fasta_spottedandeasterntowhee.fasta
