# qsub -cwd -V -N enmtml_model -pe thread 4 -m ea -M baptiste.rousseau@etu.u-paris.fr -b y "Rscript niches_ENMTML.R"

if (!require("ENMTML")) install.packages("ENMTML")
library(ENMTML)

ENMTML(pred_dir = "./data/WorldClim/1970",
       result_dir = "./results/ENMTML",
       occ_file = "./data/occurrences/Hirundo_rustica_1970/occurrence.txt",
       sp = "acceptedScientificName",
       x = "decimalLongitude",
       y = "decimalLatitude",
       colin_var = c(method = "PCA"),
       pseudoabs_method = c(method = "RND"),
       pres_abs_ratio = 1,
       part = c(method = "KFOLD", folds = "5"),
       algorithm = "MXD",
       thr = c(type = "MAX_TSS"),
       msdm = c(method = "KER"),
       cores = 4)
