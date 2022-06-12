# qsub -cwd -V -N enmtml_model -pe thread 4 -m ea -M baptiste.rousseau@etu.u-paris.fr -b y "Rscript niches_ENMTML.R"

if (!require("ENMTML")) install.packages("ENMTML")
library(ENMTML)

ENMTML(pred_dir = "~/save/data/WorldClim/1970",
       result_dir = "~/work/results/ENMTML",
       occ_file = "~/save/data/occurrences/Hirundo_rustica_1970/occurrence.txt",
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

# Checking for function arguments ...
# Loading environmental variables ...
# RasterBrick successfully created!
# Performing a reduction of variables collinearity ...
# Avis dans dir.create(DirPCA) :
#   '/home/brousseau/save/data/WorldClim/1970/PCA' existe déjà
# Avis dans dir.create(DirPCATab) :
#   '/home/brousseau/save/data/WorldClim/1970/PCA/Tables' existe déjà
# Avis dans if (tolower(e) %in% c(".tiff", ".tif")) { :
#   la condition a une longueur > 1 et seul le premier élément est utilisé
# Avis dans ENMTML(pred_dir = "~/save/data/WorldClim/1970", result_dir = "~/work/results/ENMTML",  :
#   The minimum number of occurrences is smaller than the number of predictors.
#             This may cause some issues while fitting certain algorithms!
# Loading and processing species occurrence data ...
# Result folder already exists, files may be overwritten!
# Avis dans dir.create(DirR, recursive = T) :
#   '/home/brousseau/work/results/ENMTML' existe déjà
# Results can be found at:
# ~/work/results/ENMTML
# Erreur dans scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
#   la ligne 1 n'avait pas 257 éléments
