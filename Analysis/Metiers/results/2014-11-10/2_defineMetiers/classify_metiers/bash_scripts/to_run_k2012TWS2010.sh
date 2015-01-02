#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J knn_2012TWS2010
#SBATCH -t 1:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=40000
cd /tigress/efuller/binary_classification/classify_metiers
Rscript 2_knn_classify.R  2012 TWS 2010 modified
