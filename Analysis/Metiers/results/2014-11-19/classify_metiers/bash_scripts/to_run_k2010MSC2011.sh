#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J knn_2010MSC2011
#SBATCH -t 1:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=40000
cd /tigress/efuller/binary_classification/classify_metiers
Rscript 2_knn_classify.R  2010 MSC 2011 modified
