#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J TWL_2010_2011
#SBATCH -t 2:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=60000

cd /tigress/efuller/raw_infoMap

Rscript 4_knn_classify.R 2010 "TWL" 2011 "modified"
