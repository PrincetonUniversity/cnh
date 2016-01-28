#!/bin/sh
#SBATCH -N 1 # nodes=20
#SBATCH --ntasks-per-node=1 # ppn=20 
#SBATCH -J knn_classify # job name 
#SBATCH -t 1:00:00 # 1 hour walltime 
#SBATCH --mail-user=efuller@princeton.edu 
#SBATCH --mail-type=begin 
#SBATCH --mail-type=end 
#SBATCH --mem=40000 # in MB

Rscript /tigress/efuller/metierAnalysis_final/classify_trips/2_knn_classify.R 2010 TLS 2009

