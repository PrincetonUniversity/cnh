#!/bin/sh
#SBATCH -N 1 # nodes=20
#SBATCH --ntasks-per-node=1 # ppn=20
#SBATCH -J TLS_2012 # job name
#SBATCH -t 5:00:00 # 1 hour walltime
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=45000 # in MB

cd /tigress/efuller/binary_classification/define_metiers

Rscript find_metiers.R TLS 2012
