#!/bin/sh
#SBATCH -N 1 # nodes=20
#SBATCH --ntasks-per-node=1 # ppn=20
#SBATCH -J TWL_2010 # job name
#SBATCH -t 5:00:00 # 1 hour walltime
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=10000 # in MB

cd /tigress/efuller/binary_classification/define_metiers

Rscript find_metiers.R TWL 2010
