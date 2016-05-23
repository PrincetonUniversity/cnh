#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J TLS_2006
#SBATCH -t 1:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=50000

cd /tigress/efuller/raw_infoMap

Rscript 1_makeLinkList.R "TLS" 2006