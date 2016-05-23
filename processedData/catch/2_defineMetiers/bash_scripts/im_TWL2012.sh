#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J TWL_2012
#SBATCH -t 1:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=50000

cd /tigress/efuller/raw_infoMap/Infomap

./Infomap -N 10 --clu -2 ../TWL2012.txt ..