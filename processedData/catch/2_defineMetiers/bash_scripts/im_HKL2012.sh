#!/bin/sh
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -J HKL_2012
#SBATCH -t 3:00:00
#SBATCH --mail-user=efuller@princeton.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mem=12000

cd /tigress/efuller/raw_infoMap/Infomap

./Infomap -N 10 --clu -2 ../HKL2012.txt ..