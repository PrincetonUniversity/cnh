import sys

grgroup = ['POT','DRG', 'HKL', 'NET', 'TWL', 'TWS', 'MSC', 'TLS']
base_years = [2006, 2010, 2012]
class_years = range(2006,2016)

file_path = '/Users/efuller/Desktop/CNH/processedData/catch/2_defineMetiers/bash_scripts/'

# write base year scripts
for grgrp in grgroup:
	if grgrp == "POT":
		mem = '100000'
	else:
		mem = '50000'
	for yr in base_years:
		f = open(file_path + grgrp + str(yr) + '.sh', 'w')
		f.write('#!/bin/sh\n')
		f.write('#SBATCH -N 1\n')
		f.write('#SBATCH --ntasks-per-node=1\n')
		f.write('#SBATCH -J %s_%d\n' % (grgrp, yr))
		f.write('#SBATCH -t 1:00:00\n')
		f.write('#SBATCH --mail-user=efuller@princeton.edu\n')
		f.write('#SBATCH --mail-type=begin\n')
		f.write('#SBATCH --mail-type=end\n')
		f.write('#SBATCH --mem=%s\n' % (mem))
		f.write('\n')
		f.write('cd /tigress/efuller/raw_infoMap\n\n')
		f.write('Rscript 1_makeLinkList.R "%s" %d' % (grgrp, yr))
		f.close()

# write bash scripts to generate infomap clusters for base years

for grgrp in grgroup:
	if grgrp == 'POT':
		mem='60000'
		time = '10:00:00'
	elif grgrp == 'HKL':
		mem = '12000'
		time = '3:00:00'
	else:
		mem = '50000'
		time = '1:00:00'
	for yr in base_years:
		f = open(file_path + 'im_' + grgrp + str(yr) + '.sh', 'w')
		f.write('#!/bin/sh\n')
		f.write('#SBATCH -N 1\n')
		f.write('#SBATCH --ntasks-per-node=1\n')
		f.write('#SBATCH -J %s_%d\n' % (grgrp, yr))
		f.write('#SBATCH -t %s\n' %(time))
		f.write('#SBATCH --mail-user=efuller@princeton.edu\n')
		f.write('#SBATCH --mail-type=begin\n')
		f.write('#SBATCH --mail-type=end\n')
		f.write('#SBATCH --mem=%s\n\n' % (mem))
		f.write('cd /tigress/efuller/raw_infoMap/Infomap\n\n')
		f.write('./Infomap -N 10 --clu -2 ../%s%d.txt ..' % (grgrp, yr))

# write scripts to do k-nearest neighbor classification

for grgrp in grgroup:
	if grgrp == 'POT':
		time = '10:00:00'
		mem = '80000'
	else:
		time = '2:00:00'
		mem = '60000'
	for cyr in class_years:
		for byr in base_years:
			if (grgrp == 'NET') and (byr == 2006) and (cyr == 2011):
				mem = '80000'
			if byr == cyr:
				continue
			if (grgrp == 'NET') and (byr == 2006) and (cyr == 2011):
				mem = 80000
			f = open(file_path + 'class_%s%s_%s.sh' % (grgrp, byr, cyr), 'w')
			f.write('#!/bin/sh\n')
			f.write('#SBATCH -N 1\n')
			f.write('#SBATCH --ntasks-per-node=1\n')
			f.write('#SBATCH -J %s_%s_%s\n' % (grgrp, byr, cyr))
			f.write('#SBATCH -t %s\n' % (time))
			f.write('#SBATCH --mail-user=efuller@princeton.edu\n')
			f.write('#SBATCH --mail-type=begin\n')
			f.write('#SBATCH --mail-type=end\n')
			f.write('#SBATCH --mem=%s\n\n' % (mem))
			f.write('cd /tigress/efuller/raw_infoMap\n\n')
			f.write('Rscript 4_knn_classify.R %s "%s" %s "modified"\n' % (byr, grgrp, cyr))
