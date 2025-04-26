#!/bin/bash

#$ -l h_vmem=20G
#$ -l h_rt=0:15:00
#$ -o reports/

#$ -j y
#$ -cwd



path_to_file=$1

liftFrom=hg19
liftTo=hg38



source /broad/software/scripts/useuse
use R-4.1



Rscript --vanilla ../scripts/data_prep/liftOver.R $path_to_file $liftFrom $liftTo

#EOF