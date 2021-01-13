#!/bin/sh
#SBATCH --job-name=SimulacionEnsayos
#SBATCH --nodes=1
. /etc/profile

module load R
srun R --no-save --args 2 < SimulacionEnsayos/SimulacionDisenos.R