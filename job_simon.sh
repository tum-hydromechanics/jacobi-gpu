#!/bin/bash
#SBATCH -J test01
#SBATCH -o ./%x.%j.out
#SBATCH -e ./%x.%j.err 
#SBATCH -D ./
#SBATCH --account=pn52gi
#SBATCH --time=00:08:00
#SBATCH --export=NONE
#SBATCH --partition=general
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mail-type=END
#SBATCH --mail-user=simon.wenczowski@tum.de
#SBATCH --get-user-env
#
module load intel-toolkit/2024.0.0 

export I_MPI_OFFLOAD=1
export I_MPI_OFFLOAD_IPC=0
export I_MPI_OFFLOAD_CELL_LIST=0,1
export I_MPI_OFFLOAD_L0_D2D_ENGINE_TYPE=1

mpiexec -n 1 ./main.exe 65536 131072 501
mpiexec -n 2 ./main.exe 65536 131072 501
mpiexec -n 4 ./main.exe 65536 131072 501 
