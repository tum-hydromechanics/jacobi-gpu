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

srun -n 4 ./main.exe 65536 131072 501 
srun -n 4 ./main.exe 131072 65536 501
srun -n 4 ./main.exe 262144 32768 501
srun -n 4 ./main.exe 524288 16384 501
srun -n 4 ./main.exe 1048576 8192 501
srun -n 4 ./main.exe 2097152 4096 501
srun -n 4 ./main.exe 4194304 2048 501
srun -n 4 ./main.exe 8388608 1024 501
srun -n 4 ./main.exe 16777216 512 501
srun -n 4 ./main.exe 33554432 256 501
srun -n 4 ./main.exe 67108864 128 501
