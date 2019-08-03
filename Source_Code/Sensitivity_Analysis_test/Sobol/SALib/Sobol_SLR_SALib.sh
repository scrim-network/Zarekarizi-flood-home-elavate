#!/bin/bash

# Set up variables
PARAMS="params.txt"                     # parameters sampling bounds
SOBOLPAR="sobolParameterSets.txt"       # parameter sets generated from Sobol Sequence
MODEL_EXE="Sobol_SLR_SALib.R"       # name of the executable
OUTPUT_FILE="sobolIndices"              # output file containing Sobol indices

NUMOB=1                                 # number of objectives
OBJS=$(seq 1 ${NUMOB})

# Generate parameter inputs using R Script [parameter name, lower bound, upper bound]
# If generating parameter inputs manually, comment out the following line line:
#Rscript ./Parameters/params.R

# Generate parameter sets using Sobol Sequence
echo "Generating parameter sets using Sobol Sequence"
cd ./SALib-master

python -m SALib.sample.saltelli \
     -n 1000 \
     -p ../Parameters/params.txt \
     -o ../sobolParameterSets.txt\
     --delimiter=' ' \
     --precision=8 \
     --max-order=2
    # Options:
    # -p, --paramfile: Your parameter range file (3 columns: parameter name, lower bound, upper bound)
    # -n, --samples: Sample size.
    #				 Number of model runs is N(2D + 2) if calculating second-order indices (default)
    #                or N(D + 2) otherwise.
    # -o, --output: File to output your samples into.
    # --delimiter (optional): Output file delimiter.
    # --precision (optional): Digits of precision in the output file. Default is 8.
    # --max-order (optional): Maximum order of indices to calculate. Choose 1 or 2, default is 2.
    #								   Choosing 1 will reduce total model runs from N(2D + 2) to N(D + 2)
    #								   Must use the same value (either 1 or 2) for both sampling and analysis.

# Run the model using the inputs sampled above
echo "Computation of Sobol Indices"
cd ../
Rscript Sobol_SLR_SALib.R

# Then use the output (objectiveValues.txt) to run the analysis.
# Sensitivity indices will written to Output folder
echo "Analyzing model output..."
cd ./SALib-master

OBJ_NAMES=("total_costs") # Create vector of objective names
for N in $(seq 1 1)
do
NAME=${OUTPUT_FILE}_${OBJ_NAMES[N-1]}
let "OB=${N}-1"

python -m SALib.analyze.sobol \
       -p ../Parameters/params.txt\
       -Y ../objectiveValues.txt \
       -c ${OB} \
       --max-order=2 \
       -r 10000 > ../Output/${NAME}.txt

done

echo "Sobol Analysis complete"

# Options:
# -p, --paramfile: Your parameter range file (3 columns: parameter name, lower bound, upper bound)
# -Y, --model-output-file: File of model output values to analyze
# -c, --column (optional): Column of model output file to analyze.
#                If the file only has one column, this argument will be ignored.
# --delimiter (optional): Model output file delimiter.
# --max-order (optional): Maximum order of indices to calculate.
#               This must match the value chosen during sampling.
# -r, --resamples (optional): Number of bootstrap resamples used to calculate confidence intervals on indices. Default 1000.
