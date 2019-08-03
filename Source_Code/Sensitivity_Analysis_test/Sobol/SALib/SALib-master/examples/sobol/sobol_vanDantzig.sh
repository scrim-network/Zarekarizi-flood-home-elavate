#!/bin/bash

# Example: generating samples from the command line
#cd ../../ # hack
python -m ./SALib-master/SALib.sample.saltelli \
     -n 1000 \
     -p ./SALib/test_functions/params/params.txt \
     -o ../sobolParameterSets.txt \
     --delimiter=' ' \
     --precision=8 \
     --max-order=2

# Options:
# -p, --paramfile: Your parameter range file (3 columns: parameter name, lower bound, upper bound)
#
# -n, --samples: Sample size. 
#				 Number of model runs is N(2D + 2) if calculating second-order indices (default) 
#        or N(D + 2) otherwise.
#
# -o, --output: File to output your samples into.
# 
# --delimiter (optional): Output file delimiter.
#
# --precision (optional): Digits of precision in the output file. Default is 8.
#
# --max-order (optional): Maximum order of indices to calculate. Choose 1 or 2, default is 2. 
#								   Choosing 1 will reduce total model runs from N(2D + 2) to N(D + 2)
#								   Must use the same value (either 1 or 2) for both sampling and analysis.

# Run the model using the inputs sampled above, and save outputs

echo "Running van Dantzig model"
Rscript Sobol_vanDantzig.R

# Then use the output to run the analysis.
# Sensitivity indices will print to command line. Use ">" to write to file.

echo "Analyzing model output"
python -m ./SALib-master/SALib.analyze.sobol \
     -p ./SALib/test_functions/params/params.txt \
     -Y ../objectiveValues.txt \
     -c 0 \
     --max-order=2 \
     -r 100 > ../sobolIndices.txt

echo "Sobol Analysis complete"
# Options:
# -p, --paramfile: Your parameter range file (3 columns: parameter name, lower bound, upper bound)
#
# -Y, --model-output-file: File of model output values to analyze
#
# -c, --column (optional): Column of model output file to analyze. 
#                If the file only has one column, this argument will be ignored.
#
# --delimiter (optional): Model output file delimiter.
#
# --max-order (optional): Maximum order of indices to calculate.
#               This must match the value chosen during sampling.
#
# -r, --resamples (optional): Number of bootstrap resamples used to calculate confidence intervals on indices. Default 1000.
