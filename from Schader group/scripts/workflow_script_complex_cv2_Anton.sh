#!/bin/bash

# First calculate the true value of the simulation
truth_run=$(./truth_calc.sh $scen $cvfold)

# Run the script to process results after the previous command
# Since you don't need a job scheduler, you can run these commands directly

# Run the array script
./run_script2.sh 0 $scen $cvfold sim SL.ranger

# Process results
./process_results.sh $scen $cvfold SL.rangercd