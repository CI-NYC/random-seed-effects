#!/bin/bash

cvfold=0
scen=simple

# first calculate the true value of the simulation
truth_run=$(qsub truth_calc.sh $scen $cvfold)

# run array script nine times to reach 9000 job limit
array_job0=$(qsub -W depend=afterany:$truth_run run_script2.sh 0 $scen $cvfold sim SL.ranger)
array_job1=$(qsub -W depend=afterany:$truth_run run_script2.sh 1 $scen $cvfold sim SL.ranger)
array_job2=$(qsub -W depend=afterany:$truth_run run_script2.sh 2 $scen $cvfold sim SL.ranger)
array_job3=$(qsub -W depend=afterany:$truth_run run_script2.sh 3 $scen $cvfold sim SL.ranger)
array_job4=$(qsub -W depend=afterany:$truth_run run_script2.sh 4 $scen $cvfold sim SL.ranger)
array_job5=$(qsub -W depend=afterany:$truth_run run_script2.sh 5 $scen $cvfold sim SL.ranger)
array_job6=$(qsub -W depend=afterany:$truth_run run_script2.sh 6 $scen $cvfold sim SL.ranger)
array_job7=$(qsub -W depend=afterany:$truth_run run_script2.sh 7 $scen $cvfold sim SL.ranger)
array_job8=$(qsub -W depend=afterany:$truth_run run_script2.sh 8 $scen $cvfold sim SL.ranger)


# run R script to check the files and see if any of the 90000 jobs did not run
check_work=$(qsub -W depend=afterany:$array_job8:$array_job7:$array_job6:$array_job5:$array_job4:$array_job3:$array_job2:$array_job1 check_run.sh $scen $cvfold SL.ranger)

# run an R script to re-run any missed seeds
run_missed_seeds=$(qsub -W depend=afterany:$check_work rerun.sh 0 $scen $cvfold rerun SL.ranger)

# calculate metrics from the simulation results
process_restuls=$(qsub -W depend=afterany:$run_missed_seeds process_results.sh $scen $cvfold SL.ranger)


