metric_list=(FA MD L1 RD OD ICVF ISOVF)
test_list=(scd_story scd_age age_story)

cd tbss/stats

module load fsl/6.0.5
for metric in "${metric_list[@]}"; do
  for test in "${test_list[@]}"; do
    fsleyes -std1mm all_${metric} lower_cingulum_mask -cm green \
      ${metric}/${metric}_llc_${test}_tfce_corrp_tstat1 -cm red-yellow -dr 0.949 1 \
        ${metric}/${metric}_llc_${test}_tfce_corrp_tstat2 -cm blue-lightblue -dr 0.949 1 \
	        ${metric}/${metric}_rlc_${test}_tfce_corrp_tstat1 -cm red-yellow -dr 0.949 1 \
	          ${metric}/${metric}_rlc_${test}_tfce_corrp_tstat2 -cm blue-lightblue -dr 0.949 1 \
          
  done
done
