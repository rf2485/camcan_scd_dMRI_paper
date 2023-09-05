metric_list=(FA MD RD L1 ISOVF ICVF OD)
test_list=(scd_story scd_age age_story)

cd tbss/stats

for metric in "${metric_list[@]}"; do
  for test in "${test_list[@]}"; do
    fsleyes -std1mm lower_cingulum_mask -cm green \
      ${metric}/${metric}_lc_${test}_tfce_corrp_tstat1 -cm red-yellow -dr 0.949 1 \
        ${metric}/${metric}_lc_${test}_tfce_corrp_tstat1 -cm blue-lightblue -dr 0.949 1
  done
done
