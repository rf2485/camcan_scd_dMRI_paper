cd roi_diff_means/

measures = {'FA', 'ICVF', 'ISOVF', 'L1', 'MD', 'OD', 'RD'};
mask_file_pattern = fullfile(pwd, '*_mask.nii.gz');
mask_list = dir(mask_file_pattern);

for i = 1 : length(mask_list)
    mask_base_file_name = mask_list(i).name;
    mask_full_path = fullfile(mask_list(i).folder, mask_base_file_name);
    [~, mask_name, ~] = fileparts(mask_full_path);
    [mask_folder, mask_name, mask_extension] = fileparts(mask_name);
    mask = double(niftiread(mask_full_path));

    for j = 1 : length(measures)
        ctl_file_pattern = fullfile(measures(j), "ctl*.nii.gz");
        scd_file_pattern = fullfile(measures(j), "scd*.nii.gz");
        ctl_file_list = dir(ctl_file_pattern);
        scd_file_list = dir(scd_file_pattern);
        %create structure for storing the means
        ctl_meas_field = strcat('ctl_mean_', char(measures(j)), '_', char(mask_name));
        scd_meas_field = strcat('scd_mean_', char(measures(j)), '_', char(mask_name));
        ctl_means.(ctl_meas_field) = zeros([length(ctl_file_list) 1]);
        scd_means.(scd_meas_field) = zeros([length(scd_file_list) 1]);
        for k = 1 : length(ctl_file_list)
            ctl_base_file_name = ctl_file_list(k).name;
            ctl_full_path = fullfile(ctl_file_list(k).folder, ctl_base_file_name);
            ctl_image = double(niftiread(ctl_full_path));
            ctl_image_masked = ctl_image.*mask;
            ctl_image_masked(ctl_image_masked==0)=nan;
            ctl_means.(ctl_meas_field)(k) = mean(ctl_image_masked(:),'omitnan');
        end
        for l = 1 : length(scd_file_list)
            scd_base_file_name = scd_file_list(l).name;
            scd_full_path = fullfile(scd_file_list(l).folder, scd_base_file_name);
            scd_image = niftiread(scd_full_path);
            scd_image_masked = scd_image.*mask;
            scd_image_masked(scd_image_masked==0)=nan;
            scd_means.(scd_meas_field)(l) = mean(scd_image_masked(:), 'omitnan');
        end
    end
end

scd_table = struct2table(scd_means);
writetable(scd_table, 'scd_means_table.csv')
ctl_table = struct2table(ctl_means);
writetable(ctl_table, 'ctl_means_table.csv')

exit
