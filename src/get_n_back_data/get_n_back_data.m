%% get the nback data across all participants in the data folder

% first get the list of files that we are iterating over
data_folder = 'C:\Users\kgarner\projects\routine-vs-habit_data-analysis\data\';
all_files = dir(fullfile(data_folder, '**', '*n-back-log.mat'));

% set up the saving info
res_folder = 'C:\Users\kgarner\projects\routine-vs-habit_data-analysis\res\';
res_file_name = 'routine_vs_habit_n_back_data_block.csv';
res_full_file_path = fullfile(res_folder, res_file_name);
fid = fopen(res_full_file_path, 'w');
fprintf(fid, 'sub,ses,block,hits,miss,fa,cor_rej\n');

% now start looping through the files
for ifile = 1:length(all_files)

    this_file_name = all_files(ifile).name;
    % get the subject number and block details
    pattern = 'sub-(?<sub>\w+)_ses-(?<ses>[^_]+)(?:_b-)?(?<mt>mt\d+)?';
    tokens = regexp(this_file_name, ...
        pattern,...
        'names');
    sub_id = tokens.sub;
    ses_id = tokens.ses;

    if isfield(tokens, 'mt') && ~isempty(tokens.mt)
        mt_id = tokens.mt;
    else
        mt_id = 'NA';   
    end

    % now load the data file
    full_file_path = fullfile(all_files(ifile).folder, this_file_name);
    data = load(full_file_path, "scores", "task");
    % get hits, misses, and correct rejections
    hits = data.scores.hit_count;
    miss = data.scores.miss_count;
    fa = data.scores.fa_count;
    % now we just need correct rejections
    no_target = data.task.is_target(:);
    no_target = sum(no_target == 0);
    correct_reject = no_target - fa; % this is all the times there was no target, 
    clear data

    fprintf(fid, '%s,%s,%s,%d,%d,%d,%d\n', ...
        sub_id, ses_id, mt_id, hits, miss, fa, correct_reject);

end

fclose(fid);