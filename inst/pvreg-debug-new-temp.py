def pvs_to_reff_df(pv_results, school_id):
    '''Transforms pv_results into a dataframe with pvs and a dataframe with school effect estimates'''
    
    pvs = pv_results[0]
    reffs_school = pv_results[1]
    reffs_school_cov = pv_results[2]
    m = len(reffs_school)
    reff_df = []
    for schl in reffs_school[0].keys():
        b = np.array([reffs_school[pv][schl] for pv in range(m)])
        b_est = np.mean(b, axis=0)
        W = np.zeros(shape=(len(b_est), len(b_est)))
        B = np.zeros_like(W)
        for pv in range(m):
            W += reffs_school_cov[pv][schl]
            B += np.outer(b[pv] - b_est, b[pv] - b_est)
        W /= m
        if m > 1:
            B /= m - 1
        V = W + ((1 + m) / m) * B
        b_est_se = np.sqrt(np.diag(V))
        reff_df.append([schl,
                        b_est[0],
                        b_est_se[0],
                        b_est[1],
                        b_est_se[1],
                        V.values.tolist(),
                        W.values.tolist(),
                        B.tolist(),
                        m
                        ])
    reff_df = pd.DataFrame(reff_df,
                           columns=[school_id, 'mean_schl', 'mean_schl_se', 'eva_schl', 'eva_schl_se', 'COV', 'W', 'B',
                                    'm'])
    pvs = pd.DataFrame(pvs)

    return reff_df, pvs

def pvreg(estimates_in, responses_in, # dataframes with uirt estimates and item responses - exam 0
          estimates_out, responses_out, # dataframes with uirt estimates and item responses - exam 1
          student_exog, # dataframe with student variables and school_id
          student_id, # str - name of student id column (common in responses_# and student_exog df)
          school_id, # str - name of school id column (in student_exog df)
          fixed_effects=[], # list with column names of exog variables to be included in fixed effects part (in student_exog df)
          npv = 10, # number of plausible values to be drawn
          keep_pv = True, # whether to keep plausible values
          csvname = '', # prefix used for saved csv files (can include a path)
          njobs = 1
         ):

    groups, items, responses, estimates_df, student_df, fixed_effects_all = load_data(estimates_in, responses_in,
                                                                   estimates_out, responses_out,
                                                                   student_exog,
                                                                   student_id, school_id, fixed_effects)
    pv_results = generate_pv_single(items, groups, responses, estimates_df,
                                    burn0=10, burn1=20, n_draw=npv, draw_delta=10, max_indep_chains=npv,
                                    keep_pv=keep_pv,
                                    keep_reff=True, mlv_data=student_df,
                                    mlv_args=[school_id, student_id, 'exam_var', fixed_effects_all],
                                    shift=-1,
                                    csvname=csvname)

    reffs, pvs = pvs_to_reff_df(pv_results, school_id)

    reffs['cov_mean_eva_schl'] = [x[0][1] for x in reffs.COV]
    reffs.to_csv(csvname+'reffs.csv', index=False,
                 columns=[school_id, 'mean_schl', 'mean_schl_se', 'eva_schl', 'eva_schl_se', 'cov_mean_eva_schl'])

    if keep_pv:
        pvs.columns = ['pv_'+str(i) for i in pvs.columns]
        pv_cols=[student_id, school_id, 'exam_var'] + pvs.columns.to_list()
        pvs[student_id] = student_df[student_id]
        pvs[school_id] = student_df[school_id] #unnecesary but useful
        pvs['exam_var'] = student_df['exam_var']
        pvs[pv_cols].to_csv(csvname+'pvs.csv', index=False)

 #   return reffs
