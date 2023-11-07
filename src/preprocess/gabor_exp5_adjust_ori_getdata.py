import pandas as pd

from common.process_dataframe import insert_new_col_from_four_cols, insert_new_col_from_two_cols, \
    insert_new_col_from_three_cols
from common.process_num import cal_reversal, get_resp_type, get_abs_value_diff, get_sum, \
    non_uniformity_cv

if __name__ == '__main__':
    PATH = "../../data/"
    file_name = "gabor_adjst_ori_alldata.csv"
    to_csv = False

    my_data = pd.read_csv(PATH + file_name)
    # add col "resp_type": if all 3 resps
    insert_new_col_from_four_cols(my_data, "cw_ccw", "inner_resp", "midd_resp", "outer_resp", "resp_type",
                                  get_resp_type)
    # add col "resp_variance"
    insert_new_col_from_two_cols(my_data, "inner_resp", "midd_resp", "resp_variance_a",
                                 get_abs_value_diff)
    insert_new_col_from_two_cols(my_data, "midd_resp", "outer_resp", "resp_variance_b",
                                 get_abs_value_diff)
    insert_new_col_from_two_cols(my_data, "resp_variance_a", "resp_variance_b", "resp_variance",
                                 get_sum)
    # add col "non_uniformity_cv"
    insert_new_col_from_three_cols(my_data, "inner_resp", "midd_resp", "outer_resp", "non_uniformity_cv",
                                   non_uniformity_cv)
    # add col "n_reversal"
    insert_new_col_from_four_cols(my_data, "cw_ccw", "inner_resp", "midd_resp", "outer_resp", "n_reversal",
                                  cal_reversal)

    if to_csv:
        my_data.to_csv("gabor_adjst_ori_alldata2.csv", index = False)
