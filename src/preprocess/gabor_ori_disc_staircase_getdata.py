import os
import pandas as pd

from src.common.process_dataframe import insert_new_col, insert_new_col_from_two_cols
from src.common.process_str import get_radial_tangential_con, get_ladder_snake_con, get_cons
from src.constant.gabor_ori_disc_staricase_constant import COL_multi_gabor_staircase

if __name__ == '__main__':
    to_excel = False

    PATH = "../../data/raw_data/"
    col = COL_multi_gabor_staircase

    # list data file
    files = os.listdir(PATH)

    # read raw data
    data_csv = [file for file in files if file.endswith(".csv")]

    # all raw data into a data frme
    df_list = list()
    for file_name in data_csv:
        data = pd.read_csv(PATH + file_name)
        df_list.append(data)
    totalData = pd.concat(df_list)

    # keep valid cols
    totalData = totalData[col]

    # drop nan rows
    totalData = totalData.dropna(subset = ['trials.thisN'])

    # threshold dataframe

    # get unique values for participants and block number from df
    participants = list(totalData['participant'].unique())
    blockN = list(totalData["blocks.thisN"].unique())

    # trial direction list
    direction_list = list()
    for index, row in totalData.iterrows():
        direction_list.append(row["trials.direction"])

    # get all the index of the reversal
    index_reversal = list()
    for i, direction in enumerate(direction_list):
        # pass start
        if direction != "start":
            # pass the last row
            if i+1 < len(direction_list):
                if direction != direction_list[i+1]:
                    index_reversal.append(i)
        if direction_list[-2] != direction_list[-1]:
            index_reversal.append(i-1)

    # get all reversal row in a list
    df_reversals_list = list()
    for i in index_reversal:
        df = totalData.iloc[[i]]
        df_reversals_list.append(df)

    # dataframe that contains all reversal rows
    df_reversals = pd.concat(df_reversals_list)

    # remove all reversals that thr = 10 (unable to do the task)
    df_reversals.drop(df_reversals[df_reversals["trials.intensity"] == 10].index, inplace=True)

    df_each_pp_list = list()
    for p in participants:
        df = df_reversals[df_reversals["participant"] == p]
        rever_list = list()
        for b in blockN:
            df_b = df[df["blocks.thisN"] == b]
            # get nback reversal n == 6 (the last 6 row of the df)
            df_rever = df_b.iloc[-6:]
            rever_list.append(df_rever)
        rever = pd.concat(rever_list)
        df_each_pp_list.append(rever)

    # final threshold list
    threshold_df = pd.concat(df_each_pp_list)

    # add useful cols
    insert_new_col(threshold_df, "trials.label", "r_t", get_radial_tangential_con)
    insert_new_col(threshold_df, "trials.label", "s_l", get_ladder_snake_con)

    insert_new_col_from_two_cols(threshold_df, "r_t", "s_l", "condition", get_cons)

    # separate into two exp

    exp1_df = threshold_df[threshold_df["participant"] <= 20]
    exp2_df = threshold_df[threshold_df["participant"] > 20]


    if to_excel:
        exp1_df.to_excel("prprcssed_mlti_gbr_sc_1.xlsx", index = False)
        exp2_df.to_excel("prprcssed_mlti_gbr_sc_2.xlsx", index = False)
