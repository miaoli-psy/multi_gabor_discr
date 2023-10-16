import pandas as pd

from src.common.process_dataframe import insert_new_col, insert_new_col_from_two_cols
from src.common.process_str import get_ladder_snake_con, get_radial_tangential_con, \
    process_unique_block_gabor3, get_cons

if __name__ == '__main__':
    to_excel = False
    exp = "exp4"

    PATH = "../../data/"
    if exp == "exp3":
        DATA_FILE = "gabor_2tasks_exp3_alldata.csv"
    elif exp == "exp4":
        DATA_FILE = "gabor_colored_exp4_alldata.csv"

    # read data (pre-pre processed in R)
    totalData = pd.read_csv(PATH + DATA_FILE, sep = ",")


    # get unique values for participants and block number from df
    participants = list(totalData['participant'].unique())
    blockN = list(totalData["thisN"].unique())

    # trial direction list
    direction_list = list()
    for index, row in totalData.iterrows():
        direction_list.append(row["direction"])

        # get all the index of the reversal
    index_reversal = list()
    for i, direction in enumerate(direction_list):
        # pass start
        if direction != "start":
            # pass the last row
            if i + 1 < len(direction_list):
                if direction != direction_list[i + 1]:
                    index_reversal.append(i)
        if direction_list[-2] != direction_list[-1]:
            index_reversal.append(i - 1)

    # get all reversal row in a list
    df_reversals_list = list()
    for i in index_reversal:
        df = totalData.iloc[[i]]
        df_reversals_list.append(df)

    # dataframe that contains all reversal rows
    df_reversals = pd.concat(df_reversals_list)

    # remove all reversals that thr = 10 (unable to do the task)
    # df_reversals.drop(df_reversals[df_reversals["intensity"] == 10].index, inplace=True)

    df_each_pp_list = list()
    for p in participants:
        df = df_reversals[df_reversals["participant"] == p]
        rever_list = list()
        for b in blockN:
            df_b = df[df["thisN"] == b]
            # get nback reversal n == 6 (the last 6 row of the df)
            df_rever = df_b.iloc[-6:]
            rever_list.append(df_rever)
        rever = pd.concat(rever_list)
        df_each_pp_list.append(rever)

    # final threshold list
    threshold_df = pd.concat(df_each_pp_list)

    if to_excel:
        if exp == "exp3":
            threshold_df.to_excel("gbr_sc_threshold3.xlsx", index = False)
        elif exp == "exp4":
            threshold_df.to_excel("gbr_sc_threshold4.xlsx", index=False)
