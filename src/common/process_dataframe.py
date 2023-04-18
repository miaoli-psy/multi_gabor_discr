import pandas as pd


def insert_new_col(input_df: pd.DataFrame, old_col: str, new_col: str, func_name):
    if old_col in input_df.columns:
        col_index = input_df.columns.get_loc(old_col)
        input_df.insert(col_index, new_col, input_df[old_col].map(func_name))
    else:
        raise Exception(f"Warning: missing {old_col}")


def insert_new_col_from_two_cols(input_df: pd.DataFrame, old_col1: str, old_col2: str, new_col: str, func_name):
    cols = input_df.columns
    if (old_col1 in cols) and (old_col2 in cols):
        input_df[new_col] = input_df.apply(lambda x: func_name(x[old_col1], x[old_col2]), axis = 1)
    else:
        raise Exception(f"Warning: missing {old_col1} or {old_col2}")
