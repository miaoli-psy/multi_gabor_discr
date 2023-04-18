# -*- coding: utf-8 -*- 
"""
Project: multi_gabor_discr
Creator: Miao
Create time: 2022-11-30 15:04
IDE: PyCharm
Introduction:
"""


def get_radial_tangential_con(colname):
    if "_r_" in colname:
        return "radial"
    elif "_t_" in colname:
        return "tangential"
    elif "setsize1_h" in colname:
        return "tangential"
    elif "setsize1_v" in colname:
        return "radial"
    else:
        return "check_col_name"


def get_ladder_snake_con(colname):
    if "ladder" in colname:
        return "ladder"
    elif "snake" in colname:
        return "snake"
    elif "setsize1_h" in colname:
        return "snake"
    elif "setsize1_v" in colname:
        return "ladder"
    else:
        return "check_col_name"


def get_cons(str1, str2):
    return str1 + "_" + str2