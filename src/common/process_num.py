import math


def get_innermost_resp(old_resp1, old_resp3, random_float):
    if random_float < 0.5:
        return old_resp3
    else:
        return old_resp1


def get_outermost_resp(old_resp1, old_resp3, random_float):
    if random_float < 0.5:
        return old_resp1
    else:
        return old_resp3


def get_final_angle(display_resp):
    if abs(display_resp) >= 90:
        while display_resp <= -90:
            display_resp += 180
            return display_resp
        while display_resp >= 90:
            display_resp -= 180
            return display_resp
    else:
        return display_resp


def check_ori(resp1, resp2, resp3):
    count_n = 0
    if resp1 < 0:
        count_n += 1

    if resp2 < 0:
        count_n += 1

    if resp3 < 0:
        count_n += 1

    if count_n >= 2:
        return "ccw"
    else:
        return "cw"


def check_ori_setsize1(resp1):
    if resp1 < 0:
        return "ccw"
    else:
        return "cw"


def get_real_ori(ori, cw_ccw):
    if cw_ccw == "cw":
        return ori
    else:
        return -ori


def get_display_resp_setsize1(resp):
    return resp


def get_resp_error(resp, orientation):
    return resp - orientation


def cal_reversal(cw_cww, resp_i, resp_m, resp_o):
    negative_count = count_negatives(resp_i, resp_m, resp_o)
    # If cw_cww is less than or equal to 0.5, the reversal is 3 minus the count of negatives
    # Otherwise, the reversal is equal to the count of negatives
    return 3 - negative_count if cw_cww <= 0.5 else negative_count


def get_resp_type(cw_cww, resp_i, resp_m, resp_o):
    negative_count = count_negatives(resp_i, resp_m, resp_o)

    if cw_cww <= 0.5:
        if negative_count == 3:
            return "correct_trend"
        elif 1 <= negative_count <= 2:
            return "reversal"
        else:
            return "wrong_trend"
    else:
        if negative_count == 0:
            return "correct_trend"
        elif 1 <= negative_count <= 2:
            return "reversal"
        else:
            return "wrong_trend"


def count_negatives(*values):
    # Count the number of negative values
    negative_count = sum(1 for v in values if v < 0)
    return negative_count


def get_abs_value_diff(a, b):
    return abs(a - b)


def get_sum(a, b):
    return a + b


def non_uniformity_cv(resp_i, resp_m, resp_o):
    mean = (resp_i + resp_m + resp_o) / 3
    if mean == 0:
        return float('inf')  # Avoid division by zero; handle this as per your use case
    std_deviation = math.sqrt(
        ((resp_i - mean) ** 2 + (resp_m - mean) ** 2 + (resp_o - mean) ** 2) / 3)
    cv = std_deviation / mean
    return cv
