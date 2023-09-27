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
        count_n +=1

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