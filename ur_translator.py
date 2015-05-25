import csv

file = 'haskell/neighborsUnemployedRate.csv'
slots_cnt = 10


def read_unemployment_rate_sorted_diffs():
    with open(file, 'r') as f:
        unemployment_rate_diffs = []
        reader = csv.reader(f, delimiter=',')
        for row in reader:
            if row:
                value = abs(float((row[3])))
                unemployment_rate_diffs.append(value)
    return unemployment_rate_diffs.sort()


def slot_max(min_value, slot_no, delta):
    return min_value + ((slot_no - 1) * delta) + delta


def create_slot_to_max_map(diffs):
    delta = (max(diffs) - min(diffs)) / slots_cnt
    map = {}
    for slot_no in range(1, slots_cnt + 1):
        map[slot_no] = slot_max(min(diffs), slot_no, delta)


def create_slot_to_cnt_map():
    map = {}
    for slot_no in range(1, slots_cnt + 1):
        map[slot_no] = 0
    return map


def compute_histogram(diffs, slot_to_max, slot_to_cnt):
    slot_no = 1
    for v in diffs[:-1]:
        slot_no = calculate_slot_for_value(v, slot_no, slot_to_max)
        slot_to_cnt[slot_no] += 1
    slot_to_cnt[slots_cnt] += 1


def calculate_slot_for_value(value, current_slot_no, slot_to_max):
    while not value < slot_max[current_slot_no]:
        current_slot_no += 1
    return current_slot_no


def assert_histogram_correct(diffs, slot_to_cnt):
    sum = 0
    for k, v in slot_to_cnt:
        sum += v
    assert len(diffs) == sum


def write_histogram_to_file(slot_to_cnt):
    with(outfile, 'w') as f:
        writer = csv.writer(f, delimiter = ' ')
        


def run():
    diffs = read_unemployment_rate_sorted_diffs()
    slot_to_max = create_slot_to_max_map(diffs)
    slot_to_cnt = create_slot_to_cnt_map(slot_to_max)
    compute_histogram(diffs, slot_to_max, slot_to_cnt)
    assert_histogram_correct(diffs, slot_to_cnt)
    write_histogram_to_file(slot_to_cnt)
