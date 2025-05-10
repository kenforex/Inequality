import re
from itertools import groupby
from collections import Counter
def score_license_plate(plate):
    # Extracting Plate Numbers
    digits = re.findall(r'\d', plate)

    # Define Marking Rules
    rules = [
        # No 4，6，8，9: 20 marks
        (
            lambda ds: all(d not in ('4', '6', '8', '9') for d in ds),
            20
        ),
        # Triple 3: 1 mark
        (
            lambda ds: ds.count('4') == 3,
            1
        ),
        # Double 4: 5 marks
        (
            lambda ds: ds.count('4') == 2,
            5
        ),
        # Only one 4: 10 marks
        (
            lambda ds: ds.count('4') == 1,
            10
        ),
        # Rule 1
        (lambda ds: '4' not in ds and any(len(list(g)) >= 4 for k, g in groupby(ds) if k in ('6', '8', '9')), 100),
        # Rule 2
        (lambda ds: '4' not in ds and any(len(list(g)) == 3 for k, g in groupby(ds) if k in ('6', '8', '9')), 100),
        # Rule 3
        (lambda ds: '4' not in ds and sum(1 for d in ds if d in ('6', '8', '9')) == 4 and any(len(list(g)) >= 2 for k, g in groupby(ds) if k in ('6', '8', '9')), 80),
        # Rule 4
        (lambda ds: '4' not in ds and sum(1 for d in ds if d in ('6', '8', '9')) == 5 and len(set(d for d in ds if d in ('6', '8', '9'))) >= 2, 80),
        # Rule 5
        (lambda ds: '4' not in ds and any(len(list(g)) == 2 for k, g in groupby(ds) if k in ('6', '8', '9')), 60),
        # Rule 6
        (lambda ds: any(ds.count(d) == 3 for d in ('6', '8', '9')) and '4' not in ds, 60),
        # Rule 7
        (lambda ds: len([d for d in ds if d in ('6', '8', '9')]) == 4 and len(
            set([d for d in ds if d in ('6', '8', '9')])) > 1 and '4' not in ds, 60),
        # Rule 8
        (lambda ds: any(v == 4 and k not in ('6', '8', '9') and k != '4' for k, v in Counter(ds).items()), 50),
        # Rule 9.1
        (lambda ds: sum(ds.count(d) for d in ('6', '8', '9')) == 3, 50),
        # Rule 9.2
        (lambda ds: any(v == 3 and k not in ('6', '8', '9') and k != '4' for k, v in Counter(ds).items()), 40),
        # Rule 10
        (lambda ds: any(ds[i] in ('6', '8', '9') and ds[i + 1] in ('6', '8', '9') for i in range(len(ds) - 1)) and '4' not in ds,
         40),
        # Rule 11
        (lambda ds: '4' not in ds and sum(1 for d in ds if d in ('6', '8', '9')) == 3 and any(
            len(list(g)) == 1 for k, g in groupby(ds) if k in ('6', '8', '9')), 40),
        # Rule 12.1
        (
            lambda ds: len([d for d in ds if d in ('6', '8', '9')]) == 2,
            30
        ),        #Rule 12.2
        (
            lambda ds: len([d for d in ds if d in ('6', '8', '9')]) == 1 and '4' not in ds,
            30
        ),
        # Rule 13
        (
            lambda ds: all(d not in ('4', '6', '8', '9') for d in ds),
            20
        ),
        # Rule 14
        (
            lambda ds: ds.count('4') == 3,
            1
        ),
        # Rule 15
        (
            lambda ds: ds.count('4') == 2,
            5
        ),
        # Rule 16
        (
            lambda ds: ds.count('4') == 1,
            10
        ),
    ]

    # Return Marks
    for rule, score in rules:
        if rule(digits):
            return score

    # If no rule applies then return 0
    return 0


import pandas as pd

# Read input excel file
df = pd.read_excel('D:/Download/stop/part1/yunlong.xlsx')

# Extracting 5th column's data with label 'Column5'

plates = df.iloc[:, 4].astype(str)
# Save marks for each plate

df.iloc[:, 9] = plates.apply(score_license_plate)
print(plates.apply(score_license_plate))
# Write Excel file output

#df['score'] = plates.apply(score_license_plate)
df.to_excel('D:/Download/stop/part1/yunlong.xlsx')

# Test
# print(score_license_plate("A6666"))  # Output：10
# print(score_license_plate("XYZ6663"))   # Output：9.5
# print(score_license_plate("6684"))    # Output：9.5
# print(score_license_plate("OPQ68986"))  # Output：9.5
# print(score_license_plate("4454"))    # Output：0
