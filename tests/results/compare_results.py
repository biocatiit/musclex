import pandas as pd
import numpy as np
from sklearn.metrics import r2_score, mean_squared_error
import os
from os.path import join
import matplotlib.pyplot as plt

manual_path = '/Users/jiranun/Work/RA/musclex/tests/results/Merge_results_v1_40.csv'
auto_path = '/Users/jiranun/Work/RA/musclex/tests/results/summary_gauss.csv'

manual_data = pd.read_csv(manual_path)
auto_data = pd.read_csv(auto_path)

man = manual_data[['Name','Manual area ratio']]
man.columns = ['Name','I11/I10']
ratio = np.array(auto_data['Avarage I11/I10 per fiber'])
aut = auto_data[pd.notnull(auto_data['Avarage I11/I10 per fiber'])]
aut = aut[['Filename', 'Avarage I11/I10 per fiber']]
aut.columns = ['Name','I11/I10']

# print(man.head())
# print(aut.head())

all_imgs = list(man['Name'])

res = pd.DataFrame(columns=['Name', 'Manual I11/I10', 'Equator I11/I10', 'Absolute Error', 'Manual/Equator'])

# print(all_imgs)

for im in all_imgs:
    man_row = man[man['Name'] == im]
    aut_row = aut[aut['Name'] == im]
    if len(aut_row) > 0:
        manual_ratio = float(man_row['I11/I10'])
        if str(list(aut_row['I11/I10'])[0]) == '-':
            continue
        auto_ratio = float(aut_row['I11/I10'])
        ae = abs(manual_ratio-auto_ratio)
        new_point = {
            'Name' : im,
            'Manual I11/I10': manual_ratio,
            'Equator I11/I10' : auto_ratio,
            'Absolute Error' : ae,
            'Manual/Equator':manual_ratio/auto_ratio
        }

        res = res.append(new_point, ignore_index=True)

all_manual_ratio = np.array(res['Manual I11/I10'])
all_auto_ratio = np.array(res['Equator I11/I10'])
all_cross = np.array(res['Manual/Equator'])

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(all_manual_ratio, label='Manual')
ax.plot(all_auto_ratio, label='Equator 1.8')
ax.legend()
fig.savefig('compare.png')

print(all_cross)
print("mean = "+str(np.mean(all_cross)))
print("var = "+str(np.var(all_cross)))

res.to_csv(join(os.getcwd(), 'merged_gauss.csv'))