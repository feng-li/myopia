#!/usr/bin/env python
# coding: utf-8

import os
import pandas as pd
import numpy as np

# ## Load the original file
os.chdir(
    os.path.expanduser('~/projects/myopia/projects/test/data_interpolation'))

data0 = pd.read_csv('data0.csv')
data0

# 2019 data was not marked. Let's assume they are usable with "体质健康测试"
data0['0：不可用；1：可用；2潜力，需整体近视率补充数据；3潜力，需补充整体城乡数据'] = data0[
    '0：不可用；1：可用；2潜力，需整体近视率补充数据；3潜力，需补充整体城乡数据'].fillna(1)

province_mapping = {
    '北京': 'Beijing',
    '天津': 'Tianjin',
    '河北': 'Hebei',
    '山西': 'Shanxi',
    '内蒙古': 'Inner Mongolia',
    '内蒙': 'Inner Mongolia',
    '辽宁': 'Liaoning',
    '吉林': 'Jilin',
    '黑龙江': 'Heilongjiang',
    '上海': 'Shanghai',
    '江苏': 'Jiangsu',
    '浙江': 'Zhejiang',
    '安徽': 'Anhui',
    '福建': 'Fujian',
    '江西': 'Jiangxi',
    '山东': 'Shandong',
    '河南': 'Henan',
    '湖北': 'Hubei',
    '湖南': 'Hunan',
    '广东': 'Guangdong',
    '广西': 'Guangxi',
    '海南': 'Hainan',
    '重庆': 'Chongqing',
    '四川': 'Sichuan',
    '贵州': 'Guizhou',
    '云南': 'Yunnan',
    '西藏': 'Tibet',
    '陕西': 'Shaanxi',
    '甘肃': 'Gansu',
    '青海': 'Qinghai',
    '宁夏': 'Ningxia',
    '新疆': 'Xinjiang',
    '台湾': 'Taiwan',
    '香港': 'Hong Kong',
    '澳门': 'Macao'
}
data0['英文省份'] = data0['省份.1'].map(province_mapping)

# 筛选可用数据, we use all usable data marked with 1.
df0 = data0[(data0['0：不可用；1：可用；2潜力，需整体近视率补充数据；3潜力，需补充整体城乡数据'] == 1)]
df0

# Select urban and rual data
df = df0.iloc[:, [
    80, 14, 16, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
    67, 68, 69, 70, 71, 72, 73, 74
]]

df.to_csv('filtered_data.csv', index=False)
data = pd.read_csv('filtered_data.csv')

empty = pd.read_csv('empty table.csv')
empty['method_1'] = np.nan

# Mark all "\" and "0" as nan
data = data.replace('\\', np.nan)
data = data.replace(str(0), np.nan)

data


# Prefer using method 2 ("散瞳")

data_2 = data[(data['验光方式1非散瞳；2散瞳；3试片'] == 2)]
grouped_data = data_2.groupby(['英文省份', '年份'])
for group_name, group_data in grouped_data:
    province, year = group_name
    method = 2
    value = []
    for j in range(5, 28, 3):
        group_data_0 = group_data.iloc[:, j].fillna('NaN')
        group_data_1 = group_data_0.str.rstrip('%').astype(float) / 100
        mean = group_data_1.mean()
        value.append(mean)

    for i in range(1, 5):
        empty.loc[(empty['province'] == province) & (empty['year'] == year) &
                  (empty['age_group'] == i), 'method'] = method
        empty.loc[(empty['province'] == province) & (empty['year'] == year) &
                  (empty['age_group'] == i), 'method_1'] = method
        empty.loc[(empty['province'] == province) & (empty['year'] == year) &
                  (empty['age_group'] == i),
                  'myopia_rate_urban'] = value[2 * i - 2]
        empty.loc[(empty['province'] == province) & (empty['year'] == year) &
                  (empty['age_group'] == i),
                  'myopia_rate_rural'] = value[2 * i - 1]

empty


# 填充 method 1 和 3
data_1 = data[(data['验光方式1非散瞳；2散瞳；3试片'] == 1) |
              (data['验光方式1非散瞳；2散瞳；3试片'] == 3)]
grouped_data = data_1.groupby(['英文省份', '年份'])
for group_name, group_data in grouped_data:
    province, year = group_name
    method = '1 or 3'
    value = []

    # change 100% to numbers
    for j in range(5, 28, 3):
        group_data_0 = group_data.iloc[:, j].fillna('NaN')
        group_data_1 = group_data_0.str.rstrip('%').astype(float) / 100
        mean = group_data_1.mean()
        value.append(mean)

    for i in range(1, 5):
        if empty.loc[(empty['province'] == province) & (empty['year'] == year)
                     & (empty['age_group'] == i),
                     'myopia_rate_urban'].isna().any():
            empty.loc[(empty['province'] == province) & (empty['year'] == year)
                      & (empty['age_group'] == i), 'method'] = method
            empty.loc[(empty['province'] == province) & (empty['year'] == year)
                      & (empty['age_group'] == i),
                      'myopia_rate_urban'] = value[2 * i - 2]
        if empty.loc[(empty['province'] == province) & (empty['year'] == year)
                     & (empty['age_group'] == i),
                     'myopia_rate_rural'].isna().any():
            empty.loc[(empty['province'] == province) & (empty['year'] == year)
                      & (empty['age_group'] == i),
                      'myopia_rate_rural'] = value[2 * i - 1]
            empty.loc[(empty['province'] == province) & (empty['year'] == year)
                      & (empty['age_group'] == i), 'method_1'] = method

empty

empty_sorted = empty.sort_values(by=['province', 'year', 'age_group'])
# empty_sorted = empty_sorted.loc[empty_sorted.year != 2020, :]

# Remove suspicious inputed data for certain age group and province
# empty_sorted.loc[(empty_sorted['year'] == 2006) &
#                  (empty_sorted['province'] == 'Chongqing') &
#                  (empty_sorted['age_group'] == 3),
#                  'myopia_rate_urban'] = np.nan
# empty_sorted.loc[(empty_sorted['year'] == 2020) &
#                  (empty_sorted['province'] == 'Inner Mongolia') &
#                  (empty_sorted['age_group'] == 3),
#                  'myopia_rate_rural'] = np.nan
# empty_sorted.loc[(empty_sorted['year'] == 2020) &
#                  (empty_sorted['province'] == 'Inner Mongolia') &
#                  (empty_sorted['age_group'] == 4),
#                  'myopia_rate_rural'] = np.nan
# empty_sorted.loc[(empty_sorted['year'] == 2020) &
#                  (empty_sorted['province'] == 'Guangxi') &
#                  (empty_sorted['age_group'] == 4),
#                  'myopia_rate_rural'] = np.nan
# empty_sorted.loc[(empty_sorted['year'] == 2015) &
#                  (empty_sorted['age_group'] == 2),
#                  'myopia_rate_urban'] = np.nan
# empty_sorted.loc[(empty_sorted['year'] == 2019) &
#                  (empty_sorted['age_group'] == 4),
#                  'myopia_rate_urban'] = np.nan
# empty_sorted.loc[(empty['year'] == 2019) &
#                  (empty['province'] == 'Xinjiang') &
#                  (empty['age_group'] == 3), 'myopia_rate_urban'] = np.nan
# empty_sorted.loc[(empty['year'] == 2019) &
#                  (empty['province'] == 'Xinjiang') &
#                  (empty['age_group'] == 3), 'method'] = np.nan

empty_sorted.to_csv('filled_with_basic_methods.csv', index=False)

# Using "Sunlight Intensity" as distance fur urban and rural rate
rank = pd.read_csv('光照rank.csv')
rank['province0'] = rank['province'].map(province_mapping)
merge_df = pd.merge(empty_sorted,
                    rank[['province0', 'rank']],
                    how='left',
                    left_on='province',
                    right_on='province0')

# 将合并后的结果填充到新列中
empty_sorted['rank'] = merge_df['rank']
filled = empty_sorted.copy()
filled

for i in range(0, len(empty_sorted)):
    nearest_row_index = np.nan
    if pd.isna(empty_sorted.loc[i, 'myopia_rate_urban']):
        year = empty_sorted.loc[i, 'year']
        age_group = empty_sorted.loc[i, 'age_group']
        try:
            index_lst = empty_sorted[
                (empty_sorted['year'] == year)
                & (empty_sorted['age_group'] == age_group) &
                (empty_sorted['myopia_rate_urban'].notnull())].index.to_list()
            if len(index_lst) != 0:
                nearest_index = min(range(len(index_lst)),
                                    key=lambda j: abs(index_lst[j] - i))
                nearest_row_index = index_lst[nearest_index]
                if abs(nearest_row_index - i) < 3:
                    filled.loc[i, 'myopia_rate_urban'] = empty_sorted.loc[
                        nearest_row_index, 'myopia_rate_urban']
                    filled.loc[i, 'method'] = 4
        except IndexError:
            pass

    if pd.isna(empty_sorted.loc[i, 'myopia_rate_rural']):
        year = empty_sorted.loc[i, 'year']
        age_group = empty_sorted.loc[i, 'age_group']
        try:
            index_lst = empty_sorted[
                (empty_sorted['year'] == year)
                & (empty_sorted['age_group'] == age_group) &
                (empty_sorted['myopia_rate_rural'].notnull())].index.to_list()
            if len(index_lst) != 0:
                nearest_index = min(range(len(index_lst)),
                                    key=lambda j: abs(index_lst[j] - i))
                nearest_row_index = index_lst[nearest_index]
                if abs(nearest_row_index - i) < 3:
                    filled.loc[i, 'myopia_rate_rural'] = empty_sorted.loc[
                        nearest_row_index, 'myopia_rate_rural']
                    filled.loc[i, 'method_1'] = 4
        except IndexError:
            pass

filled

filled.to_csv('filled_with_sunlight_intensity.csv', index=False)
