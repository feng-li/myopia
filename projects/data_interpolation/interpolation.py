#! /usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np
import os

os.chdir(
    os.path.expanduser('~/code/myopia/projects/data_interpolation'))

data = pd.read_csv("../data_processing/filled_with_basic_methods_and_neighbors.csv")  # Basic interpolation with methods and nearest neighbors

# data = pd.read_csv("filled_with_sunlight_intensity.csv")  # Basic interpolation with methods and nearst

data = data[[
    'province', 'year', 'age_group', 'method', 'myopia_rate_urban',
    'myopia_rate_rural'
]]
df_sorted = data.sort_values(by=['province', 'age_group', 'year'])

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
# data0['英文省份'] = data0['省份.1'].map(province_mapping)

distance = pd.read_csv('../data/weight/province_distance_matrix.csv')
distance['province'] = distance['省份'].map(province_mapping)
distance.drop(columns='省份', inplace=True)
distance.set_index(['province'], inplace=True)

for i in range(len(df_sorted)):

    i_rate = df_sorted.loc[i, [
        'province', 'age_group', 'year', 'myopia_rate_urban',
        'myopia_rate_rural'
    ]]

    if any(i_rate[['myopia_rate_urban', 'myopia_rate_rural']].isna()):
        i_province = i_rate.province
        i_age_group = i_rate.age_group
        i_year = i_rate.year
        i_myopia = i_rate.myopia_rate_urban

        i_distance_sorted = distance.loc[i_province, :].sort_values()

        nearest_year_num = 3
        nearest_year = range(i_year - nearest_year_num,
                             i_year + nearest_year_num + 1)

        nearest_province_num = 4  # including self

        nearest_province = i_distance_sorted[:nearest_province_num].index.map(
            province_mapping)

        i_rate_rural = df_sorted[
            (df_sorted.province.isin(nearest_province))
            & (df_sorted.age_group == i_age_group) &
            (df_sorted.year.isin(nearest_year))]['myopia_rate_rural'].mean()

        if any(i_rate[['myopia_rate_urban']].isna()):
            i_rate_urban = df_sorted[
                (df_sorted.province.isin(nearest_province))
                & (df_sorted.age_group == i_age_group) &
                (df_sorted.year.isin(nearest_year))]['myopia_rate_urban'].mean()
            df_sorted.loc[i, ['myopia_rate_urban']] = i_rate_urban

        if any(i_rate[['myopia_rate_rural']].isna()):
            i_rate_rural = df_sorted[
                (df_sorted.province.isin(nearest_province))
                & (df_sorted.age_group == i_age_group) &
                (df_sorted.year.isin(nearest_year))]['myopia_rate_rural'].mean()
            df_sorted.loc[i, ['myopia_rate_rural']] = i_rate_rural

df_sorted.to_csv('data_interpolated_by_nearest_year_and province.csv', index=False)


# If the above does not work, we do a further linear interpolation
