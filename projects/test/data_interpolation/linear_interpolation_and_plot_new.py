#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np
import os

os.chdir(
    os.path.expanduser('~/projects/myopia/projects/test/data_interpolation'))

data = pd.read_csv("filled_with_basic_methods.csv")  # Basic interpolation with methods and nearst
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

distance = pd.read_csv('province_distance_matrix.csv')
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
def interpolate_linear(df):
    '''
    线性插值函数
    '''
    grouped_df = df_sorted.groupby(['province', 'age_group'])
    df_ = pd.DataFrame()
    for group_name, group_data in grouped_df:
        group_data['myopia_rate_urban'] = group_data[
            'myopia_rate_urban'].interpolate(method='linear',
                                             limit_direction='both',
                                             limit=10,
                                             axis=0)
        group_data['myopia_rate_rural'] = group_data[
            'myopia_rate_rural'].interpolate(method='linear',
                                             limit_direction='both',
                                             limit=10,
                                             axis=0)
        df_ = pd.concat([df_, group_data])
    return (df_)


# Province weights based on population
w = pd.read_csv("weight.csv")
w = w.set_index('province')
w


def weight_matching(df):
    '''权重匹配'''
    df['weight'] = 0
    for index, row in df.iterrows():
        pvc = row['province']
        year = row['year']
        df.loc[index, 'weight'] = w.loc[pvc, str(year)]
    df['weighted_urban'] = df.apply(
        lambda x: x['myopia_rate_urban'] * x['weight'], axis=1)
    df['weighted_rural'] = df.apply(
        lambda x: x['myopia_rate_rural'] * x['weight'], axis=1)

    return df


# 创建空dataframe储存数据
def rate_dataframe():
    colname = np.arange(1998, 2021)
    df_rate_urban = pd.DataFrame(
        index=['Grade 1-3', 'Grade 4-6', 'Grade 7-9', 'Grade 10-12'],
        columns=colname)
    df_rate_rural = pd.DataFrame(
        index=['Grade 1-3', 'Grade 4-6', 'Grade 7-9', 'Grade 10-12'],
        columns=colname)
    return df_rate_urban, df_rate_rural


def rate_calculate(df):
    '''计算rate'''
    df_rate_urban, df_rate_rural = rate_dataframe()
    df['weight_rural'] = np.nan
    df['weight_urban'] = np.nan
    for index, row in df.iterrows():
        if not pd.isna(row['myopia_rate_urban']):
            df.loc[index, 'weight_urban'] = row['weight']
        if not pd.isna(row['myopia_rate_rural']):
            df.loc[index, 'weight_rural'] = row['weight']

    grouped_df = df.groupby(['year', 'age_group'])
    for group_name, group_data in grouped_df:
        rate_urban = group_data['weighted_urban'].sum()
        total_weight_urban = group_data['weight_urban'].sum()
        rate_urban_ = rate_urban / total_weight_urban
        rate_rural = group_data['weighted_rural'].sum()
        total_weight_rural = group_data['weight_rural'].sum()
        rate_rural_ = rate_rural / total_weight_rural

        #         print(total_weight_rural)
        year, age_group = group_name
        df_rate_urban.iloc[age_group - 1, year - 1998] = rate_urban_
        df_rate_rural.iloc[age_group - 1, year - 1998] = rate_rural_

    return df_rate_urban, df_rate_rural, df


# In[9]:

df = interpolate_linear(df_sorted)
df = weight_matching(df)
df_rate_urban, df_rate_rural, data = rate_calculate(df)
data.to_csv('filled_1.csv', index=False)

df1 = df[['province', 'myopia_rate_urban', 'myopia_rate_rural']]
out = df1.groupby('province').mean()
out.to_csv('filled_by_province.csv', index=True)


df_rate_urban.T.drop(2020)  # let's only use 1998-2019 before pandemic data
df_rate_rural.T.drop(2020)

df_rate_urban.T.index.name = "year"  # add index name
df_rate_rural.T.index.name = "year"

df_rate_urban.T.drop(2020).to_csv('myopia_rate_urban.csv', index=True)
df_rate_rural.T.drop(2020).to_csv('myopia_rate_rural.csv', index=True)


import matplotlib
matplotlib.use('QtAgg')
import matplotlib.pyplot as plt

fig, axes = plt.subplots(2, 1, figsize=(8, 10))
df_transposed_urban = df_rate_urban.T.drop(2020)
axes[0].plot(df_transposed_urban, marker='o')
axes[0].set_title('urban')
df_transposed_rural = df_rate_rural.T.drop(2020)
axes[1].plot(df_transposed_rural, marker='o')
axes[1].set_title('rural')

plt.show()


# Groupped by year plot
indexname = ['Grade 1-3', 'Grade 4-6', 'Grade 7-9', 'Grade 10-12']
colname = ['1998-2003', '2004-2007', '2008-2011', '2012-2015', '2016-2019']
df_urban = pd.DataFrame(index=indexname, columns=colname)
df_rural = pd.DataFrame(index=indexname, columns=colname)

# In[19]:

df_urban['1998-2003'] = df_rate_urban[[1998, 1999, 2000, 2001, 2002,
                                       2003]].mean(axis=1)
df_urban['2004-2007'] = df_rate_urban[[2004, 2005, 2006, 2007]].mean(axis=1)
df_urban['2008-2011'] = df_rate_urban[[2008, 2009, 2010, 2011]].mean(axis=1)
df_urban['2012-2015'] = df_rate_urban[[2012, 2013, 2014, 2015]].mean(axis=1)
df_urban['2016-2019'] = df_rate_urban[[2016, 2017, 2018, 2019]].mean(axis=1)
print(df_urban)
df_urban.to_csv('table_urban.csv', index=False)
df_rural['1998-2003'] = df_rate_rural[[1998, 1999, 2000, 2001, 2002,
                                       2003]].mean(axis=1)
df_rural['2004-2007'] = df_rate_rural[[2004, 2005, 2006, 2007]].mean(axis=1)
df_rural['2008-2011'] = df_rate_rural[[2008, 2009, 2010, 2011]].mean(axis=1)
df_rural['2012-2015'] = df_rate_rural[[2012, 2013, 2014, 2015]].mean(axis=1)
df_rural['2016-2019'] = df_rate_rural[[2016, 2017, 2018, 2019]].mean(axis=1)
print(df_rural)
df_rural.to_csv('table_rural.csv', index=False)


fig, axes = plt.subplots(2, 1, figsize=(8, 10))
df_transposed_urban = df_urban.T
axes[0].plot(df_transposed_urban, marker='o')
axes[0].set_title('urban')
df_transposed_rural = df_rural.T
axes[1].plot(df_transposed_rural, marker='o')
axes[1].set_title('rural')
plt.show()


def partition(mydata):
    x = mydata.index.values
    y1 = mydata['Grade 1-3'].values
    y2 = mydata['Grade 4-6'].values
    y3 = mydata['Grade 7-9'].values
    y4 = mydata['Grade 10-12'].values
    x_new = np.linspace(1998, 2020, 100)
    return y1, y2, y3, y4, x, x_new


def spline_and_plot(x, y, i):
    spl = UnivariateSpline(x, y)
    spl.set_smoothing_factor(0.02)
    y_smooth = spl(x)
    #计算点估计和标准误差
    y_pred = spl(x)  # 点估计
    residuals = y - y_pred
    mse = np.mean(residuals**2)  # 均方误差
    std_error = np.sqrt(mse)  # 标准误差

    # 计算置信区间
    alpha = 0.05  # 置信水平
    z_score = 1.96  # 95% 置信水平对应的标准正态分布临界值
    lower_bound = y_pred - z_score * std_error
    upper_bound = y_pred + z_score * std_error
    plt.scatter(x, y, label='Grade {}-{}'.format(3 * i - 2, 3 * i))
    plt.plot(x, y_smooth, color='black')
    plt.fill_between(x, lower_bound, upper_bound, color='gray', alpha=0.3)
    plt.legend(loc='lower right')


def isolation_forest(mydata):
    scaler = StandardScaler()
    np_scaled = scaler.fit_transform(mydata.values.reshape(-1, 1))
    data = pd.DataFrame(np_scaled)
    # train isolation forest
    outliers_fraction = float(0.01)
    model = IsolationForest(contamination=outliers_fraction)
    model.fit(data)
    result = model.predict(data)
    a = mydata.loc[result == -1]
    plt.scatter(a.index, a, color='yellow')


mydata_urban = df_rate_urban.T.drop(2020).copy()
y1, y2, y3, y4, x, x_new = partition(mydata_urban)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, i)
    isolation_forest(mydata_urban.iloc[:, i - 1])

plt.title('urban')
plt.show()

mydata_rural = df_rate_rural.T.drop(2020).copy()
y1, y2, y3, y4, x, x_new = partition(mydata_rural)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, i)
    isolation_forest(mydata_rural.iloc[:, i - 1])

plt.title('rural')
plt.show()

# Spline plot
def interpolate_spline2(df):
    '''
    二次样条插值
    '''
    grouped_df = df_sorted.groupby(['rank', 'age_group'])
    df_ = pd.DataFrame()
    for group_name, group_data in grouped_df:
        group_data['myopia_rate_urban'] = group_data[
            'myopia_rate_urban'].interpolate(method='spline',
                                             order=2,
                                             limit_direction='both')
        group_data['myopia_rate_rural'] = group_data[
            'myopia_rate_rural'].interpolate(method='spline',
                                             order=2,
                                             limit_direction='both')
        df_ = pd.concat([df_, group_data])
    return (df_)


# In[22]:

df = interpolate_spline2(df_sorted)
#df.to_csv('filled_1.csv', index=False)
df = df.drop('rank', axis=1)
df = weight_matching(df)
df_rate_urban, df_rate_rural, data = rate_calculate(df)
data.to_csv('filled_2.csv', index=False)

# In[23]:

fig, axes = plt.subplots(2, 1, figsize=(8, 10))
df_transposed_urban = df_rate_urban.T
axes[0].plot(df_transposed_urban, marker='o')
axes[0].set_title('urban')
df_transposed_rural = df_rate_rural.T
axes[1].plot(df_transposed_rural, marker='o')
axes[1].set_title('rural')

# In[24]:


def spline_and_plot(x, y, x_new, i):
    spl = UnivariateSpline(x, y)
    spl.set_smoothing_factor(0.1)
    y_smooth = spl(x_new)
    plt.scatter(x, y, label='Grade {}-{}'.format(3 * i - 2, 3 * i))
    plt.plot(x_new, y_smooth, color='black')
    plt.legend()


mydata_urban = df_rate_urban.T.copy()
y1, y2, y3, y4, x, x_new = partition(mydata_urban)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, x_new, i)
    isolation_forest(mydata_urban.iloc[:, i - 1])

plt.title('urban')
plt.show()

mydata_rural = df_rate_rural.T.copy()
y1, y2, y3, y4, x, x_new = partition(mydata_rural)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, x_new, i)
    isolation_forest(mydata_rural.iloc[:, i - 1])

plt.title('rural')
plt.show()

# In[25]:


def partition(mydata):
    x = mydata.index.values
    y1 = mydata['Grade 1-3'].values
    y2 = mydata['Grade 4-6'].values
    y3 = mydata['Grade 7-9'].values
    y4 = mydata['Grade 10-12'].values

    return y1, y2, y3, y4, x


def spline_and_plot(x, y, i):
    spl = UnivariateSpline(x, y)
    spl.set_smoothing_factor(0.02)
    y_smooth = spl(x)
    #计算点估计和标准误差
    y_pred = spl(x)  # 点估计
    residuals = y - y_pred
    mse = np.mean(residuals**2)  # 均方误差
    std_error = np.sqrt(mse)  # 标准误差

    # 计算置信区间
    alpha = 0.05  # 置信水平
    z_score = 1.96  # 95% 置信水平对应的标准正态分布临界值
    lower_bound = y_pred - z_score * std_error
    upper_bound = y_pred + z_score * std_error
    plt.scatter(x, y, label='Grade {}-{}'.format(3 * i - 2, 3 * i))
    plt.plot(x, y_smooth, color='black')
    plt.fill_between(x, lower_bound, upper_bound, color='gray', alpha=0.3)
    plt.legend(loc='lower right')


mydata_urban = df_rate_urban.T.drop(2020).copy()
y1, y2, y3, y4, x = partition(mydata_urban)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, i)
    isolation_forest(mydata_urban.iloc[:, i - 1])

plt.title('urban')
plt.show()

mydata_rural = df_rate_rural.T.drop(2020).copy()
y1, y2, y3, y4, x = partition(mydata_rural)
for i in range(1, 5):
    variable_name = "y" + str(i)
    y = globals()[variable_name]
    spline_and_plot(x, y, i)
    isolation_forest(mydata_rural.iloc[:, i - 1])

plt.title('rural')
plt.show()
