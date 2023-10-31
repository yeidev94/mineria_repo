# -*- coding: utf-8 -*-
"""exploracion_de_datos_01.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1jqV6NHyu_76mctS5IdcKXpXVrKZcmMF0
"""

# Commented out IPython magic to ensure Python compatibility.
import pandas as pd
import numpy as np
import seaborn as sns
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
# %matplotlib inline
from matplotlib import style
from matplotlib import cm
from matplotlib import colors

url ='https://raw.githubusercontent.com/utn-frocha/datos/main/encuesta_aeropuerto.csv'
df = pd.read_csv(url)

df.dtypes

print(df.head())
print("<<<<------------------------------------------------------->>>>")
print(df.info())
print("<<<<------------------------------------------------------->>>>")
print(df.isnull().sum())
print("<<<<------------------------------------------------------->>>>")
print(df.describe())

print(df.isnull().sum(axis = 0))

grupo=df.iloc[1:,1:6]
print(grupo.head(10))

#df.groupby(by = "Type of Travel").count()

agrupacion=pd.DataFrame(df)
agrupacion.groupby(by = "Type of Travel").mean().round()

#print(df['Class'].unique())
df['Customer Type'].unique()

promedio_edad =round( df['Age'].mean())
print(f' la edad promedio de los pacientes es de : {promedio_edad}')

df.replace({
    'Gender': {
        'Male': 0,
        'Female': 1
    }
}, inplace=True)

df = pd.get_dummies(
    df,
    columns=['Type of Travel', 'Customer Type'],
    drop_first=True
)

display(df)

sns.boxplot(x = 'Gender', y = 'Age', data = df)

sns.boxplot(data=df[["Age"]], orient="h")

sns.boxplot(x = 'Class', y = 'Age', data = df)

boxplot = df.boxplot(column=['Age'],grid=False, rot=45, fontsize=10 ,figsize=(10,5))

sns.boxplot(data=df[["Age", "Class"]], orient="h")

fig, axes = plt.subplots(1, 3, figsize=(25, 5), sharey=True)
fig.suptitle('Visualizing outlyers data column edad')
sns.boxplot(x = 'Class', y = 'Age', data = df,ax=axes[0])
sns.boxplot(x = 'Type of Travel', y = 'Age', data = df,ax=axes[1])
sns.boxplot(x = 'Customer Type', y = 'Age', data = df,ax=axes[2])

fig, axes = plt.subplots(1, 4, figsize=(15, 5), sharey=True)
fig.suptitle('Visualizing categorical data columns')
sns.barplot(x=df['Gender'], y=df['Flight Distance'], ax=axes[0])
sns.barplot(x=df['Type of Travel'], y=df['Flight Distance'], ax=axes[1])
sns.barplot(x=df['Class'], y=df['Flight Distance'], ax=axes[2])
sns.barplot(x=df['Customer Type'], y=df['Flight Distance'], ax=axes[3])

fig = plt.figure(figsize=(25, 10))
datos = df.groupby('Class')['Class'].count()
datos=pd.DataFrame(datos)
datos = datos.rename(columns={'Class':'Total'})
dfi = datos.reset_index()

dfi.reset_index(drop=True, inplace=True)
dfi
plt.pie(dfi['Total'], labels=dfi['Class'], autopct="%0.1f %%")
plt.show()

# Visualizamos histograma  de las caraterísticas de entrada
plt.rcParams["figure.figsize"] = (25, 25)
df.drop(['satisfaction', 'Class', 'Type of Travel','Customer Type','Gender','id'],1).hist()
plt.show( )

# Usando matplotlib
plt.figure(figsize=(25, 15))
sns.heatmap(df.corr(), annot=True)
plt.title("Correlation between the columns")
plt.show()

plt.figure(figsize=(25, 15))
df_cor=df.drop(['id'], axis=1)
sns.heatmap(df_cor.corr(), annot=True, cmap="YlGnBu")
plt.title("Correlation between the columns")
plt.show()

fig = plt.figure(figsize=(25,10))
x = df['Inflight wifi service']
y = df['Food and drink']
z = df['Arrival Delay in Minutes']
ax = Axes3D(fig)
ax.scatter(x, y, z, c=z, marker='o')
ax.set_xlabel('Inflight wifi service')
ax.set_ylabel('Food and drink')
ax.set_zlabel('Arrival Delay in Minutes')
plt.title('Grafica tridimencional usando SEABORN')
ax.scatter(x, y, z, c="red")
plt.show()