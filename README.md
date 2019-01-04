# ml-2018-2-final-project
proyecto final machine learning

En este proyecto intentaré hacer una regresión a un dataset de 481 atributos desconocidos y 93 instancias, de las cuales 42 serán usadas para entrenamiento.
La columna a predecir es es Y y las demás están enumeradas de X1 a X480.

Lo primero que noté al revisar el dataset fue que cada 10 columnas, los datos eran muy similares, lo que me hizo pensar que probablemente son mediciones del mismo atributo hechas 10 veces en distintos periodos de tiempo, así que comencé a sacar un promedio de 10 en 10 mediciones, lo que resultó en un dataset de 48 columnas y las mismas 42 instancias (dataset: ***A.promedios.txt***). Luego en R normalicé el dataset, obtuve los boxplot para cada atributo para eliminar los outliers (<= 5% de los datos) de forma manual. Finalmente volví a normalizar. 

Todo esto se encuentra en el archivo ***test.R***.
