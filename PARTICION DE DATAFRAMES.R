#Particion de data frame con variables numericas. 

#Para ver si un modelo de machine learning es bueno, suele partirse el conjunto de 
#datos de un dataframe, en dos subconjuntos: 
#el primero suele usarse para contruir el modelo con tecnicas de machine learning
#La informacion que no se usar para contruir el modelo, se guarda para despues evaluar que tan bueno es el  modelo(una vez creado)

#Como partiremos los datos en varios conjuntos. 
#Tendremos una variable de partida, variable a traves de la cual partiremos.

install.packages("caret") #Paquete para hacer las particiones correctamente. 
library(caret) 

data = read.csv("C:/Users/URIEL/Documents/DATOS (CSV,XML,JSON,ETC)/Mas Datos separados por temas/tema1/BostonHousing.csv")
head(data,10)

#Cuando queremos construir un modelo utilizando alguna tecnica de machine learning, para predecir algun valor. 
#se debe de  dividir la info original en particiones, 
#para ello usaremos la variable que nos ayudara a hacer la division:

#"MEDV" es la variable que usaremos con el proposito de arriba. 

#Crearemos un particion de entrenamiento con el 80% de los datos. 
#el de validacion sera el resto 
#La funcion createDataPartition aleatoriamente selecciona indiices de fila del array suministrado como primer parametro de la misma.



training.ids = createDataPartition(data$MEDV, p = 0.8, list = F) #Tiene 407 entradas de 506. 
data.training = data[training.ids, ] #Sacamos un dataframe de 407 columnas. 
data.validation = data[-training.ids, ] 

#Suponer que queremos una particion para entrenar con el 70% de los datos
#el resto que se divida entre particion y validacion. 
training.ids2 = createDataPartition(data$MEDV, p=0.7, list = F)
data.training2 = data[training.ids2, ] #con esto tenemos el 70% de los datos, para la fase de entrenamiento. 
#Crearemos una variable temporal con el resto de los datos. 
temp = data[-training.ids2, ] #Ahora estos datos los vamos a dividir equitativamente en 50%. 
validation.ids.2 = createDataPartition(temp$MEDV, p = 0.5, list = F)
data.validation2 = temp[validation.ids.2, ]
data.testing = temp[-validation.ids.2, ]


#PARTICION DE DATAFRAMES CON VARIABLES CATEGORICAS 
#Como partirle con variables que sean categoricas. 
data2 = read.csv("C:/Users/URIEL/Documents/DATOS (CSV,XML,JSON,ETC)/Mas Datos separados por temas/tema2/boston-housing-classification.csv")
str(data2)
#Como elaborar una particion del dataframe. 
#siendo la columna de particion la variable 
training.id.3 = createDataPartition(data2$MEDV_CAT, p = 0.7, list = F) #Hace referencia a variables que son categoricas. 
data.training.3 = data2[training.id.3, ]
data.validation3 = data2[-training.id.3, ]

#Si queremos hacer es 3 divisiones, se puede hacer sin ningun problema. 

#Crearemos una funcion que nos genere todo lo hecho hasta este momento: 
rda.cb.partition2 = function(dataframe, target.index, prob){ 
  library(caret)
  training.ids = createDataPartition(dataframe[, target.index], p = prob, list = F)
  list(train = dataframe[training.ids, ], val = dataframe[-training.ids,])
  
}
rda.cb.partition3 <- function(dataframe, target.index,
                              prob.train, prob.val){
  library(caret)
  training.ids <- createDataPartition(dataframe[,target.index], p = prob.train, list = FALSE)
  train.data <- dataframe[training.ids,]
  temp <- dataframe[-training.ids,]
  validation.ids <- createDataPartition(temp[,target.index], p = prob.val, list = FALSE)
  list(train = train.data, val = temp[validation.ids,], test = temp[-validation.ids,])
}

data1 = rda.cb.partition2(data, 14, 0.8)
data2 = rda.cb.partition3(data2, 14, 0.7,0.5)







