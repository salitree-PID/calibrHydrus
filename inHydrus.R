# Set directory path for library installation
.libPaths("C:/Program Files/R/R-4.2.2/library")

# Instalar m?ltiples librerias (Si no lo est?n)
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages<-c("ggplot2","openxlsx","forecast","zoo", "ggplot2",
            "imputeTS","fpp", "xts", "plotly", "shiny", "tidyr",
            "padr", "naniar","latticeExtra","ggpubr","DescTools","rstudioapi",
            "scales","lubridate","cowplot","ggrepel","fmtr","reshape2")
check.packages(packages)


### IMPORTAR LIBRERIAS ###
library(openxlsx)
library(tidyr)
library(zoo)
library(dplyr)
library(data.table)
library(ggplot2)
library(imputeTS)
library(latticeExtra)
library(plotly)
library(ggpubr)
library(rstudioapi)
library(DescTools)
library(scales)
library(lubridate)
library(cowplot)
library(ggrepel)
library(fmtr)
#library(hydrusR)
library(DescTools)
library(reshape2)
library(FreqProf)
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))


# Installation of HydrusInverse package
current_folder <- dirname(rstudioapi::getActiveDocumentContext()$path)
package.namepath <- "HydrusInverse"
package.path <- path.expand(file.path(current_folder, package.namepath))

install.packages(package.path, repos = NULL, type="source")
library(HydrusInverse)

########################################################################################

#################### COMPLEMENTARY FUNCTIONS TO RUN HYDRUS-1D WITH OUR DATA ###########

### READ DATA INPUTS ####
# Establecer el directorio de trabajo
current_folder<-dirname(rstudioapi::getActiveDocumentContext()$path)

Campbell.namepath = "EXPORTED/CONTINUED_VWC_HYDRUS.xlsx"
Meteo.namepath = "EXPORTED/CONTINUED_FILTERED_PRECIPITATION.xlsx"
meteo_bc_data.namepath = "EXPORTED/CONTINUED_METEO_IN.xlsx"

Campbell.path = path.expand(file.path(current_folder, Campbell.namepath))
Meteo.path = path.expand(file.path(current_folder, Meteo.namepath))
meteo_bc_data.path = path.expand(file.path(current_folder, meteo_bc_data.namepath))

# Read Campbell, Precipitation and Meteorological data

Campbell_base <- read.xlsx(Campbell.path, sheet = 1)
Campbell_base[,1] <- XLDateToPOSIXct(as.numeric(Campbell_base[,1]),tz="UTC")

Meteo_base <- read.xlsx(Meteo.path, sheet = 1)
Meteo_base[,1] <- XLDateToPOSIXct(as.numeric(Meteo_base[,1]),tz="UTC")

meteo_bc_data_base <- read.xlsx(meteo_bc_data.path, sheet = 1)
meteo_bc_data_base[,1] <- XLDateToPOSIXct(as.numeric(meteo_bc_data_base[,1]),tz="UTC")

# Select period of time of database (Meteorological, Precipitation and VWC)
select.datetime <- function(dats_input,name_project){

  # Read Campbell and Meteo
  Campbell <- read.xlsx(Campbell.path, sheet = 1)
  Campbell[,1] <- XLDateToPOSIXct(as.numeric(Campbell[,1]),tz="UTC")
  colnames(Campbell) <- c("Date","Port_3","Port_4","Port_5","Port_6")

  Meteo <- read.xlsx(Meteo.path, sheet = 1)
  Meteo[,1] <- XLDateToPOSIXct(as.numeric(Meteo[,1]),tz="UTC")

  meteo_bc_data <- read.xlsx(meteo_bc_data.path, sheet = 1)
  meteo_bc_data[,1] <- XLDateToPOSIXct(as.numeric(meteo_bc_data[,1]),tz="UTC")
  #colnames(meteo_bc_data) <- c("t","Rad","TMax","TMin","RHMean","Wind","SunHours","CropHeight","Albedo","LAI.(SCF)")

  # Create a column with names of tests
  t<- sprintf("Test_%s",seq(length(tmpTimes$Days)))
  tmpTimes$Name <- t

  name_list <- data.frame(Name = as.character(tmpTimes[,4]),
                          Days = as.numeric(tmpTimes[,3]),stringsAsFactors = FALSE)

  input_model_entrytime <- matrix(ncol=1,nrow=500)
  input_model_endtime <- matrix(ncol=1,nrow=500)

  for(i in 1:nrow(tmpTimes)){

    input_model_entrytime[i,] = tmpTimes[name_list[,1] == name[i,] , "EntryTime"]
    input_model_entrytime<- as.Date(input_model_entrytime,format = "%Y-%m-%d",origin ="1970-01-01")

    input_model_endtime[i,] = tmpTimes[name_list[,1] == name[i,], "ExitTime"]
    input_model_endtime<- as.Date(input_model_endtime,format = "%Y-%m-%d",origin ="1970-01-01")

    s = as.POSIXct(input_model_entrytime)
    e = as.POSIXct(input_model_endtime)

  }

  # Converting inputs in list in order to "for" loop works
  Data <- numeric()
  Campbell_prueba <-list(Campbell)
  Meteo_prueba <- list(Meteo)
  Meteo_in_prueba <- list(meteo_bc_data)


  if(isTRUE(dats_input)) {

    for(i in 1:nrow(tmpTimes)){

      Campbell_prueba[[i]] <- Campbell %>%
        dplyr::select(Date,Port_6,Port_5,Port_4,Port_3) %>%
        filter(Date <= e[i,] & Date >= s[i,])

      Meteo_prueba[[i]] <- Meteo %>%
        dplyr::select(Date,Rain) %>% filter(Date <= e[i,] & Date >= s[i,])

      Meteo_in_prueba[[i]] <- meteo_bc_data %>%
        dplyr::select(t,Rad,TMax,TMin,RHMean,Wind,SunHours,CropHeight,Albedo,`LAI.(SCF)`) %>%
        filter(t <= e[i,] & t >= s[i,])
    }

  } else {

    Campbell <- Campbell
    Meteo <- Meteo

  }

  return(c(Campbell_prueba,Meteo_prueba,Meteo_in_prueba))


  Data <- data.frame(Data)

}


## Add Interpolation VWC for initial conditions ##
##################

interpolation.vwc<-function(n.nodes,interp.nodes,dz){

  #interpolation <- mutate(nodes,vwc.code = case_when(
  #nodes == interp_nodes[1] ~ Decagon[,2],
  #nodes == interp_nodes[2] ~ Decagon[,3],
  #nodes == interp_nodes[3] ~ Decagon[,4],
  #nodes == interp_nodes[4] ~ Decagon[,5],
  #nodes == interp_nodes[5] ~ Decagon[,6],
  #nodes == interp_nodes[6] ~ Decagon[,7],
  #nodes == interp_nodes[7] ~ Decagon[,8]))

  # Preparacion de los datos

  Campbell_int <- data.frame(Date = Campbell_base[,1],Campbell_base[5:2])

  Decagon <- data.frame(Date = Campbell_int[,1],
                        Campbell_int[,2:5])

  # Adjusting Port_0

  Decagon <- mutate(Decagon,Port_0 = case_when(
    Decagon$Port_6 > Decagon$Port_5 ~ Decagon$Port_6 + 0.002,
    Decagon$Port_6 < Decagon$Port_5 ~ Decagon$Port_6 - 0.002
  ))

  Decagon <- data.frame(Date = Decagon[,1], Port_0 = Decagon[,6], Decagon[2:5])

  nodes <-data.frame(seq(1,n.nodes, by = dz))

  # Hago lo mismo que arriba con case_when
  interpolation <- matrix(ncol = 1463, nrow = 101)
  interpolation[,1] <- nodes[,1]
  trans_decagon <- t(data.frame(Decagon[,2:6]))

  # Dataframe con todos las columnas de datos VWC
  interpolation[interp_nodes,2:1463] <- trans_decagon[,1:1462]
  interpolation <- data.frame(interpolation)

  # Nombrar las columnas
  names <- paste0("VWC_", seq_len(ncol(interpolation)-1))
  colnames(interpolation) <- c("nodes",names)

  # Interpolaci?n lineal
  interpolation <- FreqProf::approxm(interpolation,n = 101, method = "linear")
  interpolation <- t(interpolation[,2:1463])
  interpolation <- data.frame(Campbell_int[,1],interpolation)
  names <- paste0("node_", seq_len(ncol(interpolation)-1))
  colnames(interpolation) <- c("Date",names)

  # Filter interpolated data by Datetime samples
  input_model_entrytime <- matrix(ncol=1,nrow=500)

  for(i in 1:nrow(tmpTimes)){

    input_model_entrytime[i,] = tmpTimes[name_list[,1] == name[i,] , "EntryTime"]
    input_model_entrytime<- as.Date(input_model_entrytime,format = "%Y-%m-%d",origin ="1970-01-01")

    s = as.POSIXct(input_model_entrytime)

  }

  # Converting inputs in list in order to "for" loop works

  interpolation_data<-list()

  for(i in 1:nrow(tmpTimes)){

    interpolation_data[[i]] <- interpolation %>%
      dplyr::select(Date,node_1:node_101) %>%
      filter(Date == s[i,]) %>%
      dplyr::select(node_1:node_101)

  }

  return(interpolation_data)

}


#################### BEGIN SIMULATION IN HYDRUS 1D ############################

### CREATE A NUMBER OF RANDOM SAMPLES OF DATES ###
Sys.setenv(tz="UTC")

## Set period of dates
start <- as.Date("2015-11-01")
end <- as.Date("2019-05-01")

set.seed(1)
datewant <- sample (seq(as.Date(start),as.Date(end), by = "day"),500)
ExitTime = datewant %m+% months(6)

tmpTimes <- data.frame(EntryTime = datewant,
                       ExitTime = ExitTime)

tmpTimes$Days <- abs(as.numeric((as.Date(as.character(tmpTimes$EntryTime), format = "%Y-%m-%d")-
                                    as.Date(as.character(tmpTimes$ExitTime),format = "%Y-%m-%d")))) + 1

# Create a column with names of tests
t<- sprintf("Test_%s",seq(length(tmpTimes$Days)))
tmpTimes$Name <- t

name_list <- data.frame(Name = as.character(tmpTimes[,4]),
                        Days = as.numeric(tmpTimes[,3]),stringsAsFactors = FALSE)


##### EJEMPLO DE COMO HACER UN BUCLE ####
#seq(as.Date(start),as.Date(end), by = "day")[sample(100)]
#datewant <- seq(as.Date(start),as.Date(end), by = "day")[sample(100)]
#ExitTime <- data.frame(datewant + sample(1460,100))
#tmpTimes <- data.frame(EntryTime = datewant, ExitTime = ExitTime)

#endT <- matrix(ncol=1,nrow=100)

#for(i in 1:nrow(tmpTimes)){
  #endT[i,] <- sample(seq(start,ExitTime[i,],by = "day"),1)
  #endT<- as.Date(endT,format = "%Y-%m-%d",origin ="1970-01-01")}

############################################################
############# MODEL PARAMETIZATION #########################
############################################################

### BASIC INPUTS  MODEL 1 ###


############ FUERA DEL LOOP #############

# Time inputs
TimeUnit = "days" ## time units
SpaceUnit = "mm" ## space units

# Establecer directorio del proyecto y nombres de cada carpeta
files <- paste0("FIT", seq(1:2), sep = "")
project_name <- files

# Lista de directorios (Modificar directorio según la ubicación de la carpeta donde queramos guardar los modelos)

parent_dir = path.expand("E:/Hydrus Workfile/PRUEBA")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) alfa SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) alfa SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) alfa SET3")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) n SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) n SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) n SET3")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Ks SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Ks SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Ks SET3")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) l SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) l SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) l SET3")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) thrIm SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) thrIm SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) thrIm SET3")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) thsIm SET1")

parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Omega SET1")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Omega SET2")
parent_dir = path.expand("E:/Hydrus Workfile/Hydrus Models (1.5) Omega SET3")


# Crear ruta de directorio
project_path = path.expand(file.path(parent_dir, project_name))
parent.dir = parent_dir
project.name = project_name
project.path = project_path
# hydrus_path =  "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"


# Parameters Inputs
MaxIt = 10 # Number of iterations

# Initial Conditions
soil.para = list(thr = 0.001, ths = 0.33,
                 Alfa = 0.0055, n = 4.38, Ks = 5500, l = 0.50,
                 thrIm = 0.007, thsIm = 0.022, Omega = 0.001)

# Set minimum values in range of fit
min.para = list(thr = 0.0001, ths = 0.3,
                Alfa = 0.006, n = 3.5, Ks = 5000, l = 0.40,
                thrIm = 0.001, thsIm = 0.015, Omega = 0.0007)

# Set maximum values in range of fit
max.para = list(thr = 0.02, ths = 0.36,
                Alfa = 0.12, n = 5.50, Ks = 9000, l = 1.20,
                thrIm = 0.02, thsIm = 0.045, Omega = 0.01)

# Select parameters which we want to fit (0 = No fit   1 = Fit)
fit.para = list(thr = 0, ths = 0,
                Alfa = 0, n = 0, Ks = 0, l = 0,
                thrIm = 0, thsIm = 0, Omega = 1)

para = soil.para
min = min.para
max = max.para
fit = fit.para

##Profile/geometry inputs
deltaz = 16 # n? de saltos de profundidad en el perfil
profile_depth = 1600 # Profundidad del perfil
profile.depth = profile_depth
profile_nodes = seq(0, profile_depth, by = deltaz)
nodes = 100

# N? de nodos en Hydrus equivalentes a las profundidades
# que buscamos estudiar(30,60,120,160)
obs_nodes_all = c(20,39,76,101)
nObsNodes = length(obs_nodes_all)

## Boundary conditions inputs
PrintTimes = 1
time_step = 1
hCritS = 50
input.pet = F
deltaT = time_step
const_botbc = FALSE
bot_bc_type = "FreeD" # Free Drainage
const_botFlux = 0.0000 ##### in cm/hr
meteo = TRUE # Marcar True or False si queremos a?adir datos meteorol?gicos
beginT =  0 # Punto de inicio de tiempo

## Interpolation inputs
input_data = TRUE
dz = 1
n.nodes = 101
interp_nodes = c(1,20,39,76,101)
interp.nodes = interp_nodes

# Inverse inputs
inverse = TRUE # Marcar True or False si queremos utilizar modelos inversos o directos.
number_nodes = 4
MIT = 10 # number of iterations
iWeight = 2 # type of weight

############ DENTRO DEL LOOP #############

for(i in 1:2){

 # BASIC INFORMATION

 # Prepare input data to adjust days (data have blank values)
 name = as.matrix(tmpTimes[,4])
 dats_input = TRUE
 Data <- select.datetime(dats_input = TRUE, name_project = name)

 # Defining each list of inputs
 Campbell_prueba <- Data[1:500]
 Meteo_prueba <- Data[501:1000]
 Meteo_in_prueba <- Data[1001:1500]

 # Depende del loop
 Campbell <- data.frame(matrix(unlist(Campbell_prueba[i])))
 Meteo <- data.frame(matrix(unlist(Meteo_prueba[i])))
 meteo_bc_data <- data.frame(matrix(unlist(Meteo_in_prueba[i])))

 Campbell <- data.frame(lapply(Campbell_prueba[i], data.frame))
 Meteo <- data.frame(lapply(Meteo_prueba[i], data.frame))
 meteo_bc_data <- data.frame(lapply(Meteo_in_prueba[i], data.frame))

 # Change to long format
 my.melt <- function(x){
   x <- reshape2::melt(x, id.vars = c('Date'))
 }

 Campbell_long <- lapply(Campbell_prueba, my.melt)
}

# Time inputs
endTime = as.matrix(tmpTimes[,3]) # Numero de dias de cada muestra aleatoria de tiempo
total_timesteps = list(endTime)
ntimesteps = list(endTime)
measure_values <- list()
atm_bc_data <- list()  # Dataframe precipitation data
meteo_bc_data <- list() # Dataframe meteorologic data
#Campbell_long <- list()
NOBB <- list(endTime)  # Properties of Fit.in
tmax = list(endTime)   # N? de d?as de cada muestra aleatoria de tiempo
maxAL = list()         # N? de d?as de cada muestra aleatoria de tiempo en ATMOSPH.IN
meteorecords = list()  # N? de d?as de cada muestra aleatoria de tiempo en METEO.IN
interpolation_data = list() # Dataframe de datos iniciales de VWC interpolados


for(b in seq_along(project_path)){
 total_timesteps[b] = endTime[b]/time_step
 ntimesteps[b] = total_timesteps[b]
 #ntimesteps = length(1:total_timesteps)

 # Print Times Input
 tmin = 0
 tmax[b] = endTime[b]
 tstep = 1
 tinterval = 30
 TimeUnit = TimeUnit

 ##Process inputs

 # Prepare input data
 #Campbell <- Campbell
 #Campbell_long <- list()
 #Campbell_long <- lapply(Campbell_prueba, my.melt)

 NOBB[[b]] = endTime[b] * number_nodes # n? of objective points

 for(c in 3){
  measure_values[[b]] <- data.frame("HO(N)"= rep(1:endTime[b],4),
                              "FOS" = Campbell_long[[b]][c],
                              "ITYPE(N)"= rep(2,NOBB[b]),
                              "POS" = rep(1:4,c(endTime[b],endTime[b],endTime[b],endTime[b])),
                              "WTS" = rep(1,NOBB[b]))

  measure.values = measure_values
  }


 ## Interpolation inputs
 #vwc_data <- Data[,1:7]
 #Decagon <- data.frame(Date = vwc_data[1,1],Port_0 = 0.044,vwc_data[1,2:7])

 ## Boundary conditions inputs
 maxAL[b] = endTime[b]
 meteorecords[b] = endTime[b]


 ### Atmospheric top boundary conditions
 ### Time variable boundary conditions.
 #Meteo <- data.frame(Data[,1],Data[,8])
 for (d in 2){
  atm_bc_data[[b]] = data.frame(tAtm = seq(time_step, endTime[b], time_step),
                           Prec = Meteo_prueba[[b]][d],
                           rSoil = rep(0,ntimesteps[b]),
                           rRoot = rep(0,ntimesteps[b]),
                           hCritA = rep(10000, ntimesteps[b]),
                           rB = rep(0,ntimesteps[b]),
                           hB = rep(0,ntimesteps[b]),
                           ht = rep(0,ntimesteps[b]),
                           RootDepth = NA)

  atm.bc.data = atm_bc_data

 }


  ## Meteo.in Inputs ##
  meteo_bc_data[[b]] <- Meteo_in_prueba[[b]]
  #colnames(meteo_bc_data) <- c("t \n [T]", "Rad \n [MJ/m2/d]", "TMax \n [C]", "TMin \n [C]", "RHMean \n [%]", "Wind \n [km/d]",
                              #"SunHours \n [Hours]","CropHeight \n [L]", "Albedo \n [-]", "LAI (SCF) \n [-]")
  meteo.bc.data = meteo_bc_data
 }


#### Creates a blank hydrus project with three files

create.H1D.project(project.name = project_name, parent.dir = parent_dir,
                   TimeUnit = TimeUnit, SpaceUnit = SpaceUnit, inverse = inverse, meteo = TRUE, PrintTimes = PrintTimes,
                   dt = 0.001, dtMin = 0.00001, dtMax = 1, processes = c(WaterFlow = T, RootWaterUptake = F),
                   geometry = c(ProfileDepth = profile_depth,
                                NumberOfNodes = length(profile_nodes),
                                ObservationNodes = nObsNodes))

### create the soil profile (PROFILE.DAT) info

create.soil.profile(project.path = project_path, profile.depth = profile_depth, dz = deltaz, obs.nodes = NULL)

create.fit(project.path, out.file = "FIT.IN", model = 6, hysteresis = 0, NOBB = NOBB, MIT = 10,
           iWeight = 2, para = soil.para, min = min.para, max = max.para, fit = fit.para,
           measure.values = measure_values)

## Write input data

write.obs.nodes(project.path = project_path, obs.nodes = obs_nodes_all)

interpolation_data <- interpolation.vwc(n.nodes,interp.nodes = interp_nodes, dz=1)

write.ini.cond(project.path = project_path, pr.vec = NULL, wt.depth = initial_wtable, input_data = TRUE)

write.hydraulic.para(project.path = project_path, model = 6, para = soil.para)

write.bottom.bc(constant.bc = FALSE, bc.type = bot_bc_type,
                bc.value = const_botFlux, project.path = project_path)

write.meteo.in(project.path, meteorecords, deltaT, meteo.bc.data, meteo = meteo)

write.atmosph.in(project.path = project_path, maxAL = maxAL, deltaT = time_step,
                 atm.bc.data = atm_bc_data, hCritS = 50)

write.print.times(project.path = project_path, tmin = tmin, tmax = tmax, tstep = 1,
                  tinterval = tinterval, TimeUnit = TimeUnit)

##### Default hydrus path in Windows

run.H1D.inverse.simulation(project.path = project_path,
                           profile.depth = profile_depth,
                           beginT = 0, endT = endT, deltaT = time_step, tinterval = tinterval,
                           bot.bc.type = bot_bc_type, bot.bc.value = const_botFlux,
                           const.bot.bc = FALSE, atm.bc.data = atm_bc_data, meteo.bc.data = meteo.bc.data,
                           TimeUnit = TimeUnit, show.output = T)

fitted_parameters <- read.fitted_parameters(project.path, out.file = "I_Check.out", model = 6)
write.xlsx(fitted_values, file = file.path(parent_dir, "fitted_values_adjusted.xlsx"), append = F)

#############################################################################################

## PLOTTING PROFILE AND VWC IN A SPECIFIC PERIOD TIME ##

# Read output files

obs.node.out <- read.obs_node(project.path, out.file = "Obs_Node.out", obs.output = NULL, obs.nodes = obs_nodes_all)
nod.inf.out <- read.nod_inf(project.path, out.file = "Nod_Inf.out", output = NULL, warn = FALSE)

### FITTED PARAMETERS ###
# Seleccionar el directorio de trabajo
current_folder<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_folder)

## Plotting parameter frequencies
thr = data.frame(table(fitted_parameters[,2]))
colnames(thr) <- c("n","Frequency")

ths = data.frame(table(fitted_parameters[,3]))
colnames(ths) <- c("n","Frequency")

Alfa = data.frame(table(fitted_parameters[,4]))
colnames(Alfa) <- c("n","Frequency")

n = data.frame(table(fitted_parameters[,5]))
colnames(n) <- c("n","Frequency")

Ks = data.frame(table(fitted_parameters[,6]))
colnames(Ks) <- c("n","Frequency")

l = data.frame(table(fitted_parameters[,7]))
colnames(l) <- c("n","Frequency")

thrIm = data.frame(table(fitted_parameters[,8]))
colnames(thrIm) <- c("n","Frequency")

thsIm = data.frame(table(fitted_parameters[,9]))
colnames(thsIm) <- c("n","Frequency")

Omega = data.frame(table(fitted_parameters[,10]))
colnames(Omega) <- c("n","Frequency")

############ REPRESENTATION #################

ths$n <- as.numeric(as.vector(ths$n))

ths_plot<-ggplot(data=ths, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0.2,0.21,0.22,0.23,0.24,0.25,0.26)) +

  labs(x = "Number of samples (n)",colour = "",title = "Saturated Soil Water Content (Qs)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################################
Alfa$n <- as.numeric(as.vector(Alfa$n))

Alfa_plot<-ggplot(data=Alfa, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0.001,0.003,0.006,0.009,0.012)) +

  labs(x = "Number of samples (n)",colour = "",title = "Alpha") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################
n$n <- as.numeric(as.vector(n$n))

n_plot<-ggplot(data=n, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(2.0,2.2,2.4,2.6,2.8)) +

  labs(x = "Number of samples (n)",colour = "",title = "n") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################

Ks$n <- as.numeric(as.vector(Ks$n))

Ks_plot<-ggplot(data=Ks, aes(x=n, y=Frequency)) +

  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(2000,4000,6000,8000)) +

  labs(x = "Number of samples (n)",colour = "",title = "Saturated hydraulic conductivity (Ks)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################

l$n <- as.numeric(as.vector(l$n))

l_plot<-ggplot(data=l, aes(x=n, y=Frequency),width = 0.05) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5)) +

  labs(x = "Number of samples (n)",colour = "",title = "Tortuosity parameter (l)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################
thsIm$n <- as.numeric(as.vector(thsIm$n))

thsIm_plot<-ggplot(data=thsIm, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0.01,0.02,0.03,0.04,0.05,0.06)) +

  labs(x = "Number of samples (n)",colour = "",title = "Qs for the inmobile region (thsIm)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################
thrIm$n <- as.numeric(as.vector(thrIm$n))

thrIm_plot<-ggplot(data=thrIm, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0,0.01,0.02)) +

  labs(x = "Number of samples (n)",colour = "",title = "Qr for the inmobile region (thrIm)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))

##################
Omega$n <- as.numeric(as.vector(Omega$n))

Omega_plot<-ggplot(data=Omega, aes(x=n, y=Frequency)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #scale_x_continuous(breaks = c(0,0.002,0.004,0.006,0.008,0.01)) +

  labs(x = "Number of samples (n)",colour = "",title = "Mass Transfer Coefficient for the inmobile region (Omega)") +

  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill="white",color = "black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.box = "horizontal",
        plot.title = element_text(size=10,face="bold",hjust = 0.5),
        plot.margin = margin(1, 0.5, 1, 1, "cm"),
        axis.text.x = element_text(size=8,angle = 90,hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size=8,angle = 0,hjust = 0.5, vjust = 0.5),
        axis.text.y.left = element_text(size=8),
        axis.title.y.left = element_text(size=8),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        panel.grid.major.x = element_line(color = "#555555",linetype="dotted"))


##################
parameters<-ggarrange(ths_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      Alfa_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      n_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      Ks_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      l_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      thsIm_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      thrIm_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),
                      Omega_plot+
                        theme(axis.text.x = element_text(size=10,angle = 0, vjust = 1.0, hjust = 0.50),
                              plot.margin = margin(1, 0.5, 0, 1, "cm")),


                      ncol = 2, nrow = 4,align = "v",
                      widths = c(0.2,0.2),heights=c(0.1,0.1),hjust=-2,vjust=3,common.legend = TRUE,legend = "top")



# Exportar gr?fico a imagen

jpeg("RESULTS/parameters_comparative.jpeg", units="mm", width=270, height=140, res=600,pointsize = 4)
print(parameters)
dev.off()
