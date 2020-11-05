genetica<-function(x,y){
  a1<- paste0(x[1],y[1])
  b1<- paste0(x[1],y[2])
  c1<- paste0(x[2],y[1])
  d1<- paste0(x[2],y[2])
  cruce<-c(a1,b1,c1,d1)
  if(x[1]>x[2]) dominante<-paste0(x[1],x[1])
  if(x[1]>y[1]) dominante<-paste0(x[1],x[1])
  if(y[1]>y[2]) dominante<-paste0(y[1],y[1])
  if(y[1]>x[1]) dominante<-paste0(y[1],y[1])
  if(x[1]>x[2]) heterocigoto<-paste0(x[1],x[2])
  if(x[1]>y[1]) heterocigoto<-paste0(x[1],y[1])
  if(y[1]>y[2]) heterocigoto<-paste0(y[1],y[2])
  if(y[1]>x[1]) heterocigoto<-paste0(y[1],x[1])
  if(y[1]>y[2]) heterocigotoinverso<-paste0(y[2],y[1]) 
  if(x[1]>x[2]) heterocigotoinverso<-paste0(x[2],x[1]) 
  if(y[1]>x[1]) heterocigotoinverso<-paste0(x[1],y[1])
  if(x[1]>y[1]) heterocigotoinverso<-paste0(y[1],x[1])
  if(x[1]>y[1]) recesivo<-paste0(y[1],y[1])
  if(x[1]>x[2]) recesivo<-paste0(x[2],x[2])
  if(y[1]>y[2]) recesivo<-paste0(y[2],y[2])
  if(y[1]>x[1]) recesivo<-paste0(x[1],x[1])
  contador<-0
  contadorHomocigotodominante<-0
  contadorheterocigoto<-0
  contadorhomocigotorecesivo<-0
  if(x[1]==x[2] & x[1]==y[1] & x[2]==y[2]) datos<-matrix(data=c(a1,b1,c1,d1),nrow = 2)
  else {repeat{
    contador<-contador+1
    if(crucer[contador]==dominante) contadorHomocigotodominante<-contadorHomocigotodominante+1
    if(cruce[contador]==heterocigoto) contadorheterocigoto<-contadorheterocigoto+1
    if(cruce[contador]==heterocigotoinverso) contadorheterocigoto<-contadorheterocigoto+1
    if(cruce[contador]==recesivo) contadorhomocigotorecesivo<-contadorhomocigotorecesivo+1
    
    
    datos<-list(
      punnet=matrix(data=c(a1,b1,c1,d1),nrow = 2),
      probabilidad_de_genotipos=data.frame(genotipo=c(paste0("Homocigoto_Dominante ",dominante),
                                                      paste0("Heterocigoto ",heterocigoto),
                                                      paste0("Homocigoto_Recesivo ",recesivo)),
                                           probabilidad_en_porcentaje =c(contadorHomocigotodominante/4*100,
                                                                         contadorheterocigoto/4*100,
                                                                         contadorhomocigotorecesivo/4*100)),
      probabilidad_de_fenotipos=data.frame(fenotipo=c("dominante","recesivo"),
                                           Probabilidad_en_porcentaje =c((
                                             contadorHomocigotodominante+contadorheterocigoto)/4*100,contadorhomocigotorecesivo/4*100)))
    if(contador==4)break
  }
  }
  datos
}
