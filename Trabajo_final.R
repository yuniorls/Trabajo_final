library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(plotly)

setwd("E:/R STUDIO/Manos a la data/Trabajo final")
creditos <- readxl::read_xlsx("Reporte(01-2001 - 09-2017).xlsx")

#Eliminamos las dos ultimas columnas
creditos<-select(creditos, -c("Créditos - Total País (Miles de S/)", "Créditos - Extranjero (Miles de S/)"))

#suprimimos algunos terminos comunes en los nombres
nombres<-names(creditos)
nombres<-gsub("(Miles de S/)", "", nombres)
nombres<-gsub("\\()","", nombres)
nombres<-gsub("Créditos","", nombres)
nombres<-gsub("-", "", nombres)
nombres[6]<-"Tarjetas de credito consumo"
nombres[37]<-"Elec Gas Agua"
nombres[40]<-"Trans Alm Comu"
nombres<-trimws(x = nombres)#eliminamos los espacios al inicio y al final
nombres<-gsub(" ","_", nombres) 
nombres

#asignamos los nuevos nombres al data frame
names(creditos)=nombres

creditos$Fecha=year(creditos$Fecha) # extraemos solo el año para anualizarlo

# Agregamos los datos mensuales en años
creditos<- creditos %>% group_by(Entidad, Fecha) %>% summarise(Consumo=sum(Consumo),
                                                               Hipotecarios=sum(Hipotecarios),
                                                               Corporativos=sum(Corporativos),
                                                               Grandes_Empresas=sum(Grandes_Empresas),
                                                               Medianas_Empresas=sum(Medianas_Empresas),
                                                               Pequeñas_Empresas=sum(Pequeñas_Empresas),
                                                               Microempresas=sum(Microempresas),
                                                               Comerciales=sum(Comerciales),
                                                               Vigentes=sum(Vigentes),
                                                               Refinanciados=sum(Refinanciados),
                                                               Reestructurados=sum(Reestructurados),
                                                               Vencidos=sum(Vencidos),
                                                               Cobranza_Judicial=sum(Cobranza_Judicial),
                                                               Agropecuarios=sum(Agropecuarios),
                                                               Pesca=sum(Pesca),
                                                               Minería=sum(Minería),
                                                               Manufactura=sum(Manufactura),
                                                               Elec_Gas_Agua=sum(Elec_Gas_Agua),
                                                               Construcción=sum(Construcción),
                                                               Comercio=sum(Comercio),
                                                               Trans_Alm_Comu=sum(Trans_Alm_Comu),
                                                               Amazonas=sum(Amazonas),
                                                               Ancash=sum(Ancash),
                                                               Apurímac=sum(Apurímac),
                                                               Arequipa=sum(Arequipa),
                                                               Ayacucho=sum(Ayacucho),
                                                               Cajamarca=sum(Cajamarca),
                                                               Callao=sum(Callao),
                                                               Cusco=sum(Cusco),
                                                               Huancavelica=sum(Huancavelica),
                                                               Huánuco=sum(Huánuco),
                                                               Ica=sum(Ica),
                                                               Junín=sum(Junín),
                                                               La_Libertad=sum(La_Libertad),
                                                               Lambayeque=sum(Lambayeque),
                                                               Lima=sum(Lima),
                                                               Loreto=sum(Loreto),
                                                               Madre_de_Dios=sum(Madre_de_Dios),
                                                               Moquegua=sum(Moquegua),
                                                               Pasco=sum(Pasco),
                                                               Piura=sum(Piura),
                                                               Puno=sum(Puno),
                                                               San_Martín=sum(San_Martín),
                                                               Tacna=sum(Tacna),
                                                               Tumbes=sum(Tumbes),
                                                               Ucayali=sum(Ucayali))


#imputamos con ceros a los datos faltantes
creditos[is.na(creditos)] <- 0

# reordenamos la data de ancho a largo por zona geografica
creditos<-creditos %>% pivot_longer("Amazonas":"Ucayali", names_to="Departamento", values_to="monto_zona_geografica")

# reordenamos la data de ancho a largo por tipo de credito
creditos<-creditos %>% pivot_longer("Consumo":"Comerciales", names_to="Tipo_de_credito", values_to="monto_por_tipo")

# reordenamos la data de ancho a largo por situacion
creditos<-creditos %>% pivot_longer("Vigentes":"Cobranza_Judicial", names_to="Situacion", values_to="monto_por_situacion")

# reordenamos la data de ancho a largo por sector económico
creditos<-creditos %>% pivot_longer("Agropecuarios":"Trans_Alm_Comu", names_to="Sector", values_to="monto_por_sector")

#Ejemplo 1

# En que departamento se da mayor credito en el 2016

ejem1<-creditos %>%
  group_by(Departamento) %>%
  filter(Fecha %in% 2016) %>% 
  summarise(total_dpto=mean(monto_zona_geografica))%>%
  arrange(desc(total_dpto)) %>% 
  mutate(total_dpto=total_dpto/1000000) %>% #lo dividimos por un millon
  mutate(across(is.numeric, round, 2)) # redondeamos

#grafico 1
grafico1<-ejem1 %>% 
  ggplot(aes(reorder(Departamento, total_dpto), total_dpto)) + 
  geom_col(aes(fill = total_dpto)) + 
  scale_color_brewer() + 
  coord_flip() + labs(x="Crédito", y="Departamentos", title = "Creditos por departamento 2016", subtitle = "(en miles de millones de soles)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplotly(grafico1)

#Ejemplo 2
# Que tipo de crédito se dió más en el año 2015

ejem2<-creditos %>%
  group_by(Tipo_de_credito) %>%
  filter(Fecha %in% 2015) %>% 
  summarise(total_tipo=sum(monto_por_tipo))%>%
  arrange(desc(total_tipo)) %>% 
  mutate(total_tipo=total_tipo/1000000) %>% #lo dividimos por un millon
  mutate(across(is.numeric, round, 2)) # redondeamos

#gráfico 2
grafico2 <- ejem2 %>% plot_ly(labels = ~Tipo_de_credito, values = ~total_tipo)
grafico2 <- grafico2 %>% add_pie(hole = 0.6)
grafico2 <- grafico2 %>% layout(title = "% de participación por tipo de crédito 2015",  showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 

grafico2

#Ejemplo 3
# créditos por sectores económicos 2016
ejem3<-creditos %>%
  group_by(Sector) %>%
  filter(Fecha %in% 2016) %>% 
  summarise(total_sector=sum(monto_por_sector))%>%
  arrange(desc(total_sector)) %>% 
  mutate(total_sector=total_sector/1000000) %>% #lo dividimos por un millon
  mutate(across(is.numeric, round, 2)) # redondeamos
#gráfico 3
grafico3<-ggplot(ejem3, aes(x=Sector, y=total_sector))+
  geom_bar(stat="identity")+
  labs(x="Sector económico", y="Total de creditos", title = "Creditos por sector económico", subtitle = "(en millones de soles)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggplotly(grafico3)

#Ejercicio 4
#Créditos de la Banca Multiple por tipo de cédito en el departamento de Puno.

ejem4<- creditos %>% 
  filter(Entidad %in% "Banca Múltiple", Departamento %in% "Puno") %>% 
  group_by(Tipo_de_credito, Fecha) %>% 
  summarise(total_tipo=sum(monto_por_tipo)) %>% 
  arrange(Fecha, desc(total_tipo))%>% 
  mutate(total_tipo=total_tipo/1000000)

#gráfico 4
grafico4<-ggplot(ejem4, aes(x = Fecha, y = total_tipo, group = Tipo_de_credito)) +
  geom_line(aes(linetype = Tipo_de_credito, color = Tipo_de_credito))+
  geom_point(aes(color = Tipo_de_credito))+
  theme(legend.position = "top")+
  labs(x="Periodo", y="Créditos", title = "Créditos de la banca múltiple por tipo de crédito en Puno", subtitle = "(en millones de soles)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggplotly(grafico4) 




