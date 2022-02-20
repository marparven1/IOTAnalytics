# IOTAnalytics

Shiny app sobre consumo energético de una casa con submedición eléctrica.

Proyecto de Marta Venegas Pardo en Ubiqum Code Academy para IOT Analytics.

# Estructura del proyecto

El proyecto tiene dos audiencias, una más técnica donde se expondrá con todo detalle la preparación y análisis de datos y otra de cliente, que será expuesta a través de una web desarrollada con la herramienta shiny, del paquete shiny de R.


# Plan de proyecto

- Fecha límite de entrega: 23-Feb-2022
- Tareas a realizar (fases del proyecto):

    - Lectura, descripción, preprocesado y extracción de los datos
    - Visualización 
    - Preprocesado y preparación de los datos 
    - Análisis de series temporales: 
    
        * Gráfica de la serie temporal
        * Descomposición
        * Suavizado exponencial (H-W)
        * Pronóstico del consumo energético
    - Report (Shiny) 

El proyecto se ha ido realizando en distintos scripts, uno por cada apartado del plan de proyecto. A continuación se expone brevemente, los aspectos tratados en cada uno de los scripts, que corresponden a cada una de las fases del proyecto.

## Audiencia técnica

Todo el contenido se encuentra dentro de la carpeta *Module4_Deep_Analytics* y se divide en diferentes archivos:

### Presentación del proyecto

En la carpeta **PresentacionProyecto** encontramos un archivo .ppt donde encontramos la descripción y presentación del proyecto para el cliente.


### Lectura, descripción, preprocesado y extracción de los datos

Archivo: **ExtraccionY_Preprocesado_Datos.html**

Contenidos:

- Breve explicación del proyecto y objetivos del estudio
- Variables contenidas en los datos
- Extracción de los datos desde SQL
- Preprocesado de los datos:

    * Transformación y creación de variables de variables
    * Unificación de la unidad variables (vatios-hora)
    * Tratamiento de las variables en formato fecha y hora
    * Datos faltantes (1.27 % del total de registros)
    * Creación de distintos datasets con diferente granularidad, que miden el consumo energético por minuto, diario, mensual y semanal
    * Exportación de los datos en cuatro ficheros *R.data*:

        - DF_Energia_GMinutos.RData: Datos completos con ganularidad minutos
        - DF_Energia_GHoras.RData: Datos con granularidad de horas
        - DF_Energia_GDiaria.RData: Datos con granularidad de días
        - DF_Energia_GMensual.RData: Datos con granularidad mensual

### Visualización del consumo energético

Archivo: **AnalisisExploratorio_Graficas.html**
Contenidos:

- Breve análisis exploratorio

    - Descripción de los objetivos
    - Resumen de los datos
- Gráficas del consumo energético con diferentes granularidades:
    
    - Consumo energético diario por minuto
    - Consumo energético semanal
    - Consumo energético mensual
    - Porcentaje de consumo energético (PieCharts):

        - Global
        - Diario por franja horaria
        - Diario para un día concreto
        - Anual
        - Mensual
        - Mensual para un año concreto
        - Comparación para meses de invierno y verano
- Conclusiones

### Análisis de series temporales 

Archivo: **Analisis_Series_Temporales.html**
Contenidos:

- Objetivos
- Estudio de los datos a través de una serie temporal:

    - Creación de un archivo ts
    - Visualización de la serie
    - Pronóstico de la serie sin descomponer
        
        * Pronóstico
        * Intervalo de confianza
        * Resultados de la predicción (error y coeficiente R2)
    - Descomposición de la serie temporal
    - Visualización de la tendencia
    - Suavizado exponencial de Holt-Winters
    - Pronóstico del consumo sin estacionalidad
    - Pronóstico del consumo con estacionalidad
    - Resultados de las predicciones (error y coeficiente R2)
    - Pronóstico para nuevos datos
 - Análisis de la serie temporal con la metodología de Box-Jenkis
 
 Nota: este estudio se ha hecho para las siguientes granularidades:
 
 - Evolución mensual del consumo
 - Evolución semanal del consumo
 - Evolución anual del consumo
 
 
     

## Audiencia cliente

### App shiny

Para lanzar la web, se podrá proceder de dos formas explicadas a continuación

#### Desde GitHub

- Descargar el contenido completo en formato zip desde: https://github.com/marparven1/IOTAnalytics 

    - Para visualizar el contenido de cliente, se procederá de la siguiente forma:
        * 1. Pulsar el desplegable verde *code* y pulsar *download ZIP*
        * 2. A continuación se descargará un archivo ZIP en nuestro equipo
        * 3. Abrir el archivo app.R dentro de la carpeta
        * 4. Configurar la visualización, al pulsar el desplegable a la derecha de *Run app*, podremos configurar la salida de la web (se recomienda seleccioan la opción: **run external**):
     
             - 4.1. Run in window: Para mostrar el contenido en otra pantalla de Rstudio
             - 4.2. Run in terminal: lo mostrará en el terminal dentro de la pantalla de código 
             - 4.3. Run external: salida en una pestaña web
        *  5. Pulsar el botón de Run app en la parte superior derecha del panel de fuente: <img width="101" alt="Captura de pantalla 2022-02-20 a las 13 15 26" src="https://user-images.githubusercontent.com/79573831/154841954-5e17e5b4-7fd1-43c9-a746-32b91979ff4d.png">
 
#### Desde la propia carpeta de proyecto
   
- Para una mayor comodidad para el usuario, se ha preparado el contenido para lanzar la página web dentro de la carpeta *Module4_Deep_Analytics*. El contenido se encuentra dentro de una carpeta llamada *IOT_Analytics*, donde encontramos el archivo *app.R*, abrirlo y continuar con el apartado 4 de la explicación inmediatamente anterior.





