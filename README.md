# PVCalcAPP
Aplicacion para estudios de intercambiabilidad laboratorio clinico realizada con Shiny.
Aplicación subida a https://pvillalbac.shinyapps.io/PVCalcApp/



Creación de una aplicación para realizar intercomparación mediante Bland Altman y Passing Babblok. Con la posibilidad de generar informes.

## Inputs

* Cargar un archivo excel con la estructura siguiente:
  - primera fila columna A. Nombre del equipo/método 1
  - primera fila Columna B. Nombre del equipo/método 2

## Parámetros

* Limite de acuerdo: es el criterio de aceptación que se establece para el límite de acuerdo en el análisis de diferencias entre los dos métodos
* Criterio de aceptación Passing Babblok: Es el criterio de aceptación para el error proporcional (pendiente) para la regresión Passing Babblok.

## Informe

La aplicación realiza un informe a traves de la plantilla Informe.Rmd
Indicando los errores sistemáticos y proporcionales encontrados y si cumplen los criterios de aceptación previamente establecidos.
