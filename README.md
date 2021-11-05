# VisorPSD

Proyecto diseñado con el fin de ver toda los datos que publica el Departamento de Agricultura de Estados Unidos.

Se compone de diversos scripts:

- **UpdateData.R**
    - Descarga el archivo zip del PSD-USDA, lo descomprime y lo guarda en *psd_alldata.csv*.

- **app.R**
    - Shinny app que toma el *psd_alldata.csv* y muestra en formato de tabla/gráficos los datos de los distintos commodities, países, variables comerciales y años. Adicionalmente, incluye la opción de visualizar todos los datos en relación a la participación que tienen del total y agregarlo a nivel mundial y mundial sin contar China. Contiene un botón de descarga de los datos filtrados.

- **Run.R**
    - Perimite correr en local la aplicación.

## Proceso para actualizar el proyecto
Para correr el proyecto solamente se debería:
1. Correr el script de *UpdateData.R*
2. Correr el script *Run.R*


