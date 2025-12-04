library(rgee)
ee_Initialize(project = "ee-bryan1qr", drive = TRUE)

# Assets de mi pertenencia:
ee_manage_assetlist()

# Filtrado de mi departamento:
tacna <- ee$FeatureCollection("projects/ee-bryan1qr/assets/geometries/departamentos")$
  filter(ee$Filter$eq("NOMBDEP", "TACNA"))

# Carga de la colección de imagenes SENTINEL 5P:

fecha_inicio <- ee$Date("2019-01-01T00:00:00Z")$advance(5, "hour")
fecha_fin    <- ee$Date("2025-01-01T00:00:00Z")$advance(5, "hour")

sentinel_filtrado <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")$
  select("tropospheric_NO2_column_number_density")$
  filterDate(fecha_inicio, fecha_fin)

# df <- ee_get_date_ic(sentinel_filtrado)

años <- seq(2019, 2024)

mediana_anual_fun <- ee_utils_pyfunc(function(y) {
  y <- ee$Number(y)
  inicio <- ee$Date$fromYMD(y, 1, 1)$advance(5, "hour")
  fin <- inicio$advance(1, "year")

  imagen_anual <- sentinel_filtrado$
    filterDate(inicio, fin)$
    median()$
    set("year", y)

  return(imagen_anual)
})

no2_median_anual <- ee$ImageCollection(ee$List(años)$map(mediana_anual_fun))

paleta <- list(
  min = 0,
  max = 0.00002,
  palette = c('black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red')
)

# Verificando:

Map$centerObject(eeObject = tacna, zoom = 7)
Map$addLayer(eeObject = no2_median_anual$first()$clip(tacna),
 visParams = paleta, name = "tropomi") +
  Map$addLayer(eeObject = tacna$style(
    color = "white", fillColor = "#ffffff00", width = 0.5
  ), name = "Tacna")


# Método 1:
# Más eficiente:

fechas <- ee$List(2019:2024)$map(ee_utils_pyfunc(function(i) ee$String(i)))

img_multibands <- no2_median_anual$toBands()$
  rename(fechas)$multiply(1e6) # renombrando y pasando a microgramos:

task2 <- ee_image_to_drive(
  image = img_multibands,
  description = "tropomi",
  folder = "MasterMaps2025",
  region = tacna$geometry(), 
  scale = 1113.2, 
  crs = "EPSG:4326", 
  maxPixels = 1e13
)

# O directamente en local:
sent3 <- ee_as_rast(img_multibands, 
  region = tacna$geometry(),
   via = "drive",
   container = "test3",
  scale = 1113.2, 
  crs = "EPSG:4326", 
  maxPixels = 1e13)


# Split de bandas:
library(terra)
d <- as.list(sent3)
dir.create("raster2")
bandas <- names(sent3)
lapply(1:length(d), function(i) {
  writeRaster(d[[i]],
              filename = file.path("raster", paste0("no2_", bandas[i], ".tif")),
              overwrite = TRUE)
})



# Opción2
#  Exportar cada imagen a Google Drive
folder_drive <- "NO2_Tacna"   # Cambia el nombre de la carpeta si deseas

# Iterar sobre cada año y exportar
for (year in años) {
  image_year <- no2_median_anual$
    filter(ee$Filter$eq("year", year))$
    first()

  task <- ee$batch$Export$image$toDrive(
    image = image_year,
    description = paste0("NO2_median_", year),
    folder = folder_drive,
    fileNamePrefix = paste0("NO2_median_", year),
    region = tacna$geometry(),
    scale = 1113.2,              # escala original del Sentinel-5P
    crs = "EPSG:4326",           # sistema de coordenadas global
    maxPixels = 1e13
  )
  task$start()
  Sys.sleep(5)
  print(paste("Exportando:", year))
}