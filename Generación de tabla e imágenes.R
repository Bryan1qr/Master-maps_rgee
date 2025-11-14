library(rgee)
ee_Initialize(project = "ee-bryan1qr", drive = TRUE)

# Assets de mi pertenencia:
ee_manage_assetlist()

# Filtrado de mi departamento:
tacna <- ee$FeatureCollection("projects/ee-bryan1qr/assets/geometries/departamentos")$
  filter(ee$Filter$eq("NOMBDEP", "TACNA"))

# Verificando:
Map$centerObject(eeObject = tacna, zoom = 7)
Map$addLayer(eeObject = tacna, name = "Tacna")

#* Carga de la colección de imagenes SENTINEL 5P:

fecha_inicio <- ee$Date("2019-01-01T00:00:00Z")$advance(5, "hour")
fecha_fin    <- ee$Date("2024-12-31T23:00:00Z")$advance(5, "hour")

sentinel_filtrado <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")$
  select("tropospheric_NO2_column_number_density")$
  filterDate(fecha_inicio, fecha_fin)

df <- ee_get_date_ic(sentinel_filtrado)

muestra <- sentinel$median()$clip(tacna)
paleta <- list(
  min = 0,
  max = 0.00002,
  palette = c('black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red')
)
Map$centerObject(eeObject = tacna, zoom = 7)
Map$addLayer(eeObject = no2_median_anual$first()$clip(tacna), visParams = paleta, name = "tropomi")


# Calcular promedio areal por imagen sobre la zona de estudio
promedios_areales <- sentinel_filtrado$
  map(function(img) {
    # Recortar la imagen al área de estudio
    img_clip <- img$clip(tacna)

    # Reducir al promedio dentro del área
    mean_dict <- img_clip$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = tacna$geometry(),
      scale = 1113,
      maxPixels = 1e13
    )

    # Obtener el valor medio (si existe)
    mean_value <- ee$Number(mean_dict$get("tropospheric_NO2_column_number_density"))

    # Crear el feature con control de NULL
    return(
      ee$Feature(NULL, list(
        "system:index" = img$get("system:index"),
        "fecha" = img$date()$format("YYYY-MM-dd"),
        "NO2_mean" = ee$Algorithms$If(mean_value, mean_value, ee$Number(-9999))
      ))
    )
  })

# Crear la tabla final
tabla_no2 <- ee$FeatureCollection(promedios_areales)

tabla_no2_valid <- tabla_no2$
  filter(ee$Filter$gt("NO2_mean", 0))

task_table <- ee_table_to_drive(
  collection = tabla_no2,
  description = "NO2_promedios_Tacna",
  fileFormat = "CSV",
  folder = "definitivo"
)
task_table$start()

ee_monitoring(task = task_table)


df1 <- read.csv("NO2_promedios_Tacna_2025_11_11_22_19_51.csv")


anios <- seq(2019, 2024)

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

no2_median_anual <- ee$ImageCollection(ee$List(anios)$map(mediana_anual_fun))

#  Exportar cada imagen a Google Drive
folder_drive <- "NO2_Tacna_Medianas"   # Cambia el nombre de la carpeta si deseas

# Iterar sobre cada año y exportar
for (year in anios) {
  image_year <- no2_median_anual$
    filter(ee$Filter$eq("year", year))$
    first()$
    clip(tacna)

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
  print(paste("Exportando:", year))
}

df <- read.csv("NO2_promedios_Tacna_2025_11_11_22_19_51.csv")

df1 <- df %>% select(system.index, NO2_mean, fecha) %>%
  filter(NO2_mean >= 0)

df2 <- df1 %>% 
  mutate(fecha = as.Date(fecha),
         year = format(fecha, format = "%Y")) %>%
  summarise(NO2_median = median(NO2_mean, na.rm = TRUE), .by = year)

df3 <- df1 %>%
  mutate(fecha = as.Date(fecha),
         year = format(fecha, format = "%Y"))


g1 <- ggplot() +
  # Boxplots
  stat_boxplot(
    geom = "errorbar",
    aes(x = as.numeric(year),
        y = NO2_mean * 1e6,
        group = year
    ),
    data = df3, width = 0.2
  ) +
  geom_boxplot(
    aes(x = as.numeric(year),
        y = NO2_mean * 1e6,
        group = year
    ),
    data = df3,
    color = "gray60",
    fill  = "white",
    outliers = FALSE
  ) +

  # Jitter = promedios areales
  geom_jitter(
    aes(x = as.numeric(year),
        y = NO2_mean * 1e6,
        color = "Promedios areales"
    ),
    data = df3,
    size = 3, alpha = 0.25, width = 0.15
  ) +

  # Línea de medianas
  geom_line(
    aes(x = as.numeric(year),
        y = NO2_median * 1e6,
        color = "Mediana"
    ),
    data = df2,
    linewidth = 1
  ) +

  # Punto de medianas
  geom_point(
    aes(x = as.numeric(year),
        y = NO2_median * 1e6,
        color = "Mediana"
        ),
    data = df2,
    size = 4,
    alpha = 0.8,
    shape = 17
  ) +

  # Tema
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    axis.line        = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 15),
    legend.title = element_blank(),
    legend.text  = element_text(size = 14),
    legend.background = element_blank()
  ) +

  # Etiquetas
  labs(
    x = "",
    y = expression(mu * "mol/m"^2)
  ) +

  scale_x_continuous(breaks = seq(2019, 2024, 1)) +

  # -------------------------
  # 🎨 Leyenda personalizada
  # -------------------------
  scale_color_manual(
    values = c(
      "Promedios areales" = "#721f81",
      "Mediana"        = "red"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Mediana" = 17   # triángulo
    )
  )


ggsave("graph1.svg", 
plot = g1, units = "cm",
 height = 7, width = 33, 
 dpi = 200, bg = "transparent")
  

