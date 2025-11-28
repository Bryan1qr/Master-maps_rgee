#* sentinel 5p a paso mensual:

library(rgee)
library(tidyverse)
ee_Initialize(project = "ee-bryan1qr", drive = TRUE)

tacna <- ee$FeatureCollection("projects/ee-bryan1qr/assets/geometries/departamentos")$
  filter(ee$Filter$eq("NOMBDEP", "TACNA"))


fecha_inicio <- ee$Date("2019-01-01T00:00:00Z")$advance(5, "hour")
fecha_fin    <- ee$Date("2025-01-01T00:00:00Z")$advance(5, "hour")
sentinel_filtrado <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")$
  select("tropospheric_NO2_column_number_density")$
  filterDate(fecha_inicio, fecha_fin)


years <- ee$List$sequence(2019, 2024)

# ---- FUNCION MENSUAL ----
monthly_images <- years$map(
  ee_utils_pyfunc(function(y) {
    
    year <- ee$Number(y)
    months <- ee$List$sequence(1, 12)
    
    months$map(
      ee_utils_pyfunc(function(m) {
        
        month <- ee$Number(m)
        
        start <- ee$Date$fromYMD(year, month, 1)
        end   <- start$advance(1, "month")
        
        img <- sentinel_filtrado$
          filterDate(start, end)$
          median()$
          rename(
            ee$String("no2_")
              $cat(year$format("%d"))
              $cat("_")
              $cat(month$format("%02d"))
          )
        
        img$set("year", year)$set("month", month)
      })
    )
  })
)

# Aplanar lista de listas a lista simple
monthly_flat <- monthly_images$flatten()

# Convertir lista a ImageCollection
monthly_collection <- ee$ImageCollection$fromImages(monthly_flat)

# Convertir a bandas
monthly_bands <- monthly_collection$toBands()


old_names <- monthly_bands$bandNames()

clean_names <- old_names$map(
  ee_utils_pyfunc(function(n) {
    ee$String(n)$replace('^[0-9]+_', '')
  })
)

monthly_bands_clean <- monthly_bands$rename(clean_names)

stats <- monthly_bands_clean$reduceRegions(
  collection = tacna,
  reducer   = ee$Reducer$mean(),
  scale     = 1113
)

stats_sf <- ee_as_sf(stats)

stats_df <- stats_sf %>%
  select(starts_with("no2_")) %>%
  pivot_longer(
    cols = starts_with("no2_"),
    names_to = "date_code",
    values_to = "no2"
  ) %>%
  mutate(
    year = as.integer(str_sub(date_code, 5, 8)),
    month = as.integer(str_sub(date_code, 10, 11)),
    date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"),
    no2 = no2
  ) %>%
  arrange(date) %>%
  sf::st_drop_geometry()


df2 <- stats_df %>% 
  summarise(no2 = median(no2, na.rm = TRUE), .by = year)
# Graficando:

g1 <- ggplot() +
  # Boxplots
  stat_boxplot(
    geom = "errorbar",
    aes(x = as.numeric(year),
        y = no2 * 1e6,
        group = year
    ),
    data = stats_df, width = 0.2
  ) + geom_violin(aes(x = as.numeric(year),
        y = no2 * 1e6,
        group = year
    ),
    data = stats_df,
  width = 1) +
  geom_boxplot(
    aes(x = as.numeric(year),
        y = no2 * 1e6,
        group = year
    ),
    data = stats_df,
    color = "gray10",
    fill  = "white",
    outliers = FALSE, width = 0.25
  ) +

  # Jitter = promedios areales
  geom_jitter(
    aes(x = as.numeric(year),
        y = no2* 1e6,
        color = "Promedios areales"
    ),
    data = stats_df,
    size = 4, alpha = 0.6, width = 0.12
  ) +

  # Línea de medianas
  geom_line(
    aes(x = as.numeric(year),
        y = no2 * 1e6,
        color = "Mediana"
    ),
    data = df2,
    linewidth = 1.5
  ) +

  # Punto de medianas
  geom_point(
    aes(x = as.numeric(year),
        y = no2* 1e6,
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
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text  = element_text(size = 14),
    legend.background = element_blank()
  ) +

  # Etiquetas
  labs(
    x = "",
    y = expression("μmol/m"^2)
  ) +

  scale_x_continuous(breaks = seq(2019, 2024, 1)) +

  # -------------------------
  # 🎨 Leyenda personalizada
  # -------------------------
  scale_color_manual(
    values = c(
      "Promedios areales" = "#721f81",
      "Mediana"        = "hotpink"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Mediana" = 17   # triángulo
    )
  )


ggsave("graph1.svg", 
plot = g1, units = "cm",
 height = 7, width = 36, 
 dpi = 200, bg = "transparent")
  
