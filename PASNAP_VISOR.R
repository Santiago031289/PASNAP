library(leaflet)
library(sf)
library(dplyr)
library(leafem)
library(htmltools)
library(leaflet.extras)
library(leafpop)
library(readtext)
library(stringr)
library(purrr)
library(tibble)


# 1. Leer el texto desde el Word
doc <- readtext("C:/Users/dtimae/Downloads/POA1.docx")$text

# 2. Reemplazar saltos de línea múltiples con \n limpio
doc <- str_replace_all(doc, "\\r\\n|\\r|\\n", "\n")

# 3. Separar las secciones por líneas que empiecen con "Nombre:"
secciones <- str_split(doc, "(?=\\nNombre\\s*:)", simplify = FALSE)[[1]]

# 4. Normalizador de nombre
normalizar_nombre <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9ÁÉÍÓÚÜÑ]", "") %>%
    str_trim()
}

# 5. Función para procesar una sección
procesar_ap <- function(seccion) {
  lineas <- str_split(seccion, "\n")[[1]] %>% str_trim()
  lineas <- lineas[lineas != ""]
  
  nombre_linea <- lineas[str_detect(lineas, "^Nombre\\s*:")]
  if (length(nombre_linea) == 0) return(NULL)
  
  nombre <- str_remove(nombre_linea[1], "^Nombre\\s*:\\s*") %>% str_trim()
  if (nombre == "") return(NULL)
  
  detalles <- lineas[str_detect(lineas, "^Detalle\\s*:")]
  if (length(detalles) == 0) return(NULL)
  
  map_dfr(detalles, function(det) {
    det <- str_remove(det, "^Detalle\\s*:\\s*")
    partes <- str_split_fixed(det, "=", 2)
    if (ncol(partes) < 2 || any(nchar(partes) == 0)) return(NULL)
    
    categoria <- str_trim(partes[1]) %>%
      str_to_title() %>%
      str_replace("Consultoria", "Consultoría") %>%
      str_replace("Convenio", "Convenios") %>%
      str_replace("Servicios", "Servicios")
    
    texto <- str_trim(partes[2])
    monto <- str_extract(texto, "\\$\\s*[\\d.,]+")
    descripcion <- ifelse(!is.na(monto), str_trim(str_remove(texto, fixed(monto))), texto)
    
    monto_num <- if (!is.na(monto)) {
      monto %>%
        str_remove_all("[\\$\\s]") %>%
        str_replace_all("\\.", "") %>%
        str_replace_all(",", ".") %>%
        as.numeric()
    } else {
      NA_real_
    }
    
    tibble(
      nombre_ap = nombre,
      nombre_norm = normalizar_nombre(nombre),
      categoria = categoria,
      detalle = descripcion,
      monto = monto_num
    )
  })
}

# 6. Aplicar el procesamiento
poa_detalles <- map_df(secciones, procesar_ap)

# 7. Verificación
print(unique(poa_detalles$nombre_ap))      # ✅ debe mostrar nombres reales
print(length(unique(poa_detalles$nombre_ap)))  # ✅ debe mostrar 28


# Leer shapefiles y unir info del POA
Areas_Priorizadas <- st_read("C:/Users/dtimae/Documents/PASNAP/SHAPE/AP_PRIORIZADAS1.shp") %>%
  st_transform(4326)

# Verifica qué columnas tienes disponibles
print(names(Areas_Priorizadas))

# Normalizar nombre y mantener estructura
Areas_Priorizadas_filtered <- Areas_Priorizadas %>%
  mutate(nombre_norm = normalizar_nombre(Nombre)) %>%
  st_as_sf()

# Si no tienes columnas como Infraestructura, etc., las agregamos vacías para evitar errores:
if (!"Infraestructura" %in% colnames(Areas_Priorizadas_filtered)) {
  Areas_Priorizadas_filtered$Infraestructura <- NA
  Areas_Priorizadas_filtered$Bienes <- NA
  Areas_Priorizadas_filtered$Consultoria <- NA
  Areas_Priorizadas_filtered$Monto <- NA
  Areas_Priorizadas_filtered$Convenio <- NA
  Areas_Priorizadas_filtered$Estado <- NA
}

# Leer provincias
Provincias <- st_read("D:/Quinsaloma/QUINSALOMA-20240624T125729Z-001/QUINSALOMA/RECORTE-QUINSALOMA/MXD-MAPA-BASE/PROVIN/LIMITE_PROVINCIAL_CONALI_CNE_2022.shp") %>%
  st_transform(4326)

# Leer y transformar APH y BVP
APH <- st_read("C:/Users/dtimae/Documents/PASNAP/SHAPE/APH.shp") %>%
  st_transform(4326)

BVP <- st_read("C:/Users/dtimae/Documents/PASNAP/SHAPE/BVP.shp") %>%
  st_transform(4326)
# Normalizar nombres
APH <- APH %>%
  mutate(nombre_norm = normalizar_nombre(nam)) %>%
  st_as_sf()

BVP <- BVP %>%
  mutate(nombre_norm = normalizar_nombre(nombre)) %>%
  st_as_sf()



# Crear logos
logo <- "https://github.com/Santiago031289/EPCONST/raw/main/KFW.jpeg"
logo1 <- "https://github.com/Santiago031289/EPCONST/raw/main/PASNAP1.jpeg"

logos_html <- paste0(
  '<div style="display: flex; align-items: center; background: white; padding: 5px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2);">',
  '<img src="', logo, '" style="height: 80px; margin-right: 10px;">',
  '<img src="', logo1, '" style="height: 80px;">',
  '</div>'
)

# Iconos
df <- data.frame(
  win = c("Muelle San Lorenzo", "Majagual", "Manglares Churute", "Arenillas"),
  image = c("https://github.com/Santiago031289/EPCONST/raw/main/San%20Lorenzo.jpeg",
            "https://github.com/Santiago031289/EPCONST/raw/main/Cayapas-Mataje.jpeg",
            "https://github.com/Santiago031289/EPCONST/raw/main/PASNAP1.jpeg",
            "https://github.com/Santiago031289/EPCONST/raw/main/Arenillas.jpeg"),
  lat = c(1.292287, 1.173609, -2.42730, -3.566207),
  lon = c(-78.836031, -79.079381, -79.66179, -80.142660)
)

# Función popup
create_whatsapp_table <- function(nombre_norm) {
  detalles <- poa_detalles %>% filter(nombre_norm == !!nombre_norm)
  
  if (nrow(detalles) == 0) {
    return(paste0(
      "<div style='font-family: \"Segoe UI\", sans-serif; font-size: 13px; max-width: 300px;'>",
      "<div style='font-weight: bold; color: #128C7E; font-size: 16px;'>", nombre_norm, "</div>",
      "<div style='color: #EA4335; margin-top: 8px;'>No hay información disponible</div>",
      "</div>"
    ))
  }
  
  iconos <- c(
    "Infraestructura" = "🏗️",
    "Bienes" = "📦",
    "Consultoría" = "🧠",
    "Convenios" = "🤝",
    "Servicios" = "🛎️"
  )

  
  categorias <- c("Infraestructura", "Bienes", "Consultoría", "Convenios", "Servicios")
  
  bloques <- map_chr(categorias, function(cat) {
    sub <- detalles %>% filter(categoria == cat)
    if (nrow(sub) == 0) return("")
    
    items <- paste0(
      "<li style='margin-bottom: 4px;'>",
      sub$detalle,
      ifelse(!is.na(sub$monto),
             paste0(" - <span style='color:#128C7E; font-weight:bold;'>$",
                    formatC(sub$monto, format = "f", digits = 2, big.mark = ",", decimal.mark = "."),
                    "</span>"),
             ""),
      "</li>"
    )
    
    
    paste0(
      "<div style='margin-bottom: 12px;'>",
      "<div style='color: #333; font-weight: bold; margin-bottom: 4px;'>", iconos[[cat]], " ", cat, "</div>",
      "<ul style='padding-left: 18px; margin: 0;'>", paste(items, collapse = ""), "</ul>",
      "</div>"
    )
  })
  
  paste0(
    "<div style='font-family: \"Segoe UI\", sans-serif; font-size: 13px; max-width: 320px; background: #fff; border-radius: 10px; padding: 14px; box-shadow: 0 2px 12px rgba(0,0,0,0.15);'>",
    "<div style='font-weight: bold; font-size: 16px; color: #128C7E; margin-bottom: 10px; border-bottom: 1px solid #eee; padding-bottom: 5px;'>",
    unique(detalles$nombre_ap), "</div>",
    paste(bloques[bloques != ""], collapse = ""),
    "</div>"
  )
}

# Botón de descarga
download_btn <- tags$a(
  href = "www/POA_2025_PASNAP.xlsx",
  download = NA,
  target = "_blank",
  class = "btn btn-success",
  style = "background-color: #28a745; color: white; padding: 10px; border-radius: 5px; text-decoration: none;",
  "⬇️ Descargar Excel POA"
)

# Crear popups para APH y BVP
popup_aph <- lapply(APH$nombre_norm, create_whatsapp_table)
popup_bvp <- lapply(BVP$nombre_norm, create_whatsapp_table)

# Mapa
leaflet(df) %>% 
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=p&x={x}&y={y}&z={z}",
           attribution = 'Google Terrain',
           group = "Google Terrain") %>%
  addControl(html = logos_html, position = "topleft") %>%
  addControl(html = download_btn, position = "bottomleft") %>%
  {if(exists("Provincias")) addPolygons(., data = Provincias,
                                        fillColor = "#8B4513",
                                        fillOpacity = 0.10,
                                        color = "#8B4513",
                                        weight = 1.5,
                                        popup = ~paste('<div style="font-family: Arial; font-size: 13px;"><b>Provincia:</b><br>', PROVINCIA, '</div>'),
                                        group = "Provincias") else .} %>%
  addPolygons(data = Areas_Priorizadas_filtered,
              fillColor = "#FF5733",
              fillOpacity = 0.7,
              color = "#C70039",
              weight = 3,
              popup = lapply(Areas_Priorizadas_filtered$nombre_norm, create_whatsapp_table),
              group = "Áreas Protegidas") %>%
  addSearchFeatures(
    targetGroups = c("Provincias"),
    options = searchOptions(
      zoom = 10,
      position = "topleft",
    )
  ) %>%
  addPolygons(data = APH,
              fillColor = "#63e4d9",
              fillOpacity = 0.5,
              color = "#0e0680",
              weight = 2,
              popup = popup_aph,
              group = "APH") %>%
  
  addPolygons(data = BVP,
              fillColor = "#128006",
              fillOpacity = 0.5,
              color = "#62f73e",
              weight = 2,
              popup = popup_bvp,
              group = "BVP") %>%
  addEasyButton(
    easyButton(
      icon = "fa-globe", 
      title = "Volver a vista general",
      onClick = JS("function(btn, map){ map.setView([-1.8312, -78.1834], 6); }")
    )
  ) %>%
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   popup = ~paste0(
                     '<div style="font-family: Arial; width: 280px; background: white; border-radius: 10px; overflow: hidden;">',
                     '<div style="padding: 12px; background: #128C7E; color: white; font-weight: bold;">', win, '</div>',
                     '<img src="', image, '" style="width: 100%; height: auto;">',
                     '<div style="padding: 10px; background: #f9f9f9;">📍 Ubicación específica</div></div>'
                   ),
                   color = "#34B7F1",
                   radius = 7,
                   stroke = FALSE,
                   fillOpacity = 0.9,
                   group = "Construcciones") %>%
  addMouseCoordinates() %>%
  addMeasure(position = "topleft", 
             primaryLengthUnit = "meters", 
             primaryAreaUnit = "sqmeters",
             activeColor = "#128C7E",
             completedColor = "#25D366") %>%
  addControlGPS(options = gpsOptions(position = "topleft", 
                                     activate = TRUE, 
                                     autoCenter = TRUE,
                                     setView = TRUE)) %>%
  addLayersControl(
    baseGroups = c("Google Terrain"),
    overlayGroups = c("Provincias", "Áreas Protegidas", "Construcciones", "APH", "BVP"),
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
  ) %>%
  addMiniMap(tiles = providers$Esri.WorldImagery,
             toggleDisplay = TRUE,
             position = "bottomright",
             width = 120,
             height = 120,
             minimized = FALSE) %>%
  addLegend(
    position = "bottomleft",
    colors = c("#FF5733", "#128006", "#63e4d9"),
    labels = c("Áreas Priorizadas", "BVP", "APH"),
    title = "Capas",
    opacity = 1
  )

