# ============================================================
# 01_setup.R - Recharge l'environnement de travail
# ============================================================

# Librairies
library(sf)
library(mapsf)
library(happign)
library(terra)

# Rechargement des données depuis base.gpkg (instantané)
clermont     <- st_read("base.gpkg", "commune_clermont")
senlis       <- st_read("base.gpkg", "commune_senlis")
car_clermont <- st_read("base.gpkg", "carreaux_clermont")
car_senlis   <- st_read("base.gpkg", "carreaux_senlis")

# Vérification
clermont$nom_com
senlis$nom_com
nrow(car_clermont)
nrow(car_senlis)
# ============================================================
# TEST : téléchargement orthophoto IGN
# ============================================================
library(happign)
library(terra)

# Test : téléchargement orthophoto IGN pour Clermont (résolution 10m)
r_clermont <- get_wms_raster(clermont,
                             layer = "ORTHOIMAGERY.ORTHOPHOTOS",
                             res = 10,
                             crs = 2154,
                             rgb = TRUE,
                             filename = "ortho_clermont.tif",
                             overwrite = TRUE,
                             verbose = TRUE)

# Lecture du raster
r_cl <- rast("ortho_clermont.tif")

# Découpage strict sur le contour de la commune
r_cl <- crop(r_cl, vect(clermont), mask = TRUE)

# Affichage
plotRGB(r_cl, main = "Orthophoto IGN - Clermont")
# ============================================================
# Téléchargement orthophoto Senlis + traitement complet
# ============================================================

# 1) Téléchargement orthophoto Senlis
r_senlis <- get_wms_raster(senlis,
                           layer = "ORTHOIMAGERY.ORTHOPHOTOS",
                           res = 10, crs = 2154, rgb = TRUE,
                           filename = "ortho_senlis.tif",
                           overwrite = TRUE,
                           verbose = TRUE)

r_se <- rast("ortho_senlis.tif")
r_se <- crop(r_se, vect(senlis), mask = TRUE)

# 2) Affichage côte à côte
par(mfrow = c(1, 2))
plotRGB(r_cl, main = "Orthophoto - Clermont")
plotRGB(r_se, main = "Orthophoto - Senlis")
par(mfrow = c(1, 1))

# 3) Conversion en niveaux de gris
r_cl_gris <- mean(r_cl[[1:3]])
r_se_gris <- mean(r_se[[1:3]])

# 4) Seuillage binaire (bâti = clair > 100, végétation = sombre)
r_cl_seuil <- r_cl_gris > 100
r_se_seuil <- r_se_gris > 100

# 5) Affichage des masques binaires
par(mfrow = c(1, 2))
plot(r_cl_seuil, main = "Bâti vs végétation - Clermont",
     col = c("darkgreen", "beige"))
plot(r_se_seuil, main = "Bâti vs végétation - Senlis",
     col = c("darkgreen", "beige"))
par(mfrow = c(1, 1))

# 6) Croisement raster × vecteur : % de bâti par carreau Filosofi
car_clermont$pct_bati <- terra::extract(r_cl_seuil, vect(car_clermont),
                                        fun = mean, na.rm = TRUE)[, 2] * 100
car_senlis$pct_bati   <- terra::extract(r_se_seuil, vect(car_senlis),
                                        fun = mean, na.rm = TRUE)[, 2] * 100

# 7) Cartes choroplèthes du % de bâti
par(mfrow = c(1, 2))
mf_map(car_clermont, var = "pct_bati", type = "choro",
       nbreaks = 5, pal = "Grey", border = "white",
       leg_title = "% de bâti", leg_pos = "topleft")
mf_layout(title = "Densité du bâti - Clermont", credits = "IGN | Wazeuh")

mf_map(car_senlis, var = "pct_bati", type = "choro",
       nbreaks = 5, pal = "Grey", border = "white",
       leg_title = "% de bâti", leg_pos = "topleft")
mf_layout(title = "Densité du bâti - Senlis", credits = "IGN | Wazeuh")
par(mfrow = c(1, 1))

# 8) Sauvegarde
st_write(car_clermont, "base.gpkg", "carreaux_clermont", delete_layer = TRUE)
st_write(car_senlis,   "base.gpkg", "carreaux_senlis",   delete_layer = TRUE)
writeRaster(r_cl_seuil, "masque_clermont.tif", overwrite = TRUE)
writeRaster(r_se_seuil, "masque_senlis.tif", overwrite = TRUE)