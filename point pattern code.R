# ================================
# POINT PATTERN ANALYSIS - Soy constituency 
# ================================

# Load libraries
library(sf)
library(spatstat)
library(spatstat.geom)

# -------------------------------
# 1. LOAD DATA
# -------------------------------

# Set working directory (CHANGE THIS)
setwd("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2")

# Read shapefiles
    hcf <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/health facility points/clean data/soy health data.shp")          # Health facilities (points)
     w1 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/kapkures ward.shp")
    w2 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/kapsuswa ward.shp")
     w3 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/kipsomba ward.shp")
     w4 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/moi's brigde ward.shp")
    w5 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/segero ward.shp")
     w6 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/soy ward.shp")
     w7 <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/kenya wards/ziwa ward.shp")        # 6 wards
     wards <- rbind(w1, w2, w3, w4, w5, w6, w7)
     const <- st_read("C:/Users/Admin/Documents/3rd YEAR WORK/3.2 WORK/GIS Spatial  analysis/ASSINMENTS/ASSISNMENT  2/data/constituency data/clean/soy constistuency.shp")        # Constituency


# 2. CHECK CRS & PROJECT


# Ensure all data use same CRS (UTM)
hcf <- st_transform(hcf, 32737)
wards <- st_transform(wards, 32737)
const <- st_transform(const, 32737)
-
# 3. CONVERT TO POINT PATTERN


# Extract coordinates
coords <- st_coordinates(hcf)

# Convert boundary to spatstat window
window <- as.owin(st_union(const))

# Create point pattern
ppp_data <- ppp(
  x = coords[,1],
  y = coords[,2],
  window = window
)


# 4. PLOT BASE MAP
--

pdf("map.pdf")

plot(window, main="Health Facilities - Soy Constituency")
plot(ppp_data, add=TRUE, col="red", pch=16)

dev.off()


# 5. QUADRAT DENSITY


Q <- quadratcount(ppp_data, nx=5, ny=5)

pdf("quad_density.pdf")

plot(Q, main="Quadrat Density - Soy")

dev.off()


# 6. QUADRAT PLOT (WITH POINTS)


pdf("quad.pdf")

plot(ppp_data, main="Quadrat Plot")
plot(Q, add=TRUE)

dev.off()


# 7. TESSELLATED DENSITY
-

D <- density(ppp_data)

pdf("density.pdf")

plot(D, main="Tessellated Density")

dev.off()


# 8. KERNEL DENSITY (KDE)

K <- density(ppp_data, sigma=500)

pdf("kde.pdf")

plot(K, main="Kernel Density Estimate (KDE)")

dev.off()



print("All outputs generated successfully!")

