# Mining Spatial Data

wst
attach(wst)
install.packages("sp")
install.packages("raster")
library(sp)
library(raster)

wst

###Spatial Vector Form Data
#Spatial Points Data
lonlat = cbind(longitude,latitude)
pts = SpatialPoints(lonlat)
class(pts)
plot(pts)
pts


#define corrdinate sistem
crdref =CRS('+proj=longlat +datum=WSG84')
pts = SpatialPoints(lonlat, proj4string = crdref)

#embed the atribut  to SpatialPoint vector
df = data.frame(ID=name,precip)
ptsdf = SpatialPointsDataFrame(pts,data=df)
plot(ptsdf)

showDefault(ptsdf)

#spital Line Data
lonlat = cbind(longitude,latitude)
linesD = spLines(lonlat,crs = crdref)
plot(linesD)

#spital Plyon Data
lonlat = cbind(longitude,latitude)
polyD = spPolygons(lonlat,crs = crdref)
plot(polyD)


###Spatial form Raster Data
r = raster(ncol=10,nrow=10,xmx=-80,xmn=-150,ymn=20,ymx=60)

area = rnorm(ncell(r))
values(r)=area
plot(r)

#Raster Stack & Reaster Brick
r2 =r*r
r3 =r^3
r4 = 2*r2+r3
s=stack(r,r2,r3,r4)

plot(s)


###Data Manipulation for Spatial Vector
f =system.file("external/lux.shp",package = "raster")
f
p= shapefile(f)
p

#presentingh in data frame format
d =data.frame(p)

#extrract specfic artibutes
p$NAME_1
p$AREA

#add new variable
set.seed(12)
xp = sample(letters,length(p))
p$new = xp
data.frame(p)


##del varible
p$new=NULL
p
data.frame(p)

# Data integration
dfr <- data.frame(District=p$NAME_1, Canton = p$NAME_2,
                  Value = runif(length(p)))
cD <- merge(p, dfr, by.x = c('NAME_1', 'NAME_2'), 
            by.y = c('District', 'Canton'))

# Data Aggregation
pa <- aggregate(p, by = 'NAME_1')
plot(p)
plot(pa, add = TRUE, col = rainbow(3))
data.frame(p)

pa <- aggregate(p, by = 'NAME_2')
plot(p)
plot(pa, add = TRUE, col = rainbow(12))
data.frame(p)
