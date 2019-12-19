var treeCover = hansen.select(['treecover2000']);
var greater10 = treeCover.gte(10)  // this is a mask

Map.addLayer(greater10, {}, 'hansen')

var image = imageCollection
              .filterBounds(geometry)
              .filterDate('2001-01-01', '2001-12-31')
              .select(['BurnDate']);
  
var composite = image.max().gte(1).int16().rename('x2001');
print(composite);

for (var year = 2002; year <= 2019; year++) {
  
  // Northern hemisphere
  var firstdate = year+'-01-01';
  var seconddate = year+'-12-31';
  
  var image1 = imageCollection
              .filterBounds(geometry)
              .filterDate(firstdate, seconddate)
              .select(['BurnDate']);
              
  //print(image1);
  
  var composite1 = image1.max().gte(1).int16().rename('x'+year);
  composite = composite.addBands(composite1);
  
}

var forest_comp = composite.multiply(greater10);

print(forest_comp);

var wkt = 'PROJCS["Canada_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["North_American_Datum_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["longitude_of_center",-96],PARAMETER["Standard_Parallel_1",50],PARAMETER["Standard_Parallel_2",70],PARAMETER["latitude_of_center",40],UNIT["Meter",1],AUTHORITY["EPSG","102001"]]';

Export.image.toDrive({
  image: forest_comp,
  description: 'modis_forest_fires_Canada',
  folder: 'modis_fires_2',
  scale: 250,
  fileFormat: 'GeoTIFF',
  crs: wkt,
  region: geometry,
  maxPixels: 1e10
});

Export.image.toDrive({
  image: greater10,
  description: 'forest_extent_Canada',
  folder: 'modis_fires_2',
  scale: 250,
  fileFormat: 'GeoTIFF',
  crs: wkt,
  region: geometry,
  maxPixels: 1e10
});



