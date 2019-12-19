var treeCover = hansen.select(['treecover2000']);
var greater10 = treeCover.gte(10);  // this is a mask

Map.addLayer(greater10, {}, 'hansen');

var image = imageCollection
              .filterBounds(geometry)
              .filterDate('2000-07-01', '2001-06-30')
              .select(['BurnDate']);
  
var composite = image.max().gte(1).int16().rename('x2001');
print(composite);

for (var year = 2002; year <= 2019; year++) {
  
  var firstdate = (year-1)+'-07-01';
  var seconddate = year+'-06-30';

  var image1 = imageCollection
              .filterBounds(geometry)
              .filterDate(firstdate, seconddate)
              .select(['BurnDate']);
  
  var composite1 = image1.max().gte(1).int16().rename('x'+year);
  composite = composite.addBands(composite1);
  
}

var forest_comp = composite.multiply(greater10);

print(forest_comp);

Export.image.toDrive({
  image: forest_comp,
  description: 'modis_forest_fires_aus',
  folder: 'modis_fires_2',
  scale: 250,
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:3577',
  region: geometry,
  maxPixels: 1e10
});

Export.image.toDrive({
  image: greater10,
  description: 'forest_extent_aus',
  folder: 'modis_fires_2',
  scale: 250,
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:3577',
  region: geometry,
  maxPixels: 1e10
});


