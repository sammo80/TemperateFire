Map.addLayer(table);

var treeCover = ee.Image('UMD/hansen/global_forest_change_2018_v1_6').select(['treecover2000']);
var greater10 = treeCover.gte(10);  // this is a mask


// load the LandTrendr.js module
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js');

// define parameters
var startYear = 1991;
var endYear = 2019;
var startDay = '01-01';
var endDay = '04-01';
var aoi = geometry;
var maskThese = ['cloud', 'shadow', 'snow']

// add NBR band
var addNBR = function(image) {
  var nbr = image.normalizedDifference(['B4', 'B7'])
    .rename('NBR')
    .multiply(1000)
    .int16();
  return image.addBands(nbr);
};

// apply LandTrendr.js functions
var annualSRcollection = ltgee.buildSRcollection(startYear, endYear, startDay, endDay, aoi, maskThese);

var NBRcollection = annualSRcollection.map(addNBR).select('NBR');

var stackCollection = function(collection) {
  // Create an initial image.
  var first = ee.Image(collection.first()).select([]);

  // Write a function that appends a band to an image.
  var appendBands = function(image, previous) {
    return ee.Image(previous).addBands(image);
  };
  return ee.Image(collection.iterate(appendBands, first));
};

var yrs = ['1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007'
          ,'2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019'];


var stacked = stackCollection(NBRcollection).rename(yrs).addBands(greater10.int16());
var NBRstack = stacked.unmask();

print(NBRstack);

// Overlay the points on the imagery to get training.
var NBRpoints = NBRstack.sampleRegions({
  collection: table,
  properties: ['ID'],
  scale: 30,
  tileScale: 8
});

Export.table.toDrive({
  collection: NBRpoints,
  folder: "worldfire3",
  fileNamePrefix: ("nbr_extract_Australia2")
});

Export.image.toDrive({
  image: NBRstack,
  description: 'nbr_sample_Australia',
  folder: 'worldfire3',
  scale: 30,
  fileFormat: 'GeoTIFF',
  //crs: 'EPSG:3577',
  region: geometry2,
  maxPixels: 1e10
});


