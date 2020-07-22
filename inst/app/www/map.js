$( document ).ready(function() {

  // mapid is the id of the div where the map will appear
let map = L
  .map('mapid')
  .setView([35.5, -96.5], 4); // center position + zoom

// Add a tile to the map = a background. Comes from OpenStreetmap
L.tileLayer(
  'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a>',
    maxZoom: 18,
  }).addTo(map);


// Add a svg layer to the map
L.svg().addTo(map);

// We pick up the SVG from the map object
let svgMap = d3.select("#mapid").select("svg").append("g");

let sensorID;

metaset.then(function(meta) {

  // Add a LatLng object from meta coords
  meta.forEach(m => {
    m.LatLng = new L.LatLng(m.latitude, m.longitude)
  })

  let onMarkerClick = function (d) {
    //console.log(d)
    sensorID = d.monitorID
    
    feature.style("stroke", "white")
    
    d3.select(d3.event.target)
      .raise()
      .transition()
      .duration(100)
      .style("stroke", "black")
      .attr("stroke-width", 4)
      .attr("fill-opacity", 1)
      .attr("stroke-opacity", 1)
  }

  let mouseIn = function (d) {
    d3.select(d3.event.target)
      .raise()
      .transition()
      .duration(100)
      .attr("r", 12);
  }

  let mouseOut = function (d) {
    d3.select(d3.event.target)
      .transition()
      .duration(150)
      .attr("r", 8);
  }

  // Select the svg layer and add the circles to it
  let feature = svgMap.selectAll("mycircle")
    .data(meta)
    .enter()
    .append("circle")
    .attr("cx", d => {
      map.latLngToLayerPoint(d.LatLng).x
    })
    .attr("cy", d => {
      map.latLngToLayerPoint(d.LatLng).y
    })
    .attr("r", 8)
    //.style("fill", "red")
    .attr("stroke", "white")
    .attr("stroke-width", 2)
    .attr("fill-opacity", 0.75)
    .attr("stoke-opacity", 0.75)
    .on("mouseover", mouseIn)
    .on("mouseout", mouseOut)
    .attr("pointer-events", "visible")
    .on("click", onMarkerClick);

  // Function that update circle position if something change
  function updateMap() {
    //g.selectAll("circle")
    feature
      .attr("cx", function (d) {
        return map.latLngToLayerPoint(d.LatLng).x
      })
      .attr("cy", function (d) {
        return map.latLngToLayerPoint(d.LatLng).y
      })
  }

  function updateColor() {
    feature
      .transition()
      .duration(75)
      .style("fill", (d, i) => { return focusColor[i].color })
  }

  // If the user change the map (zoom or drag), update circle position:
  map.on("moveend", updateMap)
  svgPlot.on("mousemove", updateColor)
  updateMap();

});

});

