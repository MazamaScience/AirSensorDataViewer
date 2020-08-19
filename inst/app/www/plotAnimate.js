/*
Collapse button click event handler
NOTE: Loads on initialization of App
*/
shinyjs.init = function() {
  $('#collapse_btn').click(function() {
  if ($('a span').hasClass('glyphicon-chevron-down')) { // is down
    $('#collapse_btn').html('<span class="glyphicon glyphicon-chevron-up"></span> Show');
    $('#collapse_btn').toggleClass();
    d3.select('#overview_ui_1-timeseriesMap')
      .select('.leaflet-bottom .leaflet-control')
      .transition()
      .duration(75)
      .style('margin-bottom', '10px')
    
  } else { // is up
    $('#collapse_btn').html('<span class="glyphicon glyphicon-chevron-down"></span> Hide');
    $('#collapse_btn').toggleClass();
    d3.select('#overview_ui_1-timeseriesMap')
      .select('.leaflet-bottom .leaflet-control')
      .transition()
      .duration(75)
      .style('margin-bottom', '25vh')
  }
})};
