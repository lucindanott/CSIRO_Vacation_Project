###############################################################################
####################### ANIMATED SCATTER PLOT FUNCTION ######################## 
###############################################################################

# This function is used across both Groups 
# This function adds the animation for the plots in 
# It is the standard 3D Scatter Plot produced 

# Inputs: 
#       d       - data structure/vector 
#       x       - x value
#       y       - y value
#       z       - z value 
#       cols    - group classification 
#       leg     - legend colouring (relating to R shiny app k or legend 2)
#       xax     - x label for scene in plotly 
#       yax     - y label for scene in plotly 
#       zax     - z label for scene in plotly 

# Outputs:
#       Structure for Plotly output WITH ANIMATIONS 

AnimatedScatterPlotFunction <- function(d,xdat,ydat,zdat,cols,leg,xax,yax,zax){
  produced.plotly <- plot_ly(
    data = d, 
    x = ~xdat, 
    y = ~ydat,
    z = ~zdat, 
    type = "scatter3d", 
    mode = "markers",
    sizemode = "Diameter", 
    color = ~cols,
    colors = c("firebrick","darkorange", "gold", "forestgreen")
  ) %>% 
    layout(
      legend = leg, 
      scene = list(xaxis = xax,
                   yaxis = yax,
                   zaxis = zax, 
                   camera = list(
                     eye = list(
                       x = 1.25,
                       y = 1.25,
                       z = 1.25
                     ),
                     center = list(x = 0,
                                   y = 0,
                                   z = 0)
                   ))) %>%
    onRender("
      function(el, x){
  var id = el.getAttribute('id');
  var gd = document.getElementById(id);
  Plotly.plot(id).then(attach);
  function attach() {
    var cnt = 0;

    function run() {
      rotate('scene', Math.PI / 180);
      requestAnimationFrame(run);
    }
    run();

    function rotate(id, angle) {
      var eye0 = gd.layout[id].camera.eye
      var rtz = xyz2rtz(eye0);
      rtz.t += angle;

      var eye1 = rtz2xyz(rtz);
      Plotly.relayout(gd, id + '.camera.eye', eye1)
    }

    function xyz2rtz(xyz) {
      return {
        r: Math.sqrt(xyz.x * xyz.x + xyz.y * xyz.y),
        t: Math.atan2(xyz.y, xyz.x),
        z: xyz.z
      };
    }

    function rtz2xyz(rtz) {
      return {
        x: rtz.r * Math.cos(rtz.t),
        y: rtz.r * Math.sin(rtz.t),
        z: rtz.z
      };
    }
  };
}
    ")
  produced.plotly
}