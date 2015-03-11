HTMLWidgets.widget({

  name: 'rhandsontable',

  type: 'output',

  initialize: function(el, width, height) {

    //Handsontable.renderers.registerRenderer('heatmapRenderer', this.heatmapRenderer);

    return {

    }

  },

  renderValue: function(el, x, instance) {

    // used to pass color to heatmap
    hotParams[el.id] = x;

    // convert json to array
    x.data = toArray(JSON.parse(x.data));

    x.columns = JSON.parse(x.columns)

    if (x.customBorders) {
      x.customBorders = JSON.parse(x.customBorders)
    }

    if (x.groups) {
      x.groups = JSON.parse(x.groups)
    }

    if (x.heatmapCols) {
      x.afterLoadData = this.updateHeatmap
      x.beforeChangeRender = this.updateHeatmap

      for (var i = 0; i < x.heatmapCols.length; i++) {
        x.columns[x.heatmapCols[i]].renderer = x.heatmapRenderer
      }
    }

    this.afterChangeCallback(x);
    this.afterRowAndColChange(x);

    if (instance.hot) { // update existing instance

      instance.hot.updateSettings(x);

    } else {  // create new instance

      instance.hot = new Handsontable(el, x);

    }

  },

  resize: function(el, width, height, instance) {

    instance.hot.updateSettings({ width: width,
                                  height: height
    });
  },

  afterChangeCallback: function(x) {

    x.afterChange = function(changes, source) {

      if (HTMLWidgets.shinyMode && changes) {
        Shiny.onInputChange(this.rootElement.id, {
          data: JSON.stringify(this.getData()),
          changes: { event: "afterChange", changes: changes },
          params: hotParams[this.rootElement.id]
        });
      }

    };
  },

  afterRowAndColChange: function(x) {

    x.afterCreateRow = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: JSON.stringify(this.getData()),
          changes: { event: "afterCreateRow", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterRemoveRow = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: JSON.stringify(this.getData()),
          changes: { event: "afterRemoveRow", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterCreateCol = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: JSON.stringify(this.getData()),
          changes: { event: "afterCreateCol", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterRemoveCol = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: JSON.stringify(this.getData()),
          changes: { event: "afterRemoveCol", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

  },

  // see http://handsontable.com/demo/heatmaps.html
  updateHeatmap: function(change, source) {

    if (change) {
      this.heatmap[change[0][1]] = generateHeatmapData.call(this, change[0][1]);
    } else {
      this.heatmap = [];

      for(var i = 0, colCount = this.countCols(); i < colCount ; i++) {
        this.heatmap[i] = generateHeatmapData.call(this, i);
      }
    }
  }

});

var hotParams = [];

function heatmapRenderer(instance, td, row, col, prop, value, cellProperties) {

  Handsontable.renderers.TextRenderer.apply(this, arguments);
  heatmapScale  = chroma.scale(hotParams[instance.rootElement.id].color_scale);

  if (instance.heatmap[col]) {
    td.style.backgroundColor = heatmapScale(point(instance.heatmap[col].min, instance.heatmap[col].max, parseInt(value, 10))).hex();
    //td.style.textAlign = 'right';
    //td.style.fontWeight = 'bold';
  }
}

function point(min, max, value) {
  return (value - min) / (max - min);
}

function generateHeatmapData(colId) {

  var values = this.getDataAtCol(colId);

  return {
    min: Math.min.apply(null, values),
    max: Math.max.apply(null, values)
  };
}

// http://stackoverflow.com/questions/11922383/access-process-nested-objects-arrays-or-json
function toArray(obj) {
  var result = [];
  for (var prop in obj) {
      var value = obj[prop];
      if (typeof value === 'object') {
          result.push(toArray(value)); // <- recursive call
      }
      else {
          result.push(value);
      }
  }
  return result;
}
