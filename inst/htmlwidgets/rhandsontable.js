HTMLWidgets.widget({

  name: 'rhandsontable',

  type: 'output',

  initialize: function(el, width, height) {

    var hot = new Handsontable(el);

    Handsontable.renderers.registerRenderer('heatmapRenderer', this.heatmapRenderer);

    return {
      hot: hot
    }

  },

  renderValue: function(el, x, instance) {

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
    };

    // convert json to array
    x.data = toArray(JSON.parse(x.data));

    x.columns = JSON.parse(x.columns)

    if (x.isheatmap) {
      x.afterLoadData = this.updateHeatmap
      x.beforeChangeRender = this.updateHeatmap
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

  },

  afterChangeCallback: function(x) {

    // check for an existing
    var prevAfterChange = x["afterChange"];

    x.afterChange = function(changes, source) {

      // call existing
      if (prevAfterChange)
        prevAfterChange(changes, source);

      // not implemented
    };
  },

  afterRowAndColChange: function(x) {

    funcs = ["afterCreateRow", "afterRemoveRow",
             "afterCreateCol", "afterRemoveCol"];

    for (var i = 0; i < funcs.length; i++) {
      // check for an existing
      var prev = x[funcs[i]];

      x[funcs[i]] = function(ind, ct) {

        // call existing
        if (prev)
          prev(ind, ct);

        // not implemented
      };
    }
  },

  // see http://handsontable.com/demo/heatmaps.html
  //heatmap: [],
  //heatmapScale: [],
  updateHeatmap: function(change, source) {

    if (change) {
      this.heatmap[change[0][1]] = generateHeatmapData.call(this, change[0][1]);
    } else {
      this.heatmap = [];

      for(var i = 0, colCount = this.countCols(); i < colCount ; i++) {
        this.heatmap[i] = generateHeatmapData.call(this, i);
      }
    }
  },

  heatmapRenderer: function(instance, td, row, col, prop, value, cellProperties) {

    Handsontable.renderers.TextRenderer.apply(this, arguments);
    heatmapScale  = chroma.scale(['#17F556', '#ED6D47']);

    if (instance.heatmap[col]) {
      td.style.backgroundColor = heatmapScale(point(instance.heatmap[col].min, instance.heatmap[col].max, parseInt(value, 10))).hex();
      td.style.textAlign = 'right';
      td.style.fontWeight = 'bold';
    }
  },

  condformatRenderer: function(instance, td, row, col, prop, value, cellProperties) {

    // not implemented

    //Handsontable.renderers.TextRenderer.apply(this, arguments);
    //instance.hot.condformat.vals
    //instance.hot.condformat.styles

  }

});

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
