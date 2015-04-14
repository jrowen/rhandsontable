HTMLWidgets.widget({

  name: 'rhandsontable',

  type: 'output',

  initialize: function(el, width, height) {

    return {

    }

  },

  renderValue: function(el, x, instance) {

    // used to pass color to heatmap
    hotParams[el.id] = x;

    // convert json to array
    x.data = toArray(x.data);

    x.afterLoadData = this.updateHeatmap;
    x.beforeChangeRender = this.updateHeatmap;

    this.afterChangeCallback(x);
    this.afterRowAndColChange(x);

    if (x.selectCallback) {
      this.afterSelectCallback(x);
    }

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
          data: this.getData(),
          changes: { event: "afterChange", changes: changes },
          params: hotParams[this.rootElement.id]
        });
      }

    };
  },

  afterSelectCallback: function(x) {

    x.afterSelectionEnd = function(r, c, r2, c2) {

      if (HTMLWidgets.shinyMode) {
        Shiny.onInputChange(this.rootElement.id + "_select", {
          data: this.getData(),
          select: { r: r, c: c, r2: r2, c2: c2},
          params: hotParams[this.rootElement.id]
        });
      }

    };
  },

  afterRowAndColChange: function(x) {

    x.afterCreateRow = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: this.getData(),
          changes: { event: "afterCreateRow", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterRemoveRow = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: this.getData(),
          changes: { event: "afterRemoveRow", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterCreateCol = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: this.getData(),
          changes: { event: "afterCreateCol", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

    x.afterRemoveCol = function(ind, ct) {

      if (HTMLWidgets.shinyMode)
        Shiny.onInputChange(this.rootElement.id, {
          data: this.getData(),
          changes: { event: "afterRemoveCol", ind: ind, ct: ct },
          params: hotParams[this.rootElement.id]
        });
    };

  },

  // see http://handsontable.com/demo/heatmaps.html
  updateHeatmap: function(change, source) {

    function generateHeatmapData(colId) {

      var values = this.getDataAtCol(colId);

      return {
        min: Math.min.apply(null, values),
        max: Math.max.apply(null, values)
      };
    }

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
