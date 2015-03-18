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
    x.data = toArray(JSON.parse(x.data));

    x.columns = JSON.parse(x.columns)

    if (x.cell) {
      x.cell = JSON.parse(x.cell)
    }

    if (x.customBorders) {
      x.customBorders = JSON.parse(x.customBorders)
    }

    if (x.groups) {
      x.groups = JSON.parse(x.groups)
    }

    for (var c in x.columns) {
      col = x.columns[c];
      if (col.renderer) {
        x.columns[c].renderer = col.renderer.parseFunction()
      }

      if (col.validator) {
        x.columns[c].validator = col.validator.parseFunction()
      }
    }

    x.afterLoadData = this.updateHeatmap;
    x.beforeChangeRender = this.updateHeatmap;

    this.afterChangeCallback(x);
    this.afterRowAndColChange(x);
    this.afterSelectCallback(x);

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

  afterSelectCallback: function(x) {

    x.afterSelectionEnd = function(r, c, r2, c2) {

      if (HTMLWidgets.shinyMode) {
        Shiny.onInputChange(this.rootElement.id + "_select", {
          data: JSON.stringify(this.getData()),
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

// http://stackoverflow.com/questions/1271516/executing-anonymous-functions-created-using-javascript-eval
if (typeof String.prototype.parseFunction != 'function') {
    String.prototype.parseFunction = function () {
        var funcReg = /function *\(([^()]*)\)[ \n\t]*{(.*)}/gmi;
        var match = funcReg.exec(this.replace(/\n/g, ' '));

        if(match) {
            return new Function(match[1].split(','), match[2]);
        }

        return null;
    };
}

