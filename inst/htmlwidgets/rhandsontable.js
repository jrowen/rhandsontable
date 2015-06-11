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

    menu_items = {};

    if (x.allowRowEdit) {
      menu_items["row_above"] = {};
      menu_items["row_below"] = {};
      menu_items["hsep1"] = "---------";
    }
    if (x.allowColEdit) {
      menu_items["col_left"] = {};
      menu_items["col_right"] = {};
      menu_items["hsep2"] = "---------";
    }
    if (x.allowRowEdit) {
      menu_items["remove_row"] = {};
    }
    if (x.allowColEdit) {
      menu_items["remove_col"] = {};
    }
    if (x.allowRowEdit || x.allowColEdit) {
      menu_items["hsep3"] = "---------";
    }

    menu_items["undo"] = {};
    menu_items["redo"] = {};
    menu_items["hsep4"] = "---------";
    menu_items["make_read_only"] = {};
    menu_items["alignment"] = {};

    if (x.customBorders) {
      menu_items["borders"] = {};
    }

    if (x.comments) {
      menu_items["commentsAddEdit"] = {};
      menu_items["commentsRemove"] = {};
    }

    if (x.exportToCsv && !HTMLWidgets.shinyMode) {
      menu_items["hsep5"] = "---------";
      menu_items["csv"] = {"name": "Export to csv"};
    }

    x.contextMenu = {
      callback: function (key, options) {
        if (key === 'csv') {
          csvDownload(instance.hot, "filename.csv");
        }
      },
      items: menu_items
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

// https://stackoverflow.com/questions/22477612/converting-array-of-objects-into-array-of-arrays
function toArray(input) {
  var result = input.map(function(obj) {
    return Object.keys(obj).map(function(key) {
      return obj[key];
    });
  });
  return result;
}

// csv logic adapted from https://github.com/juantascon/jquery-handsontable-csv
function csvString(instance) {

  var headers = instance.getColHeader();

  var csv = headers.join(",") + "\n";

  for (var i = 0; i < instance.countRows(); i++) {
      var row = [];
      for (var h in headers) {
          var prop = instance.colToProp(h)
          var value = instance.getDataAtRowProp(i, prop)
          row.push(value)
      }

      csv += row.join(",")
      csv += "\n";
  }

  return csv;
}

function csvDownload(instance, filename) {

  var csv = csvString(instance)

  var link = document.createElement("a");
  link.setAttribute("href", "data:text/plain;charset=utf-8," + encodeURIComponent(csv));
  link.setAttribute("download", filename);

  document.body.appendChild(link)
  link.click();
  document.body.removeChild(link)
}
