HTMLWidgets.widget({

  name: 'rHandsontable',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      // TODO: add instance fields as required
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

    this.afterChangeCallback(x);
    this.afterRowAndColChange(x);

    if (instance.rHandsontable) { // update existing instance

      instance.rHandsontable.updateSettings(x);

    } else {  // create new instance

      instance.rHandsontable = new Handsontable(el, x);

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

      };
    }
  }

});
