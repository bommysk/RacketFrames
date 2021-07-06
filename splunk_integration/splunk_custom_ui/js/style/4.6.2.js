// Translations for en_US
i18n_register({ "catalog": {}, "plural": function(n) { return n == 1 ? 0 : 1; } });

require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/searchmanager',
    '../app/search/js/style/lib/latest',
    'splunkjs/mvc/simplexml/ready!'

], function(_, $, mvc, TableView, SearchManager, lib) {

    var CustomCellRenderer = TableView.BaseCellRenderer.extend({
        row_index: -1,
        row_count: 0,
        rows: {},
        style_lookup: "",
        read_style_lookup: function() {
            // get the value of the token
            this.reading_style_lookup = true;

            var token_value = this.tokens.get("style_file");

            if (token_value) {
                this.style_lookup = eval(token_value);

                var search = new SearchManager({
                    cache: false,
                    search: "| inputlookup " + this.style_lookup
                });

                var searchResults = search.data('results', {
                    output_mode: 'json',
                    count: 0
                });

                var that = this;

                searchResults.on("data", function() {
                    if (searchResults.hasData()) {
                        var styleResultArray = searchResults.data()["results"];

                        var date = new Date();
                        date.setTime(date.getTime() + (60 * 1000));
                        $.cookie(that.style_lookup, JSON.stringify(styleResultArray), { expires: date }); // expires after 1 minute                                                                    
                        // Create the event, need to pass data as an object, arrays get truncated to just first value
                        $(document).trigger("style_lookup_ready", { "data": styleResultArray });
                    }
                });
            }
        },
        canRender: function(cell) {
            return true;
        },
        // monitor the current row and column index
        setup: function($container, cell) {
            if (cell.index == 0) {
                this.row_index++;
                this.row_count++;
            }

            var results = this.tableView.resultsModel._data.rows;
            // build up a data structure to be available in the Dashboard code
            var row = {};
            for (var i = 0; i < this.tableView.resultsModel._data.fields.length; i++) {
                var field = this.tableView.resultsModel._data.fields[i].name;
                var value = results[this.row_index][i];
                row[field] = value;
            }

            this.rows[this.row_index] = row;

            // Cache Token Name Matching
            if (! this.hasOwnProperty("isRegexToken") )  {
               this.isRegexToken = {};
            }

            for (var token in this.tokens.attributes) {
                if (! (token in this.isRegexToken)) {
                  var isRegexToken = /^style\.\/(.*)\/$/
                  var coreRegex = isRegexToken.exec(token);
                  if (coreRegex) {
                    this.isRegexToken[token] = coreRegex;
                  }
                  else {
                    this.isRegexToken[token] = null;
                  }
              }
          }


        },
        teardown: function($container, cell) {
            if (cell.index == 0) {
                this.row_count--;
            }
            if (this.row_count == 0) {
                this.row_index = -1;
            }

            $(document).trigger("mailer_teardown");

        },
        render: function($td, cell) {
            var value = cell.value;
            var field = cell.field;
            
            // support for NA
            if (cell && value && value.toString().toLowerCase() == "na" ) {
                $td.addClass("light");
            }

            // support for EX
            if (cell && value && value.toString().toLowerCase() == "ex" ) {
                $td.addClass("lt_blue light");
            }

            //read lookup only once
            if (!$.cookie(this.style_lookup)) {
                //no cookie
                if (!this.reading_style_lookup && this.row_count == 1) {
                    this.read_style_lookup();
                }

                if (this.row_count > 1 && this.reading_style_lookup) {
                    this.reading_style_lookup = false;
                }

                var that = this;

                // Add an event listener for when lookup data is ready
                $(document).on("style_lookup_ready", function(e, opts) {
                    lib.bg_lookup(cell, $td, opts.data, that.rows);
                });
            } else {
                var data = JSON.parse($.cookie(this.style_lookup));
                lib.bg_lookup(cell, $td, data, this.rows);
            }

            var row = this.rows[this.row_index];

            var token_values = [];
            // get the value of the token
            var token_value = this.tokens.get("style." + this.dashboard_id + "." + cell.field);
            if (token_value) {
                token_values.push(token_value);
            }
            token_value = this.tokens.get("style." + cell.field);
            if (token_value) {
                token_values.push(token_value);
            }
            for (var token in this.tokens.attributes) {
                var coreRegex = this.isRegexToken[token];
                if (coreRegex) {
                    var myRegex = new RegExp(coreRegex[1]);
                    if (myRegex.exec(cell.field)) {
                        token_values.push(this.tokens.get(token));
                    }
                }
            }

            for (var token_value of token_values) {
                try {
                    // execute the eval to determine the classes names
                    var style_value = eval(token_value);
                    // apply CSS classes onto the cell
                    var arr = style_value.split(",");
                    arr.forEach(function(class_name) {
                        if (lib.isColor(class_name)) {
                            $td.css("background-color", class_name);
                        } else {
                            $td.addClass(class_name);
                        }
                    });
                } catch (err) {
                    console.error("Failed evaluating style for field " + cell.field + ",Error:", err);
                }
            }
            if ($.isArray(cell.value)) {
                $td.html(cell.value.map(_.escape).join('<br>'));
            } else {
                $td.text(cell.value);
            }
        }
    });
    $.each(mvc.Components.getInstances(), function(i, view) {
        if (typeof view.getVisualization != "undefined") {
            // Apply only on Data Tables
            view.getVisualization(function(subview) {
                // Obtain a reference to the search Manager of the table
                var searchManager = mvc.Components.getInstance(view.manager.id);
                var searchResults = searchManager.data('results', {
                    output_mode: 'json_rows',
                    count: 0
                });
                // Reigster a callback for when the data is being loaded
                searchResults.on("data", function() {
                    if (searchResults.hasData()) {
                        // All of the data have been loaded
                        // Obtain a reference to the tokens service
                        var tokens = mvc.Components.get("default");
                        // Create a new instance of the new custom cell renderer
                        var customCellRenderer = new CustomCellRenderer();
                        customCellRenderer.dashboard_id = view.id;
                        customCellRenderer.tokens = tokens;
                        customCellRenderer.tableView = subview;
                        // Assigning all of the style functions extensions
                        Object.assign(customCellRenderer, lib);
                        // Adding the Cell Renderer to the table
                        subview.addCellRenderer(customCellRenderer);
                    }
                });
            });
        }
    });
});
