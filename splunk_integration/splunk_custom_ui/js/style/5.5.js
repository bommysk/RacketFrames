// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});


////////////////////////////////////////////////////////////////////
//                                                                //
// Style JS implements our core custom JS functionality in Splunk //
//                                                                //
////////////////////////////////////////////////////////////////////

require.config({
  waitSeconds: 0,
  paths: {
    'lib': '../app/search/js/style/lib/latest',
    'nested_headers': '../app/search/js/double_headers/latest',
    'splunk_datatables': '../app/search/js/datatables/latest'
  }
});

require([
  'underscore',
  'jquery',
  'splunkjs/mvc/utils',
  'splunkjs/mvc/tokenutils',
  'splunkjs/mvc',
  'splunkjs/mvc/tableview',
  'splunkjs/mvc/searchmanager',
  'splunkjs/mvc/savedsearchmanager',
  'lib',
  'nested_headers',
  'splunk_datatables',
  'splunkjs/ready!',
  'splunkjs/mvc/simplexml/ready!'
], function (_, $, splunk_utils, token_utils, mvc, TableView, SearchManager, SavedSearchManager, lib, nested_headers, splunk_datatables) {
  // Create a basic custom row renderer
  var RowRenderer = TableView.BaseRowExpansionRenderer.extend({
      canRender: function(rowData) {
        console.log("RowData: ", rowData);

        return true;
      },
      render: function($container, rowData) {
        // Display some of the rowData in the expanded row        
        $container.append(eval(this.row_expansion_body));
      }
  });
    
  var CustomCellRenderer = TableView.BaseCellRenderer.extend({
    row_index: -1,
    row_count: 0,
    rows: {},
    initialize: function () {
      // cache token name matching
      if (!this.hasOwnProperty("isStyleRegexToken")) {
        this.isStyleRegexToken = {};
      }

      if (!this.hasOwnProperty("isImageRegexToken")) {
        this.isImageRegexToken = {};
      }

      if (!this.hasOwnProperty("isJSEvalColRegexToken")) {
        this.isJSEvalColRegexToken = {};
      }

      for (let token in default_token_model.attributes) {
        if (!(token in this.isStyleRegexToken)) {
          let isStyleRegexToken = /^style\.\/(.*)\/$/
          let coreRegex = isStyleRegexToken.exec(token);
          if (coreRegex) {
            this.isStyleRegexToken[token] = coreRegex;
          } else {
            this.isStyleRegexToken[token] = null;
          }
        }

        if (!(token in this.isImageRegexToken)) {
          let isImageRegexToken = /^image\.\/(.*)\/$/
          let coreRegex = isImageRegexToken.exec(token);
          if (coreRegex) {
            this.isImageRegexToken[token] = coreRegex;
          } else {
            this.isImageRegexToken[token] = null;
          }
        }

        if (!(token in this.isJSEvalColRegexToken)) {
          let isJSEvalColRegexToken = /^(?:config\.)?js_eval_col\.\/(.*)\/$/
          let coreRegex = isJSEvalColRegexToken.exec(token);
          if (coreRegex) {
            this.isJSEvalColRegexToken[token] = coreRegex;
          } else {
            this.isJSEvalColRegexToken[token] = null;
          }
        }
      }
    },
    canRender: function (cell) {
      if (!cell) {
        return false;
      }

      if (cell.dataOverlay && cell.columnType) {
        if (cell.dataOverlay === "heatmap" && cell.columnType === "number") {
          return false;
        }

        if (cell.dataOverlay === "none" && cell.columnType === "sparkline") {
          return false;
        }
      }

      return true;
    },
    // monitor the current row and column index
    setup: function ($container, cell) {
      if (cell.index == 0) {
        this.row_index++;
        this.row_count++;
      }
    },
    teardown: function ($container, cell) {
      if (cell.index == 0) {
        this.row_count--;
      }
      if (this.row_count == 0) {
        this.row_index = -1;
      }

      $(document).trigger("mailer_teardown");

    },
    render: function ($td, cell) {
      if (this.subview_id) {
        custom_cell_renderers_success[this.subview_id] = true;
      }

      var value = cell.value;
      var field = cell.field;
      var html_value = value;

      /************************/
      /* Image renderer */
      /************************/
      let image_token_value = this.tokens.get('image.' + this.dashboard_id + '.' + cell.field);
      if (!image_token_value) {
        image_token_value = this.tokens.get('image.' + cell.field);

        if (!image_token_value) {
          for (let token in this.tokens.attributes) {            
            let coreRegex = this.isImageRegexToken[token];
            if (coreRegex) {
              let myRegex = new RegExp(coreRegex[1]);
              if (myRegex.exec(cell.field)) {
                image_token_value = this.tokens.get(token);
              }
            }
          }
        }
      }
      if (image_token_value) {
        try {
          // Retrieve the site_server from the tokens
          let site_server = this.tokens.get("site_server");
          // Compute the image url base on the cell's value
          let url = 'https://mxd.csg.apple.com/latest/view.php?file=' + cell.value + '&thumbnail';
          // Create the img element and add it to the table cell
          $td.html('<img style="cursor:pointer;max-width:250px" src="' + url + '"></img>');

          // Returning true means that we have updated the outer html/text of the cell
          return true;
        } catch (err) {
          console.log('Failed evaluating style for field ' + cell.field + ',Error:', err);
        }
      }

      /************************/
      /* Style renderer */
      /************************/
      // support for NA
      if (cell && value && value.toString().toLowerCase() == "na") {
        $td.addClass("light");
      }

      // support for EX
      if (cell && value && value.toString().toLowerCase() == "ex") {
        $td.addClass("lt_blue light");
      }

      // support for numeric columns
      if ( /^-?[0-9][0-9.,]*$/.test(value) ) {
	$td.addClass("numeric");
      }
	
      this.style_lookup_cookie = this.dashboard_name + "_style_lookup_cookie";

      // read lookup only once
      if (!$.cookie(this.style_lookup_cookie)) {
        var that = this;

        // add an event listener for when lookup data is ready
        $(document).on("style_lookup_ready", function (e, opts) {
          lib.bg_lookup(cell, $td, opts.data, that.rows);
        });
      } else {
        var data = JSON.parse($.cookie(this.style_lookup_cookie));
        lib.bg_lookup(cell, $td, data, this.rows);
      }

      var row = this.rows[this.row_index];      

      // Check for default style as literal "style.*"
      let default_token_value = this.tokens.get("style.*");
      if (default_token_value) {
        try {
          // execute the eval to determine the classes names
          let style_value = eval(default_token_value);
          // apply CSS classes onto the cell
          let arr = style_value.split(",");
          arr.forEach(function (class_name) {
            $td.addClass(class_name);
          });
        } catch (err) {
          console.error("Failed evaluating style for field " + cell.field + ",Error:", err);
        }
      }

      let style_token_values = [];
      // get the value of the token
      let style_token_value = this.tokens.get("style." + this.dashboard_id + "." + cell.field);
      if (style_token_value) {
        style_token_values.push(token_value);
      }
      style_token_value = this.tokens.get("style." + cell.field);
      if (style_token_value) {
        style_token_values.push(style_token_value);
      }
      for (let token in this.tokens.attributes) {
        let coreRegex = this.isStyleRegexToken[token];
        if (coreRegex) {
          let myRegex = new RegExp(coreRegex[1]);
          if (myRegex.exec(cell.field)) {
            style_token_values.push(this.tokens.get(token));
          }
        }
      }

      for (let style_token_value of style_token_values) {
        try {
          // execute the eval to determine the classes names

          let style_value = eval(style_token_value);
          // apply CSS classes onto the cell

          let arr = style_value.split(",");

          arr.forEach(function (class_name) {
            if (lib.isColor(class_name)) {
              $td.css("background-color", class_name);
            } else {
              $td.addClass(class_name);
            }
          });
        } catch (err) {
          console.error("Failed evaluating style for field " + cell.field + ". Error:", err);
        }
      }

      /************************/
      /* * Style from event * */
      /************************/
      // handle styles injected as events
      let style_check = `_${field}_style`;

      try {
        if (row && style_check in row) {
          let style = row[style_check];

          if (style && lib.isColor(style)) {
            $td.css("background-color", style);
          } else {
            $td.addClass(style);
          }
        }
      } catch (err) {
        console.error("Failed evaluating style for field " + cell.field + ". Error:", err);
      }

      var js_eval_col_token_values = [];
      // get the value of the token
      var js_eval_col_token_value = this.tokens.get("js_eval_col." + this.dashboard_id + "." + cell.field);

      if (!js_eval_col_token_value) {
        js_eval_col_token_value = this.tokens.get("config.js_eval_col." + this.dashboard_id + "." + cell.field);
      }  

      if (js_eval_col_token_value) {
        js_eval_col_token_values.push(token_value);
      }
      js_eval_col_token_value = this.tokens.get("js_eval_col." + cell.field);

      if (!js_eval_col_token_value) {
        js_eval_col_token_value = this.tokens.get("config.js_eval_col." + cell.field);
      }

      if (js_eval_col_token_value) {
        js_eval_col_token_values.push(js_eval_col_token_value);
      }
      for (var token in this.tokens.attributes) {
        var coreRegex = this.isJSEvalColRegexToken[token];
        if (coreRegex) {
          var myRegex = new RegExp(coreRegex[1]);
          if (myRegex.exec(cell.field)) {
            js_eval_col_token_values.push(this.tokens.get(token));
          }
        }
      }

      for (var js_eval_col_token_value of js_eval_col_token_values) {
        try {
          // execute the eval to determine the classes names
          value = eval(js_eval_col_token_value);
          html_value = value;
        } catch (err) {
          console.error("Failed evaluating style for field " + cell.field + ". Error:", err);
        }
      }

      /************************/
      /* * Tooltip renderer * */
      /************************/
      // tooltips injected as events get priority over csv lookup
      // to avoid duplicate tooltips also we want this to be the 
      // preferred approach
      let tooltip_applied = false;
      let tooltip_check = `_${field}_tooltip`;
      let tooltip_placement = this.tooltip_placement;
      let tooltip_truncation_limit = default_token_model.get("tooltips.truncation_limit");

      if (tooltip_truncation_limit) {
        tooltip_truncation_limit = eval(tooltip_truncation_limit);
      } else {
        // default tooltip truncation happens at 48 characters
        tooltip_truncation_limit = 48;
      }

      // render multivalue cells correctly
      let multi_value_html = "";
      if ($.isArray(html_value)) {

        html_value.forEach(function (element, idx) {
            let message = element;
            let tip = element;

            if (tip && tip.length && (!tip.includes('<') && !tip.includes('>')) && tip.length > tooltip_truncation_limit && tooltip_placement) {
              message = message.substring(0, tooltip_truncation_limit) + "...";
              let tooltip_element = _.template('<a href="#" data-toggle="tooltip" data-placement="<%- tooltip_placement%>" title="<%- tip%>"><%- message%></a>', {
                tip: tip,
                message: message,
                tooltip_placement: tooltip_placement
              });
              multi_value_html += `<div tabindex="${idx}" class="multivalue-subcell" data-mv-index="${idx}">${tooltip_element}</div>`;
            } else {
	      multi_value_html += `<div tabindex="${idx}" class="multivalue-subcell" data-mv-index="${idx}">${element}</div>`;
            }
        });

        $td.html(multi_value_html);
      }
      try {
        if (row && tooltip_check in row) {
          let original_field_value = row[field];
          let tip = row[tooltip_check];

          if (tip && tooltip_placement) {
	    var full_value = $.isArray(original_field_value) ? multi_value_html : original_field_value;
            html_value = _.template('<span data-toggle="tooltip" data-placement="<%- tooltip_placement%>" data-original-title="<%- tip%>"><%- value%></span>', {
              tip: tip,
              value: full_value,
              tooltip_placement: tooltip_placement
            });

            tooltip_applied = true;
          }
        } else {
          this.tooltips_cookie = this.dashboard_name + "_tooltips_cookie";

          // read lookup only once
          if (!$.cookie(this.tooltips_cookie)) {
            var that = this;

            $(document).on("fetched_tooltips", function (e, opts) {
              let tooltip_data = opts.data;

              if (tooltip_data) {
                for (const tooltip of tooltip_data) {
                  if (tooltip.Field && tooltip.Field === field) {
                    let tip = lib.tooltip_lookup(cell, tooltip_data, that.rows);

                    if (tip && tooltip_placement) {
		      var full_value = $.isArray(value) ? multi_value_html : value;
                      html_value = 
_.template('<span data-toggle="tooltip" data-placement="<%- tooltip_placement%>" data-original-title="<%- tip%>"><%- value%></span>', {
                        tip: tip,
                        value: full_value,
                        tooltip_placement: tooltip_placement
                      });

                      tooltip_applied = true;
                    }

                    break;
                  }
                }
              }
            });
          } else {
            let tooltip_data = JSON.parse($.cookie(this.tooltips_cookie));

            for (const tooltip of tooltip_data) {
              if (tooltip.Field && tooltip.Field === field) {

                let tip = lib.tooltip_lookup(cell, tooltip_data, this.rows);

                if (tip && tooltip_placement) {
console.log("HERE 3");
		  var full_value = $.isArray(value) ? multi_value_html : value;
                  html_value = `<span data-toggle="tooltip" data-placement="${tooltip_placement}" data-original-title="${tip}">${full_value}</span>`;

                  tooltip_applied = true;
                }

                break;
              }
            }
          }
	}
      } catch (err) {
        console.error("Failed evaluating tooltip for field " + cell.field + ". Error:", err);
      }

      if (this.has_tooltips) {
	if (tooltip_applied) {
	  $td.html(html_value);
	} else {
	  if (! $.isArray(value)) {
	    let message = value;
	    let tip = value;

	    if (tip && tip.length && (!tip.includes('<') && !tip.includes('>')) && tip.length > tooltip_truncation_limit && tooltip_placement) {
	      message = message.substring(0, tooltip_truncation_limit) + "..."
		$td.html(_.template('<a href="#" data-toggle="tooltip" data-placement="<%- tooltip_placement%>" title="<%- tip%>"><%- message%></a>', {
			   tip: tip,
					message: message,
					tooltip_placement: tooltip_placement
					}));
	    } else {
	      // this allows us to use html and xml tags as the cell value
	      // i.e. <asserts>
	      $td.text(value);
	    }
	  }
	}

      } else {
	if (! $.isArray(value)) {
	  // this allows us to use html and xml tags as the cell value
	  // i.e. <asserts>
	  $td.text(value);
	}
      }

      // This line wires up the Bootstrap tooltip to the cell markup
      // This won't do anything if tooltips weren't created above
      $td.find('[data-toggle="tooltip"]').tooltip({
	html: true,
	container: 'body'
      });

      // Returning false means that we haven't updated the outer html/text of the cell, e.g. adding a callback
      return true;
    }
  });

  function set_token(name, value) {
    default_token_model.set(name, value);
    submitted_token_model.set(name, value);
  }

  function unset_token(name) {
    default_token_model.unset(name);
    submitted_token_model.unset(name);
  }

  function check_scheduled_report_exists(view, idx, display_counter) {
    return new Promise(function (resolve, reject) {
      let go_live = default_token_model.get("go_live");

      if (go_live && eval(go_live)) {
        console.log(`resolving tbl${display_counter + 1} not-cached`);
        set_token(`tbl${display_counter + 1}_go_live`, true);
        resolve(idx + 1);
      }

      let saved_search = `${dashboard_url_id}_scheduled_report_${display_counter + 1}`;

      console.log(`Saved Search: ${saved_search}`);

      try {
        let savedSearchManager = new SavedSearchManager({
          id: saved_search,
          searchname: saved_search,
          app: app,
          cached: true
        });

        savedSearchManager.on("search:error", function (state, job) {
          console.log(`resolving tbl${display_counter + 1} not-cached`);
          set_token(`tbl${display_counter + 1}_go_live`, true);
          resolve(idx + 1);
        });

        savedSearchManager.on("search:fail", function (state, job) {
          console.log(`resolving tbl${display_counter + 1} not-cached`);
          set_token(`tbl${display_counter + 1}_go_live`, true);
          resolve(idx + 1);
        });

        savedSearchManager.on("search:done", function (state, job) {
          if (state.content.resultCount > 0) {
            console.log(`delayed resolving tbl${display_counter + 1} cached on data`);
          } else {
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);
            resolve(idx + 1);
          }
        });

        let savedSearchResults = savedSearchManager.data('results', {
          output_mode: 'json',
          count: 0
        });

        savedSearchResults.on("data", function () {
          if (savedSearchResults.hasData()) {
            console.log(`resolving tbl${display_counter + 1} cached`);
            set_token(`tbl${display_counter + 1}_cached`, true);
            resolve(idx);
          }
        });
      } catch (err) {
        console.log(err);
        console.log(`resolving tbl${display_counter + 1} not-cached`);
        set_token(`tbl${display_counter + 1}_go_live`, true);
        resolve(idx + 1);
      }
    });
  }

  function check_scheduled_report_csv_exists(view, idx, display_counter) {
    return new Promise(function (resolve, reject) {
      let go_live = default_token_model.get("go_live");

      if (go_live && eval(go_live)) {
        console.log(`resolving tbl${display_counter + 1} not-cached`);
        set_token(`tbl${display_counter + 1}_go_live`, true);
        resolve(idx + 1);
      }

      let scheduled_report_csv = `${dashboard_url_id}_scheduled_report_${display_counter + 1}.csv`;

      console.log(`Schedule Report CSV: ${scheduled_report_csv}`);

      try {
        let searchManager = new SearchManager({
          cache: true,
          search: "| inputlookup " + scheduled_report_csv
        });

        searchManager.on("search:error", function (state, job) {
          $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, {
            "data": false
          });
          console.log(`resolving tbl${display_counter + 1} not-cached`);
          set_token(`tbl${display_counter + 1}_go_live`, true);
          resolve(idx + 1);
        });

        searchManager.on("search:fail", function (state, job) {
          $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, {
            "data": false
          });
          console.log(`resolving tbl${display_counter + 1} not-cached`);
          set_token(`tbl${display_counter + 1}_go_live`, true);
          resolve(idx + 1);
        });

        searchManager.on("search:done", function (state, job) {
          if (state.content.resultCount > 0) {
            console.log(`resolving tbl${display_counter + 1} cached on data`);
            let date = new Date();
            date.setTime(date.getTime() + (60 * 1000));
            $.cookie(`scheduled_report_csv_ready_cookie_${display_counter + 1}`, true, {
              expires: date
            }); // expires after 1 minute
            set_token(`tbl${display_counter + 1}_cached`, true);
            resolve(idx);
          } else {
            $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, {
              "data": false
            });
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);
            resolve(idx + 1);
          }
        });

        let searchResults = searchManager.data('results', {
          output_mode: 'json',
          count: 0
        });
      } catch (err) {
        console.log(err);
        console.log(`resolving tbl${display_counter + 1} not-cached`);
        set_token(`tbl${display_counter + 1}_go_live`, true);
        resolve(idx + 1);
      }
    });
  }

  function read_style_lookup() {
    // get the value of the style+_file token
    let style_file = default_token_model.get("style_file");

    try {
      if (style_file) {
        let style_lookup = eval(style_file);

        let searchManager = new SearchManager({
          cache: true,
          search: "| inputlookup " + style_lookup
        });

        searchManager.on("search:error", function (state, job) {
          $(document).trigger("style_lookup_ready", {
            "data": false
          });
        });

        searchManager.on("search:fail", function (state, job) {
          $(document).trigger("style_lookup_ready", {
            "data": false
          });
        });

        searchManager.on("search:done", function (state, job) {
          if (state.content.resultCount === 0) {
            $(document).trigger("style_lookup_ready", {
              "data": false
            });
          }
        });

        let searchResults = searchManager.data('results', {
          output_mode: 'json',
          count: 0
        });

        searchResults.on("data", function () {
          if (searchResults.hasData()) {
            let styleResultArray = searchResults.data()["results"];

            let date = new Date();
            date.setTime(date.getTime() + (60 * 1000));
            $.cookie(dashboard_name + "_style_lookup_cookie", JSON.stringify(styleResultArray), {
              expires: date
            }); // expires after 1 minute
            // Create the event, need to pass data as an object, arrays get truncated to just first value
            $(document).trigger("style_lookup_ready", {
              "data": styleResultArray
            });
          }
        });
      }
    } catch (err) {
      console.log(err);
      return false;
    }
  }

  function fetch_tooltips(table_id) {
    let tooltips_csv = `${dashboard_name}_${table_id}_tooltips.csv`;

    console.log(`Tooltips CSV: ${tooltips_csv}`);

    try {
      // read tooltips lookup
      let searchManager = new SearchManager({
        cache: true,
        search: "| inputlookup " + tooltips_csv
      });

      searchManager.on("search:error", function (state, job) {
        $(document).trigger("fetched_tooltips", {
          "data": false
        });
      });

      searchManager.on("search:fail", function (state, job) {
        $(document).trigger("fetched_tooltips", {
          "data": false
        });
      });

      searchManager.on("search:done", function (state, job) {
        if (state.content.resultCount === 0) {
          $(document).trigger("fetched_tooltips", {
            "data": false
          });
        }
      });

      let searchResults = searchManager.data('results', {
        output_mode: 'json',
        count: 0
      });

      searchResults.on("data", function () {
        if (searchResults.hasData()) {
          let tooltips = searchResults.data()["results"];

          console.log("tooltips");
          console.log(tooltips);

          let date = new Date();
          date.setTime(date.getTime() + (1200 * 1000));
          $.cookie(dashboard_name + "_tooltips_cookie", JSON.stringify(tooltips), {
            expires: date
          }); // expires after 20 minute

          // Create the event, need to pass data as an object, arrays get truncated to just first value
          $(document).trigger("fetched_tooltips", {
            "data": tooltips
          });
        }
      });
    } catch (err) {
      console.log(err);
      return false;
    }
  }

  function run(all_view_instances) {
    let has_style_file = default_token_model.get("style_file");
    let has_tooltips = default_token_model.get("tooltips");
    let tooltip_placement = default_token_model.get('tooltip_placement');
    if (!tooltip_placement) {
      tooltip_placement = default_token_model.get('tooltips.placement');
    }    
    let all_view_instances_count = all_view_instances.length;
    let total_view_nested_headers_count = all_view_instances_count;
    let datatables_global_token_value = default_token_model.get("datatables");

    has_style_file = has_style_file && eval(has_style_file);

    // get tooltips of whole dashboard not individual tables
    if (tooltip_placement) {
      tooltip_placement = eval(tooltip_placement);
    } else {
      tooltip_placement = 'right';
    }

    has_tooltips = has_tooltips && eval(has_tooltips);

    let all_views = [];
    // Keep track of rendered views
    let rendered_views = {};
    let nested_headers_formatted = {};
    let deferred = [];
    let view_rows = {};
    let custom_cell_renderers = {};
    // Splunk tends to visit each view multiple times for some reason
    // tracking visited views to look out for duplicate loops
    let visited_views = [];
    let visited_views_checkboxes = [];
    let visited_subviews = [];

    console.log(`total view instances: ${all_view_instances_count}`);
    console.log(all_view_instances);

    $.each(all_view_instances, function (i, view) {
      // Apply only on Data Tables
      deferred.push(
        new Promise(function (resolve, reject) {
          view.getVisualization(function (subview) {
            console.log(`tbl${i + 1}`);

            unset_token(`tbl#${view.id}_display`);
            unset_token(`tbl${i + 1}_display`);

            // Handle saved search by id
            // let saved_search = `${dashboard_url_id}_scheduled_report_${display_counter + 1}`;

            // Obtain a reference to the search manager of the table
            var searchManager = mvc.Components.getInstance(subview.options.managerid);
            var searchResults = searchManager.data('results', {
              output_mode: 'json_rows',
              count: 0,
              cache: true
            });

            searchManager.on("search:error", function (state, job) {
              set_token(`tbl#${view.id}_display`, true);
              set_token(`tbl${i + 1}_display`, true);
              resolve(false);
            });

            searchManager.on("search:fail", function (state, job) {
              set_token(`tbl#${view.id}_display`, true);
              set_token(`tbl${i + 1}_display`, true);
              resolve(false);
            });

            searchManager.on("search:done", function (state, job) {
              if (state.content.resultCount === 0) {
                set_token(`tbl#${view.id}_display`, true);
                set_token(`tbl${i + 1}_display`, true)
                resolve(false);
              }
            });

            // Reigster a callback for when the data is being loaded
            searchResults.on("data", function () {
              if (searchResults.hasData()) {
                console.log("HAS DATA");
                var data = searchResults.data();
                var fields = data.fields;
                var data_rows = data.rows;
                var total_rows = data_rows.length;

                all_views.push({
                  subview,
                  i,
                  "id": view.id,
                  "fields": fields
                });

                var rows = {};
                var row = {};

                for (var row_idx = 0; row_idx < total_rows; row_idx++) {
                  for (var f = 0; f < fields.length; f++) {
                    var field = fields[f];
                    var value = data_rows[row_idx][f];
                    row[field] = value;
                  }

                  rows[row_idx] = row;

                  row = {};
                }

                view_rows[i] = rows;

                try {
                  // eval any tokens with JS
                  let re = /^(config\.)?js_eval\.(.*)$/;
                  for (const [token, value] of Object.entries(default_token_model.attributes)) {
                    let match = re.exec(token);
                    if (match) {
                      console.log(`match: ${match}, value: ${value}`);
                      console.log('eval: ' + eval(value));
                      set_token(match, eval(value));
                      unset_token(token);
                    }
                  }

                  // special handling for drilldowns
                  default_token_model.on("change:js_eval.click_link", function (new_token_name, value, options) {
                    if (!value) {
                      return;
                    }
                    // get right row set for multiple tables                         
                    console.log("js_eval.click_link---------------->" + value);
                    set_token('click_link', eval(value));
                    unset_token('js_eval.click_link');
                  });

                   // special handling for drilldowns
                  default_token_model.on("change:config.js_eval.click_link", function (new_token_name, value, options) {
                    if (!value) {
                      return;
                    }
                    // get right row set for multiple tables                         
                    console.log("config.js_eval.click_link---------------->" + value);
                    set_token('config.click_link', eval(value));
                    unset_token('config.js_eval.click_link');
                  });

                  // Create a new instance of the new custom cell renderer
                  let customCellRenderer = new CustomCellRenderer();
                  customCellRenderer.dashboard_name = dashboard_name;
                  customCellRenderer.dashboard_id = view.id;
                  customCellRenderer.tokens = default_token_model;
                  customCellRenderer.tooltip_placement = tooltip_placement;
                  customCellRenderer.has_tooltips = has_tooltips;
                  customCellRenderer.tableView = subview;
                  customCellRenderer.rows = rows;
                  customCellRenderer.fields = fields;
                  customCellRenderer.subview_id = subview.id;
                  
                  // Assigning all of the style functions extensions
                  Object.assign(customCellRenderer, lib);
                  // Adding the Cell Renderer to the table 

                  console.log("ADDING CELL RENDERER");
                  subview.addCellRenderer(customCellRenderer);
                  // Create an instance of the basic row renderer
                  let tableRowRenderer = new RowRenderer();
                  let row_expansion_body = default_token_model.get("config.row_expansion_body");
                  if (row_expansion_body) {
                    console.log("ADDING ROW RENDERER");
                    tableRowRenderer.row_expansion_body = row_expansion_body;
                    subview.addRowExpansionRenderer(tableRowRenderer);
                  }                  
                  
                  subview.viz.children.viz.children.vizList.stats.render();
                  //subview.table.render();
                  custom_cell_renderers[subview.id] = customCellRenderer;
                  console.log(customCellRenderer)

                  // Trigger events after binding in render function
                  if (has_style_file) {
                    read_style_lookup();
                  }

                  if (has_tooltips) {
                    fetch_tooltips(view.id);
                  }

                  // View Refresh Button
                  let refresh_enabled_token_value = default_token_model.get('refresh_enabled');

                  if (!refresh_enabled_token_value) {
                    refresh_enabled_token_value = default_token_model.get('config.refresh_enabled');
                  }

                  if (refresh_enabled_token_value && eval(refresh_enabled_token_value)) {
                    let refresh_enabled_text_token_value = default_token_model.get('refresh_enabled_text');
                    if (!refresh_enabled_text_token_value) {
                      refresh_enabled_text_token_value = default_token_model.get('config.refresh_enabled_text');
                    }
                    let refresh_enabled_text = "refresh";
                    if (refresh_enabled_text_token_value && eval(refresh_enabled_text_token_value)) {
                      refresh_enabled_text = eval(refresh_enabled_text_token_value);
                    }
                    if (!visited_views.includes(subview.id)) {
                      $(`#${view.id}`).prepend(`<button id='refresh_${view.id}' class='btn btn-primary pull-right'>${refresh_enabled_text}</button>`);

                      document.getElementById('refresh_' + view.id).addEventListener('click', function () {
                        let subview_search = mvc.Components.getInstance(subview.options.managerid);
                        subview_search.startSearch();
                      });

                      visited_views.push(subview.id);
                    }
                  }

                  // hide pagination when row_count is set to 0 (ALL)
                  // <row><panel><table id="mine">...</table></panel></row>
                  // <row><panel><table id="yours">...</table></panel></row>
                  // <set token="configure.hide_pagination">mine,yours</set>
                  // $('#mine .splunk-view .splunk-paginator').hide()
                  // $('#yours .splunk-view .splunk-paginator').hide()
                  let hide_pagination_token_value = default_token_model.get('config.hide_pagination');
                  if (hide_pagination_token_value) {
                    let hide_pagination_table_ids = eval(hide_pagination_token_value).split(",");
                    for (let table_id of hide_pagination_table_ids) {
                      $(`#${table_id} .splunk-view .splunk-paginator`).hide();
                    }
                  }

                  subview.$el.addClass('custom_cell_renderer');

                  subview.$el.addClass(`custom_cell_renderer_${i + 1}`);

                  // Reject promise when subview render failed

                  --all_view_instances_count;
                  rendered_views[subview.id] = true;

                  console.log("Adding " + subview.id + "_nested_headers_formatted listener");

                  // Add an event listener for when headers are formatted.
                  // Headers must be formatted before applying DataTables
                  $(document).on(subview.id + "_nested_headers_formatted", function (e, opts) {
                    --total_view_nested_headers_count;
                    nested_headers_formatted[subview.id] = true;

                    let datatables_id_token_value = default_token_model.get(`datatables#${view.id}`);
                    let datatables_idx_token_value = default_token_model.get(`datatables${i + 1}`);

                    if (datatables_global_token_value && eval(datatables_global_token_value)) {
                      if (datatables_id_token_value && (!eval(datatables_id_token_value))) {
                        console.log(`datatables#${view.id} disabled`);
                        set_token(`tbl#${view.id}_display`, true);
                        set_token(`tbl${i + 1}_display`, true);
                        resolve(subview);
                      } else if (datatables_idx_token_value && (!eval(datatables_idx_token_value))) {
                        console.log(`datatables${i + 1} disabled`);
                        set_token(`tbl#${view.id}_display`, true);
                        set_token(`tbl${i + 1}_display`, true);
                        resolve(subview);
                      } else {
                        // setup datatables
                        // Show Hide Column Checkboxes  
                        let datatables_show_hide_column_token_value = default_token_model.get(`datatables#${view.id}.show_hide_column`);
                        if (!datatables_show_hide_column_token_value) {
                          // check for global setting, table specific gets precedence
                          datatables_show_hide_column_token_value = default_token_model.get(`show_hide_column`);
                        }

                        // we don't want underscore fields in datatables
                        let datatable_fields = [];

                        // to avoid possibility of wrong fields being passed into datatable
                        // initialization, we need to use the all_views structure constructed
                        // before this callback
                        for (let field of all_views[all_views.length - 1].fields) {
                          if (!field.startsWith("_")) {
                            datatable_fields.push(field);
                          }
                        }

                        // avoid potential repeat datatable setup due to nested headers strangeness
                        if (!$.fn.DataTable.isDataTable("#" + subview.id + " table")) {
                          splunk_datatables.setupDataTable(subview, view.id, i, datatable_fields);
                        }

                        if (datatables_show_hide_column_token_value && eval(datatables_show_hide_column_token_value)) {
                          if (!visited_views_checkboxes.includes(subview.id)) {
                            let show_hide_column_config = eval(datatables_show_hide_column_token_value);
                            let input_html = [];

                            for (let checkbox_row of show_hide_column_config) {
                              input_html.push('<form style="position:absolute;">');
                              for (let input_config of checkbox_row) {
                                // add subview.id to input id because there can be multiple tables and multiple checkboxes
                                input_html.push(`<label for="${subview.id}_${input_config.id}" class="show_hide pull-left" style="margin-left:1px"><input type="checkbox" id="${subview.id}_${input_config.id}" name="hide" class="show_hide" style="margin-left:3px" checked></input>${input_config.label}</label>`);
                              }
                              input_html.push('</form><br>');
                            }

                            $(`#${view.id}`).prepend(input_html.join(' '));

                            // repeat loop to add event listener to newly created DOM elements
                            for (let checkbox_row of show_hide_column_config) {
                              for (let input_config of checkbox_row) {
                                document.getElementById(`${subview.id}_${input_config.id}`).addEventListener('click', function () {
                                  splunk_datatables.show_hide_column(subview.id, this, input_config.cols, datatable_fields);
                                });
                              }
                            }

                            visited_views_checkboxes.push(subview.id);
                          }
                        }
                        resolve(subview);
                      }
                    } else {
                      set_token(`tbl#${view.id}_display`, true);
                      set_token(`tbl${i + 1}_display`, true);
                      resolve(subview);
                    }
                  });
                } catch (err) {
                  console.log(err);
                  console.log("gracefully resolving");
                  resolve(false);
                }
              }
            });
          })
        }));

      console.log(deferred);

      if (deferred.length === all_view_instances.length) {
        Promise.all(deferred)
          .then(function (values) {
            console.log("RESOLVING ALL PROMISES");
            console.log(values);
            $(document).trigger("full_render_complete");
          }, function (reason) {
            console.log("promise failed");
            console.log(reason);
          });
      }
    });    
  } 

  // globals, var variables will be hoisted    
  // Obtain a reference to the tokens service
  var default_token_model = mvc.Components.get("default");
  var submitted_token_model = mvc.Components.get("submitted");
  var custom_cell_renderers_success = {};

  // For token in default_token_model.attributes
  var page_info = splunk_utils.getPageInfo();
  var app = page_info.app;
  var page = page_info.page;
  var dashboard_name = `app_${app}_${page}`;
  var all_default_tokens = default_token_model.attributes;

  // Extend default token model
  default_token_model.formAttributes = function () {
    let result = {},
      key;

    for (key in all_default_tokens) {
      if (all_default_tokens.hasOwnProperty(key) && key.includes("form.") && key !== "form.dashboard_status") {
        result[key] = all_default_tokens[key];
      }
    }

    return result;
  }();

  let window_title = default_token_model.get("config.window_title");
  if (window_title) {
    document.title = window_title;
  }

  let sorted_form_tokens = Object.entries(default_token_model.formAttributes).sort((a, b) => a[0].localeCompare(b[0]));

  var dashboard_url_id = dashboard_name;

  if (sorted_form_tokens.length > 0) {
    dashboard_url_id = dashboard_name + "_" + sorted_form_tokens.map(x => x[1]).join("_");
  }

  console.log(`dashboard_url_id: ${dashboard_url_id}`);

  let all_view_instances = mvc.Components.getInstances().filter(function (view) {
    return (typeof view.getVisualization != "undefined");
  });

  // Run rendering
  let addCustomCellRenderer = function () {
    run(all_view_instances);
  }  
  
  let addCellRendererOnce = _.once(addCustomCellRenderer);
  addCellRendererOnce();
});
