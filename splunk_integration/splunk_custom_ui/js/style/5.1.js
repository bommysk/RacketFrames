// Translations for en_US
i18n_register({ "catalog": {}, "plural": function(n) { return n == 1 ? 0 : 1; } });

require.config({
  waitSeconds: 0,  
  paths: {
    'lib': '../app/mxd3/js/style/lib/latest',
    'nested_headers': '../app/mxd3/js/double_headers/latest',
    'splunk_datatables': '../app/mxd3/js/datatables/latest',
    'utils': '../app/mxd3/js/utils/latest'
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
    'utils',
    'splunkjs/ready!',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, splunk_utils, token_utils, mvc, TableView, SearchManager, SavedSearchManager, lib, nested_headers, splunk_datatables, utils) {
    var CustomCellRenderer = TableView.BaseCellRenderer.extend({
        row_index: -1,
        row_count: 0,
        rows: {},
        initialize: function() {
          // cache token name matching
          if (! this.hasOwnProperty("isRegexToken") )  {
            this.isRegexToken = {};
          }

          for (var token in default_token_model.attributes) {
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
        canRender: function(cell) {
            if ( !cell ) {
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
        setup: function($container, cell) {
            if (cell.index == 0) {
                this.row_index++;
                this.row_count++;
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
            var html_value = value;

            // support for NA
            if (cell && value && value.toString().toLowerCase() == "na" ) {
                $td.addClass("light");
            }

            // support for EX
            if (cell && value && value.toString().toLowerCase() == "ex" ) {
                $td.addClass("lt_blue light");
            }

            this.style_lookup_cookie = this.dashboard_name + "_style_lookup_cookie";

            // read lookup only once
            if (!$.cookie(this.style_lookup_cookie)) {
                var that = this;

                // add an event listener for when lookup data is ready
                $(document).on("style_lookup_ready", function(e, opts) {
                    lib.bg_lookup(cell, $td, opts.data, that.rows);
                });
            } else {
                var data = JSON.parse($.cookie(this.style_lookup_cookie));
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
                } 
                catch (err) {
                    console.error("Failed evaluating style for field " + cell.field + ". Error:", err);
                }
            }

            this.tooltips_cookie = this.dashboard_name + "_tooltips_cookie";

            // read lookup only once
            if (!$.cookie(this.tooltips_cookie)) {
                var that = this;

                $(document).on("fetched_tooltips", function(e, opts) {
                    var tip = opts.data.tooltip;

                     html_value = _.template('<span data-toggle="tooltip" data-placement="left" title="<%- tip%>"><%- value%></span>', {
                        tip: tip,
                        value: value
                    });
                });
            } 
            else {
                let data = JSON.parse($.cookie(this.tooltips_cookie))[0];

                let tip = data.tooltip;

                html_value = _.template('<span data-toggle="tooltip" data-placement="left" title="<%- tip%>"><%- value%></span>', {
                    tip: tip,
                    value: value
                });
            }

            if ($.isArray(html_value)) {
                // render multivalue cells correctly
                let multi_value_html = "";

                html_value.forEach(function (element, idx) {
                  multi_value_html += `<div tabindex="${idx}" class="multivalue-subcell" data-mv-index="${idx}">${element}</div>`;
                });

                $td.html(multi_value_html);
            } 
            else {
              if (this.has_tooltips) { 
                if ($.cookie(this.tooltips_cookie)) {
                  $td.html(html_value);  
                }
                else {
                  let message = value;
                  let tip = value;

                  if ( tip && tip.length && (!tip.includes('<') && !tip.includes('>')) && tip.length > 48 ) {
                    message = message.substring(0,47) + "..." 
                    $td.html(_.template('<a href="#" data-toggle="tooltip" data-placement="left" title="<%- tip%>"><%- message%></a>', {
                      tip: tip,
                      message: message
                    }));
                  }
                }
                                
                // This line wires up the Bootstrap tooltip to the cell markup
                // This won't do anything if tooltips weren't created above
                $td.children('[data-toggle="tooltip"]').tooltip({
                  container : 'body'
                });
              }
              
              // this allows us to use html and xml tags as the cell value
              // i.e. <asserts>
              $td.text(value);
            }
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

          savedSearchManager.on("search:error", function(state, job) {
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);
            resolve(idx + 1);
          });

          savedSearchManager.on("search:fail", function(state, job) {
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);                 
            resolve(idx + 1);          
          });

          savedSearchManager.on("search:done", function(state, job) {
            if (state.content.resultCount > 0) {
              console.log(`delayed resolving tbl${display_counter + 1} cached on data`);            
            }
            else {
              console.log(`resolving tbl${display_counter + 1} not-cached`); 
              set_token(`tbl${display_counter + 1}_go_live`, true);
              resolve(idx + 1);
            }
          });
          
          let savedSearchResults = savedSearchManager.data('results', {
            output_mode: 'json',
            count: 0
          });

          savedSearchResults.on("data", function() {
            if (savedSearchResults.hasData()) {
              console.log(`resolving tbl${display_counter + 1} cached`);
              set_token(`tbl${display_counter + 1}_cached`, true);
              resolve(idx);
            }
          });
        }
        catch (err) {
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

          searchManager.on("search:error", function(state, job) {
            $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, { "data": false });
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);                 
            resolve(idx + 1);
          });

          searchManager.on("search:fail", function(state, job) {           
            $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, { "data": false });
            console.log(`resolving tbl${display_counter + 1} not-cached`);
            set_token(`tbl${display_counter + 1}_go_live`, true);                 
            resolve(idx + 1);
          });

          searchManager.on("search:done", function(state, job) {
            if (state.content.resultCount > 0) {
              console.log(`resolving tbl${display_counter + 1} cached on data`);
              let date = new Date();
              date.setTime(date.getTime() + (60 * 1000));
              $.cookie(`scheduled_report_csv_ready_cookie_${display_counter + 1}`, true, { expires: date }); // expires after 1 minute
              set_token(`tbl${display_counter + 1}_cached`, true);              
              resolve(idx);              
            }
            else {
              $(document).trigger(`scheduled_report_csv_ready_${display_counter + 1}`, { "data": false });
              console.log(`resolving tbl${display_counter + 1} not-cached`); 
              set_token(`tbl${display_counter + 1}_go_live`, true);
              resolve(idx + 1);              
            }
          });

          let searchResults = searchManager.data('results', {
              output_mode: 'json',
              count: 0
          });          
        }
        catch (err) {
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

      if (style_file) {
        let style_lookup = eval(style_file);

        let searchManager = new SearchManager({
            cache: false,
            search: "| inputlookup " + style_lookup
        });

        searchManager.on("search:error", function(state, job) {           
          $(document).trigger("style_lookup_ready", { "data": false });
        });

        searchManager.on("search:fail", function(state, job) {           
          $(document).trigger("style_lookup_ready", { "data": false });
        });

        searchManager.on("search:done", function(state, job) {
          if (state.content.resultCount === 0) {                  
            $(document).trigger("style_lookup_ready", { "data": false });
          }
        });

        let searchResults = searchManager.data('results', {
            output_mode: 'json',
            count: 0
        });

        searchResults.on("data", function() {
          if (searchResults.hasData()) {
            let styleResultArray = searchResults.data()["results"];

            let date = new Date();
            date.setTime(date.getTime() + (60 * 1000));
            $.cookie(dashboard_name + "_style_lookup_cookie", JSON.stringify(styleResultArray), { expires: date }); // expires after 1 minute
            // Create the event, need to pass data as an object, arrays get truncated to just first value
            $(document).trigger("style_lookup_ready", { "data": styleResultArray });
          }
        });
      }
    }

    function fetch_tooltips() {
      let project = default_token_model.get('project');
      let form_query = '';

      for (form_token in default_token_model.formAttributes) {
        form_query += form_token.split('.')[1] + '=' + default_token_model.formAttributes[form_token];
      }
      // get the value of the token
      let searchManager = new SearchManager({
        cache: true,
        search: "`all_project(" + project + ")` " + form_query + ` datatype=tooltip dashboard=${dashboard_name} | table *`
      });

      searchManager.on("search:error", function(state, job) {           
        $(document).trigger("fetched_tooltips", { "data": false });
      });

      searchManager.on("search:fail", function(state, job) {           
        $(document).trigger("fetched_tooltips", { "data": false });
      });

      searchManager.on("search:done", function(state, job) {
        if (state.content.resultCount === 0) {                  
          $(document).trigger("fetched_tooltips", { "data": false });
        }
      });

      let searchResults = searchManager.data('results', {
        output_mode: 'json',
        count: 0
      });      

      searchResults.on("data", function() {
        if (searchResults.hasData()) {
          let tooltips = searchResults.data()["results"];

          console.log("tooltips");
          console.log(tooltips);

          tooltips = tooltips.map(function(t) {
              return JSON.parse(t._raw);
          });

          console.log(tooltips);

          let date = new Date();
          date.setTime(date.getTime() + (60 * 1000));
          $.cookie(dashboard_name + "_tooltips_cookie", JSON.stringify(tooltips), { expires: date }); // expires after 1 minute

          // Create the event, need to pass data as an object, arrays get truncated to just first value
          $(document).trigger("fetched_tooltips", { "data": tooltips });          
        }
      });
    }

    function run(all_view_instances) {
      let has_style_file = default_token_model.get("style_file");
      let has_tooltips = default_token_model.get("tooltips");    
      let all_view_instances_count = all_view_instances.length;
      let total_view_nested_headers_count = all_view_instances_count;
      let datatables_global_token_value = default_token_model.get("datatables");       

      if (has_style_file) {
        read_style_lookup();
      }

      // get tooltips of whole dashboard not individual tables
      has_tooltips = has_tooltips && eval(has_tooltips);
      if (has_tooltips) {
        fetch_tooltips();
      }

      let all_views = [];
      // Keep track of rendered views
      let rendered_views = {};        
      let nested_headers_formatted = {};
      let deferred = [];
      let view_rows = {};

      console.log(`total view instances: ${all_view_instances_count}`);
      console.log(all_view_instances);

      $.each(all_view_instances, function(i, view) {
        // Apply only on Data Tables
        deferred.push(
          new Promise(function (resolve, reject) {
            view.getVisualization(function(subview) {              
              console.log(`tbl${i + 1}`);       
              unset_token(`tbl${i + 1}_display`);
              
              // Handle saved search by id
              // let saved_search = `${dashboard_url_id}_scheduled_report_${display_counter + 1}`;

              // Obtain a reference to the search manager of the table
              var searchManager = mvc.Components.getInstance(view.options.managerid);
              var searchResults = searchManager.data('results', {
                  output_mode: 'json_rows',
                  count: 0,
                  cache: true
              });

              searchManager.on("search:error", function(state, job) {           
                set_token(`tbl${i + 1}_display`, true);
                resolve(false);
              });

              searchManager.on("search:fail", function(state, job) {           
                set_token(`tbl${i + 1}_display`, true);
                resolve(false);
              });

              searchManager.on("search:done", function(state, job) {
                if (state.content.resultCount === 0) {                  
                  set_token(`tbl${i + 1}_display`, true)
                  resolve(false);
                }
              });

              // Reigster a callback for when the data is being loaded
              searchResults.on("data", function() {
                if (searchResults.hasData()) {
                  all_views.push({subview, i});   
                  
                  var data = searchResults.data();
                  var fields = data.fields;
                  var data_rows = data.rows;
                  var total_rows = data_rows.length;

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
                    let re = /^js_eval\.(.*)$/;
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
                    default_token_model.on("change:js_eval.click_link", function(new_token_name, value, options) {                                                  
                         if (!value) {
                             return;
                         }
                         // get right row set for multiple tables                         
                         console.log("js_eval.click_link---------------->"+value);                                  
                         set_token('click_link', eval(value));
                         unset_token('js_eval.click_link');
                     });

                    // Create a new instance of the new custom cell renderer
                    var customCellRenderer = new CustomCellRenderer();
                    customCellRenderer.dashboard_name = dashboard_name;
                    customCellRenderer.dashboard_id = view.id;
                    customCellRenderer.tokens = default_token_model;
                    customCellRenderer.has_tooltips = has_tooltips;
                    customCellRenderer.tableView = subview;
                    customCellRenderer.rows = rows;
                    // Assigning all of the style functions extensions
                    Object.assign(customCellRenderer, lib);
                    // Adding the Cell Renderer to the table
                    subview.addCellRenderer(customCellRenderer);
                    // Render
                    subview.render();

                    subview.$el.addClass('custom_cell_renderer');

                    subview.$el.addClass(`custom_cell_renderer_${i + 1}`);
                  
                    // Reject promise when subview render failed
                    subview.on("rendered", function() {                      
                      --all_view_instances_count;
                      rendered_views[subview.id] = true;
                     
                                            
                      // Add an event listener for when headers are formatted.
                      // Headers must be formatted before applying DataTables
                      $(document).on(subview.id + "_nested_headers_formatted", function(e, opts) {
                        --total_view_nested_headers_count; 
                        nested_headers_formatted[subview.id] = true;

                        let datatables_token_value = default_token_model.get(`datatables${i + 1}`);                        
                    
                        if (datatables_global_token_value && eval(datatables_global_token_value)) {
                          if (datatables_token_value && (! eval(datatables_token_value))) {
                            console.log(`datatables${i + 1} disabled`);
                            set_token(`tbl${i + 1}_display`, true);
                          }
                          else {
                            splunk_datatables.setupDataTable(subview, i);
                          }
                        }
                        else {
                          resolve(subview);                       
                          set_token(`tbl${i + 1}_display`, true);
                        }                        
                      });
                    });
                  }
                  catch (err) {
                    console.log(err);
                    console.log("gracefully resolving");
                    resolve(false);
                  }
                }
                else {
                  // no data found resolve false
                  set_token(`tbl${i + 1}_display`, true);
                  resolve(false);
                }
              });              
            })
          }));

          console.log(deferred);          

          if (deferred.length === all_view_instances.length) {            
            Promise.all(deferred)
            .then(function(values) {
              console.log("RESOLVING ALL PROMISES");
              console.log(values);            
              $(document).trigger("full_render_complete");
            }, function(reason) {
               console.log("promise failed");
               console.log(reason);
            });
          }
      });

      $(document).on("full_render_complete", function(e, opts) {
        console.log("full render callback");        
        all_views.forEach(function(view) {
          var redo_datatables = false;

          if (view.subview && view.subview.table && !rendered_views[view.subview.id]) {        
            console.log(`Initial render not successful.. retrying: ${view.subview.id}`)

            view.subview.table.render();

            redo_datatables = true;
          }

          if (view.subview && !nested_headers_formatted[view.subview.id]) {
            console.log(`Initial nested headers not successful.. retrying: ${view.subview.id}`);
                  
            nested_headers.timedCount(view.subview);

            redo_datatables = true;
          }

          let datatables_token_value = default_token_model.get(`datatables${view.i + 1}`);

          // Headers must be formatted before applying DataTables          
          if (datatables_global_token_value && eval(datatables_global_token_value)) {
            if (datatables_token_value && (! eval(datatables_token_value))) {
              console.log(`datatables${view.i + 1} disabled`);
            }
            else {
              let table_selector = "#" + view.subview.id + " table";
              if (! $.fn.DataTable.isDataTable( table_selector )) {
                console.log(`setup datatables again: ${table_selector}`);
                splunk_datatables.setupDataTable(view.subview, view.i);
              }
              else {
                console.log(`${table_selector} table setup`);
              } 
            }
          }
        });
      });
    }

    // globals, var variables will be hoisted    
    // Obtain a reference to the tokens service
    var default_token_model = mvc.Components.get("default");
    var submitted_token_model = mvc.Components.get("submitted");

    // For token in default_token_model.attributes
    var page_info = splunk_utils.getPageInfo();
    var app = page_info.app;
    var page = page_info.page;
    var dashboard_name = `app_${app}_${page}`;
    var all_default_tokens = default_token_model.attributes;

    // Extend default token model
    default_token_model.formAttributes = function() {
      let result = {}, key;

      for (key in all_default_tokens) {
        if (all_default_tokens.hasOwnProperty(key) && key.includes("form.") && key !== "form.dashboard_status") {
          result[key] = all_default_tokens[key];
        }
      }

      return result;
    }();

    let sorted_form_tokens = Object.entries(default_token_model.formAttributes).sort((a, b) => a[0].localeCompare(b[0]));

    var dashboard_url_id = dashboard_name;

    if (sorted_form_tokens.length > 0) {
      dashboard_url_id = dashboard_name + "_" + sorted_form_tokens.map(x => x[1]).join("_");
    }    

    console.log(`dashboard_url_id: ${dashboard_url_id}`);

    let all_view_instances = mvc.Components.getInstances().filter(function(view) {
      return (typeof view.getVisualization != "undefined");
    });

    // Run rendering
    run(all_view_instances);
});
