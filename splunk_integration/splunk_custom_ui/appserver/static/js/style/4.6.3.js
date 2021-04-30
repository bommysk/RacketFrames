// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});


// Translations for en_US
i18n_register({ "catalog": {}, "plural": function(n) { return n == 1 ? 0 : 1; } });

require.config({
  waitSeconds: 10,  
  paths: {    
    'lib': '../app/mxd3/js/style/lib/latest',
    'nested_headers': '../app/mxd3/js/double_headers/6.7',
    'datatables.net': '../app/mxd3/gizmos/DataTables/js/jquery.dataTables.min'    
  }
});

require([
    'underscore',
    'jquery',
     "splunkjs/mvc/utils",
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/searchmanager',
    'splunkjs/mvc/savedsearchmanager',
    'lib',
    'nested_headers',
    'datatables.net',
    'splunkjs/mvc/simplexml/ready!'

], function(_, $, utils, mvc, TableView, SearchManager, SavedSearchManager, lib, nested_headers) {

    var CustomCellRenderer = TableView.BaseCellRenderer.extend({
        row_index: -1,
        row_count: 0,
        rows: {},
        canRender: function(cell) {
            if (cell.dataOverlay === "heatmap" && cell.columnType === "number") {
                return false;
            }

            return true;
        },
        // monitor the current row and column index
        setup: function($container, cell) {
            if (cell.index == 0) {
                this.row_index++;
                this.row_count++;
            }

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

            //read lookup only once
            if (!$.cookie(this.style_lookup_cookie)) {
                var that = this;

                // Add an event listener for when lookup data is ready
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
                } catch (err) {
                    console.error("Failed evaluating style for field " + cell.field + ". Error:", err);
                }
            }

            this.tooltips_cookie = this.dashboard_name + "_tooltips_cookie";

            //read lookup only once
            if (!$.cookie(this.tooltips_cookie)) {
                var that = this;

                $(document).on("fetched_tooltips", function(e, opts) {
                    var tip = opts.data.tooltip;

                     html_value = _.template('<span data-toggle="tooltip" data-placement="left" title="<%- tip%>"><%- value%></span>', {
                        tip: tip,
                        value: value
                    });
                });
            } else {
                var data = JSON.parse($.cookie(this.tooltips_cookie))[0];

                //console.log(data);
                var tip = data.tooltip;

                html_value = _.template('<span data-toggle="tooltip" data-placement="left" title="<%- tip%>"><%- value%></span>', {
                    tip: tip,
                    value: value
                });
            }

            if ($.isArray(html_value)) {
                var multi_value_html = "";

                html_value.forEach(function (element, idx) {
                    multi_value_html += `<div tabindex="${idx}" class="multivalue-subcell" data-mv-index="${idx}">${element}</div>`;
                });

                $td.html(multi_value_html);
            } else {
                $td.html(html_value);
            }

            // This line wires up the Bootstrap tooltip to the cell markup
            $td.children('[data-toggle="tooltip"]').tooltip();
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

    // global keep track of rendered datatables
    var rendered_datatables = {};
    function setupDataTable(subview, i) {
      let datatables_token_value_i = default_token_model.get(`datatables${i + 1}`);

      if (datatables_token_value_i && (! eval(datatables_token_value_i))) {
        set_token(`tbl${i + 1}_display`, true);
        return;
      }

      let table_selector = "#" + subview.id + " table";
                                           
      setTimeout(
          function() {
              console.log(`${table_selector} render compelete`);
              console.log(`${table_selector} nested headers complete`);              

              // remove Splunk sorting
              var table_headers = $(`${table_selector} th`);
              table_headers.removeClass("sorts");
              $(`${table_selector} th .icon-sorts`).remove();

              table_headers.find("a[href='#']")
              .removeAttr("href")
              .css("cursor","pointer")
              .css("pointer-events","none")
              .css("color", "black");

              // remove Splunk paginator and just use Datatables
              $(".splunk-paginator").remove();

              $(table_selector).css('width', '100%');
              $($.fn.dataTable.tables( true ) ).css('width', '100%');
              $($.fn.dataTable.tables( true ) ).DataTable().columns.adjust().draw();

              if (! rendered_datatables[subview.id]) {
                initDataTable(table_selector, i);
                rendered_datatables[subview.id] = true; 
              }

              set_token(`tbl${i + 1}_display`, true);
          }, 500);

      // extra check to make sure datatables are initialized
      setTimeout(function() {
        if (! $.fn.DataTable.isDataTable( table_selector )) {
          console.log(`init datatables again: ${table_selector}`);
          initDataTable(table_selector, i);                                 
        }
        else {
          console.log(`${table_selector} table setup`);
        }                                   

      }, 5000);
    }    

    function initDataTable(table_selector, i) {
      var th_list = [];
      var toggle_column = '<div class="btn-toolbar scrollmenu" role="toolbar"><div id="toggle-columns" class="btn-group btn-group-justified" role="group">';

      $(table_selector + " thead tr th").each(function(index){
          var title = $(this).text();
          // by standard we should number rows we can bring this condition back
          // then
          //if (index == 0)
          //  return true;
          toggle_column += '<button type="button" class="toggle-vis btn btn-primary active" data-column="' + index + '">' + title + '</button> ';
      });

      toggle_column += '</div></div><br/>';
      //$(toggle_column).insertBefore('table');

      // datatable config defaults
      let datatable_row_count = 100;
      let datatable_search = true;
      let border_targets = [];      

      let datatable_global_row_count_token = default_token_model.get("datatables.config.row_count");
      let datatable_global_search_token = default_token_model.get("datatables.config.search");
      let datatable_global_borders_token = default_token_model.get("datatables.config.borders");      

      // globals take less precendence than specific tables by index
      // and may be overwritten in following code block
      if (datatable_global_row_count_token) {
        datatable_row_count = eval(datatable_global_row_count_token);
      }

      if (datatable_global_search_token) {
        datatable_search = eval(datatable_global_search_token);
      }

      if (datatable_global_borders_token) {
        let datatable_borders_cols = String(eval(datatable_global_borders_token));
        datatable_borders_cols = datatable_borders_cols.split(",");

        datatable_borders_cols.forEach(function(col) {
          border_targets.push(parseInt(col.trim()));
        });
      }

      // datatable config index specific
      let datatable_row_count_token = default_token_model.get(`datatables${i + 1}.config.row_count`);
      let datatable_search_token = default_token_model.get(`datatables${i + 1}.config.search`);
      let datatable_borders_token = default_token_model.get(`datatables${i + 1}.config.borders`);      

      if (datatable_row_count_token) {
        datatable_row_count = eval(datatable_row_count_token);
      }

      if (datatable_search_token) {
        datatable_search = eval(datatable_search_token);
      }

      if (datatable_borders_token) {
        let datatable_borders_cols = String(eval(datatable_borders_token));
        datatable_borders_cols = datatable_borders_cols.split(",");

        datatable_borders_cols.forEach(function(col) {
          border_targets.push(parseInt(col.trim()));
        });
      }      

      console.log("border_targets");
      console.log(border_targets);          

      let columnDefs = [{ className: "border_left", "targets": border_targets }];      

      var table_options = {
          "order": [],
          "scrollY":       "800px",
          "scrollX":      true,
          "scrollCollapse": true,
          "searching": datatable_search,
          "paging":         true,    
          "pagingType": "full_numbers",
          "pageLength": datatable_row_count,
          "lengthMenu": [[datatable_row_count, -1], [datatable_row_count, "All"]],          
          "info": false,          
          "fixedHeader": {
              header: true,
              footer: true
          },          
          "stateSave": true,
          "dom": '<"top"f>rt<"bottom"ilp><"clear">',          
          "deferRender": true,
          "colReorder": true,
          "retrieve": true,
          "fnDrawCallback": function() {
            setTimeout(function() {                
              if ( $(`#DataTables_Table_${i}_paginate  > span > a.paginate_button`).size() > 1 ) {                  
                $(`#DataTables_Table_${i}_paginate`).show();
                $(`#DataTables_Table_${i}_length`).show();
              } 
              else {                  
                $(`#DataTables_Table_${i}_paginate`).hide();
                $(`#DataTables_Table_${i}_length`).hide();
              }
            }, 2000);
          },
          columnDefs
      };

      // const header_scroll_length = 12;

      // if ($(table_selector + " thead tr th").length < header_scroll_length) {
      //   delete table_options.scrollX;
      //   delete table_options.fixedColumns;
      // }

      // Datatables object created
      var table = $(table_selector).DataTable(table_options);

      // row selection
      $(table_selector + " tbody").on( 'click', 'tr', function () {
        $(this).toggleClass('selected');
      });

      $(table_selector + " tbody").on( 'click', 'tr', function () {
        if ( $(this).hasClass('selected') ) {
            $(this).removeClass('selected');
        }
        else {
            table.$('tr.selected').removeClass('selected');
            $(this).addClass('selected');
        }
      });

      $(table_selector + '_delete_button').click( function () {
          table.row('.selected').remove().draw( false );
      } );


      $('button.toggle-vis').on( 'click', function (e) {
        e.preventDefault();

        // Get the column API object
        var column = table.column( $(this).attr('data-column') );

        // Toggle the visibility
        column.visible( ! column.visible() );

        $(this).toggleClass('active',function(index,current_class) { if (current_class === 'active') {return 'disabled'} else { return 'active' } });
      });

        // adjust header width for initially hidden tables
        // datatables headers comes out deformed because it doesn't initially know width
      $('a[data-toggle="tab"]').on( 'shown.bs.tab', function (e) {
        $.fn.dataTable.tables( {visible: true, api: true} ).columns.adjust();
      });

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
            app: app            
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
            console.log(state.content);
            if (state.content.resultCount > 0) {
              console.log(`delayed resolving tbl${display_counter + 1} cached on data`);
              // set_token(`tbl${display_counter}_cached`, true);              
              // resolve(idx);              
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

    function read_style_lookup(dashboard_name) {
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

    function fetch_tooltips(dashboard_name) {
      // get the value of the token
      let searchManager = new SearchManager({
        cache: false,
        search: `index=playground1 datatype=tooltip dashboard=${dashboard_name} | table *`
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

          console.log("TOOLTIPS");

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
      let datatables_token_value = default_token_model.get("datatables");       

      if (has_style_file) {
        read_style_lookup(dashboard_name);
      }

      // get tool tips of whole dashboard not individual tables
      if (has_tooltips) {
        fetch_tooltips(dashboard_name);
      }

      let all_views = [];
      // keep track of rendered views
      let rendered_views = {};        
      let nested_headers_formatted = {};
      let deferred = [];

      console.log(`total view instances: ${all_view_instances_count}`);
      console.log(all_view_instances);

      $.each(all_view_instances, function(i, view) {
        // Apply only on Data Tables
        deferred.push(
          new Promise(function (resolve, reject) {
            view.getVisualization(function(subview) {              
              console.log(`tbl${i + 1}`);       
              unset_token(`tbl${i + 1}_display`);

              // Obtain a reference to the search Manager of the table
              //var searchManager = mvc.Components.getInstance(view.manager.id);
              var searchManager = mvc.Components.getInstance(view.options.managerid);
              var searchResults = searchManager.data('results', {
                  output_mode: 'json_rows',
                  count: 0
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

                  try {
                    
                    // Create a new instance of the new custom cell renderer
                    var customCellRenderer = new CustomCellRenderer();
                    customCellRenderer.dashboard_name = dashboard_name;
                    customCellRenderer.dashboard_id = view.id;
                    customCellRenderer.tokens = default_token_model;
                    customCellRenderer.tableView = subview;
                    customCellRenderer.rows = rows;
                    // Assigning all of the style functions extensions
                    Object.assign(customCellRenderer, lib);
                    // Adding the Cell Renderer to the table
                    subview.addCellRenderer(customCellRenderer);

                    subview.$el.addClass('custom_cell_renderer');

                    subview.$el.addClass(`custom_cell_renderer_${i}`);
                  
                    // reject promise when subview render failed
                    subview.on("rendered", function() {                      
                      --all_view_instances_count;
                      rendered_views[subview.id] = true;
                                            
                      // Add an event listener for when headers are formatted.
                      // Headers must be formatted before applying DataTables
                      $(document).on(subview.id + "_nested_headers_formatted", function(e, opts) {
                        --total_view_nested_headers_count; 
                        nested_headers_formatted[subview.id] = true;                        
                    
                        if (datatables_token_value && eval(datatables_token_value)) {              
                          setupDataTable(subview, i);                            
                        }
                        else {                          
                          set_token(`tbl${i + 1}_display`, true);
                        }

                        resolve(subview);
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

          // Headers must be formatted before applying DataTables
          if (redo_datatables) {
            if (datatables_token_value && eval(datatables_token_value)) {
                setupDataTable(view.subview, view.i);                            
            }
          }
        });
      });
    }

    // globals, var variables will be hoisted    
    // Obtain a reference to the tokens service
    var default_token_model = mvc.Components.get("default");
    var submitted_token_model = mvc.Components.get("submitted");

    //for token in default_token_model.attributes
    var page_info = utils.getPageInfo();
    var app = page_info.app;
    var page = page_info.page;
    var dashboard_name = `app_${app}_${page}`;        
    var all_default_tokens = default_token_model.attributes;

    // extend default token model
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

    let cache_enabled = default_token_model.get("cache_enabled");
    let check_cached_views = [];    

    let all_view_instances = mvc.Components.getInstances().filter(function(view) {
      return (typeof view.getVisualization != "undefined");
    });

    // handle cache
    if (cache_enabled && eval(cache_enabled)) {
      console.log("cache enabled");
      let display_counter = 0;            
      all_view_instances.forEach(function(view, idx) {
        // even indexes are cached panels: 0,2,4
        // odd indexes are live panels: 1,3,5        
        if (idx % 2 === 0) {        
          check_cached_views.push(check_scheduled_report_exists(view, idx, display_counter++));
        }
      });

      console.log(check_cached_views);

      Promise.all(check_cached_views)
      .then(function(view_idx_list) {
        all_view_instances = all_view_instances.filter(function(view, idx) {
          return (view_idx_list.includes(idx));
        });

        // run rendering
        run(all_view_instances); 
      }, 
      function(reason) {
        console.log("cache check promise failed");
        console.log(reason);
      });    
    }
    else {
      // run rendering
      run(all_view_instances);
    }
});
