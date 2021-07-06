///////////////////////////////////////////////////////////////////
//                                                               //
// DataTables JS functions to get DataTables to work with Splunk //
//                                                               //
///////////////////////////////////////////////////////////////////

// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

require.config({
  waitSeconds: 0,  
  paths: {    
    'datatables.net': '../app/search/gizmos/DataTables/DataTables-1.10.20/js/jquery.dataTables.min',    
    'datatables.net-bootstrap': '../app/search/gizmos/DataTables/DataTables-1.10.20/js/dataTables.bootstrap.min',    
    'datatables.net-fixed-columns': '../app/search/gizmos/DataTables/FixedColumns-3.3.0/js/dataTables.fixedColumns.min',    
    'datatables.net-auto-fill': '../app/search/gizmos/DataTables/AutoFill-2.3.4/js/dataTables.autoFill.min',
    'datatables.net-buttons': '../app/search/gizmos/DataTables/Buttons-1.6.1/js/dataTables.buttons.min',
    'datatables.net-buttons-flash': '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.flash.min',
    'jszip': '../app/search/gizmos/DataTables/JSZip-3.2.0/jszip.min',
    'pdfmake': '../app/search/gizmos/DataTables/pdfmake-0.1.62/pdfmake.min',
    'vfs-fonts': '../app/search/gizmos/DataTables/pdfmake-0.1.62/vfs_fonts',
    'datatables.net-buttons-html5' : '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.html5.min',
    'datatables.net-buttons-print' : '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.print.min',
    'datatables.net-buttons-colvis' : '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.colVis.min',
    'datatables.net-col-reorder': '../app/search/gizmos/DataTables/ColReorder-1.5.2/js/dataTables.colReorder.min',    
    'datatables.net-key-table': '../app/search/gizmos/DataTables/KeyTable-2.5.1/js/dataTables.keyTable.min',
    'datatables.net-responsive': '../app/search/gizmos/DataTables/Responsive-2.2.3/js/dataTables.responsive.min',
    'datatables.net-row-reorder': '../app/search/gizmos/DataTables/RowReorder-1.2.6/js/dataTables.rowReorder.min',
    'datatables.net-scroller': '../app/search/gizmos/DataTables/Scroller-2.0.1/js/dataTables.scroller.min',
    'datatables.net-select': '../app/search/gizmos/DataTables/Select-1.3.1/js/dataTables.select.min'
  },
  shim: {
    'datatables.net': {
      deps: ['jquery']
    },
    'datatables.net-bootstrap': {
      deps: ['datatables.net']
    },
    'datatables.net-fixed-columns': {
      deps: ['datatables.net']
    },
    'datatables.net-auto-fill': {
      deps: ['datatables.net']
    },    
    'datatables.net-buttons': {
      deps: ['datatables.net']
    },    
    'jszip': {
      deps: ['datatables.net-buttons']
    },
    'pdfmake': {
      deps: ['jszip']
    },
    'vfs-fonts': {
      deps: ['pdfmake']
    },
    'datatables.net-buttons-flash': {
      deps: ['vfs-fonts']
    },
    'datatables.net-buttons-html5': {
      deps: ['datatables.net-buttons-flash']
    },
    'datatables.net-buttons-print': {
      deps: ['datatables.net-buttons-html5']
    },
    'datatables.net-buttons-colvis': {
      deps: ['datatables.net-buttons-html5']
    },
    'datatables.net-col-reorder': {
      deps: ['datatables.net']
    },    
    'datatables.net-key-table': {
      deps: ['datatables.net']
    },    
    'datatables.net-responsive': {
      deps: ['datatables.net']
    },
    'datatables.net-row-reorder': {
      deps: ['datatables.net']
    },
    'datatables.net-scroller': {
      deps: ['datatables.net']
    },
    'datatables.net-select': {
      deps: ['datatables.net']
    }    
  }
});

var libs = [
  'jquery',  
  'jszip',
  'splunkjs/mvc',
  'splunkjs/mvc/simplexml/element/table',
  'splunkjs/mvc/simplexml/ready!',
  'datatables.net',
  'datatables.net-fixed-columns',
  'datatables.net-auto-fill', 
  'datatables.net-buttons',  
  'pdfmake',
  'vfs-fonts',
  'datatables.net-buttons-flash',
  'datatables.net-buttons-html5',
  'datatables.net-buttons-print',
  'datatables.net-buttons-colvis',
  'datatables.net-col-reorder',  
  'datatables.net-key-table',  
  'datatables.net-responsive',
  'datatables.net-row-reorder',
  'datatables.net-scroller',
  'datatables.net-select'
];

define(libs, function ($, jszip, mvc, TableElement) {
  "use strict";

  // Needed to make DataTables export to Excel work
  window.JSZip = jszip;

  $.fn.hasAnyClass = function() {
    for (let i = 0; i < arguments.length; i++) {
      let classes = arguments[i];      
      for (let j = 0; j < classes.length; j++) {
          if (this.hasClass(classes[j])) {
              return true;
          }
      }
    }
    return false;
  }
  

  /**
   * Takes a positive integer and returns the corresponding excel column name.
   * @param {number} idx  The positive integer to convert to a column name.
   * @return {string} The excel column name.
   */
  function to_excel_column_name(idx) {    
    for (var ret = '', a = 1, b = 26; (idx -= a) >= 0; a = b, b *= 26) {
      ret = String.fromCharCode(parseInt((idx % b) / a) + 65) + ret;
    }
    return ret;
  }

  function get_color_class_td($td) {
    let class_list = $td.attr('class').split(/\s+/);

    for (let idx = 0; idx < class_list.length; idx++) {
      if (class_list[idx].includes('lt_') || class_list[idx].includes('light')) {
         return class_list[idx];
      }
    };

    return false;
  }

  function hide_length_paginate() {
    var css = `.dataTables_length {
      display: none !important;
    }

    .dataTables_paginate {
      display: none !important;
    }`,
    head = document.head || document.getElementsByTagName('head')[0],
    style = document.createElement('style');
    style.setAttribute("id", "temp_remove_length_paginate");

    head.appendChild(style);

    style.type = 'text/css';
    if (style.styleSheet){
      // This is required for IE8 and below.
      style.styleSheet.cssText = css;
    } else {
      style.appendChild(document.createTextNode(css));
    }
  }

  hide_length_paginate();

  // globals, var variables will be hoisted    
  // Obtain a reference to the tokens service
  var default_token_model = mvc.Components.get("default");
  var submitted_token_model = mvc.Components.get("submitted");

  function set_token(name, value) {
    default_token_model.set(name, value);
    submitted_token_model.set(name, value);
  }

  function unset_token(name) {
    default_token_model.unset(name);
    submitted_token_model.unset(name);
  }
  
  // Add sorting on numbers inside of HTML
  jQuery.extend( jQuery.fn.dataTableExt.oSort, {
    "html-numeric-pre": function ( a ) {
        var x = String(a).replace( /<[\s\S]*?>/g, "" );
        if (x === 'NA' ) 
          x = -9007199254740992; // -2^64 - smallest number possible
        return parseFloat( x );
    },
 
    "html-numeric-asc": function ( a, b ) {
        return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    },
 
    "html-numeric-desc": function ( a, b ) {
        return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
  });

  // Create auto detect of numbers inside of HTML
  // Put this at the beginning of the check-array
  var default_sort_datatypes = function ( sData ) {
    /* Allow zero length strings as a number */
    if ( typeof sData === 'number' ) {
      return 'html-numeric';
    }
    else if ( typeof sData !== 'string' ) {
      return null;
    }
    else if (sData === 'NA') {
      /* SPLUNK NULL */
      return 'html-numeric';
    }

    sData = String(sData).replace( /<[\s\S]*?>/g, "" );
    
    let sValidFirstChars = "0123456789-";
    let sValidChars = "0123456789.";
    let Char;
    let bDecimal = false;
    
    /* Check for a valid first char (no period and allow negatives) */
    Char = sData.charAt(0); 
    if (sValidFirstChars.indexOf(Char) == -1) {
      return null;
    }
    
    /* Check all the other characters are valid */
    for ( let i=1 ; i<sData.length ; i++ ) 
    {
      Char = sData.charAt(i); 
      if (sValidChars.indexOf(Char) == -1) 
      {
        return null;
      }
      
      /* Only allowed one decimal place... */
      if ( Char == "." )
      {
        if ( bDecimal )
        {
          return null;
        }
        bDecimal = true;
      }
    }
    
    return 'html-numeric';
  };

  jQuery.fn.dataTableExt.aTypes.unshift( default_sort_datatypes );

  // Object for (un_)fake_rowspan functions
  // - holds indexes of the previous row's rowspans
  //   that are also max_rowspans for that row
  // - means no need to un_fake the rowspan when sorting
  var prev_rowspan = {
    rows_remain : 0,
    max_rowspan : 0,
    idxs : []
  };

  // DataTables cannot handle rowspans 
  // - using attributes data-rowspan and data-hide in the HTML emulate rowspan
  //   after table loaded into DataTables

  // Parse a table
  function fake_rowspan(index, oData) {
    var max_rowspan = 0;
    var max_rowspan_cells = [];
    var covered_idxs = [];

    if( prev_rowspan.rows_remain != 0 ) {
      max_rowspan = prev_rowspan.max_rowspan;
      covered_idxs = prev_rowspan.idxs;
    }

    for (var iColumn = 0; iColumn < oData.nTr.childNodes.length; iColumn++) {
      var cell = oData.nTr.childNodes[iColumn];
      var rowspan = typeof($(cell).data('rowspan')) != "undefined"? $(cell).data('rowspan') : 1;
      var hide = $(cell).data('hide');

      if (rowspan > max_rowspan) {
        max_rowspan = rowspan;
        max_rowspan_cells = [$(cell)];
        covered_idxs = [iColumn];
      } else if (rowspan == max_rowspan) {
        max_rowspan_cells.push($(cell));
        covered_idxs.push(iColumn);
      }

      if (hide) {
        if( covered_idxs.indexOf(iColumn) != -1 ) {
          max_rowspan_cells.push($(cell));
        }
        cell.style.display="none";
      } else if (rowspan > 1) {
        cell.rowSpan = rowspan;
      }
    }

    if( prev_rowspan.rows_remain != 0 ) {
      prev_rowspan.rows_remain --;
    } else {
      prev_rowspan.rows_remain = max_rowspan - 1;
      prev_rowspan.max_rowspan = max_rowspan;
      prev_rowspan.idxs = covered_idxs;
    }

    $(max_rowspan_cells).each(function(){
      $(this).addClass('max_rowspan');
    });
  }

  function un_fake_rowspan(index, oData) {
    var sort_idx = oData._aSortData.length - 1;
    var is_max = $(oData.nTr.children[sort_idx]).hasClass('max_rowspan');
    if( is_max ) {
      return;
    }

    for (var iColumn = 0; iColumn < oData.nTr.childNodes.length; iColumn++) {
      var cell = oData.nTr.childNodes[iColumn];
      var rowspan = $(cell).data('rowspan');
      var hide = $(cell).data('hide');

      if (hide) {
        cell.style.display="";
      } else if (rowspan > 1) {
        cell.removeAttribute("rowSpan");
        $(cell).removeClass("max_rowspan");
      }
    }
  }

  // Create DataTable rowspan call back  
  $.fn.dataTableExt.oApi.fnFakeRowspan = function (oSettings) {
    if (oSettings) {
      $.each(oSettings.aoData, fake_rowspan);
    }
    return this;
  };

  (function ( $ ) {
    // Create a link in tables when <tr> is followed by a <!-- comment field
    // If comment is <!--context_id= then link to show_context.php is created
    $.fn.ConvertRowsToLinks = function(attr, call, double_click) {
        return this.each(function() {
          $( this ).find('tbody  > tr').each(function(index, row) {
            if (double_click=="1") {
              var cid = $(row).data('cid');
              if (cid) {
                if (typeof(cid) == "number")
                  cid = "context_id="+cid;
                else if (typeof(cid) == "string")
                  cid = ((cid.search(',') == -1)? "context_id" : "context_ids") + "="+cid;
                row.ondblclick = new Function("document.location.href=\"" + call + "?" + cid + attr + '"');
              }
            }
            row.onmouseover = new Function("$(this).addClass('highlight')");
            row.onmouseout = new Function("$(this).removeClass('highlight')");
          });
        });
    };
    
    //Function to check for the existence of files on disk prior to showing the link to the file based on data attributes attached to td tags
    $.fn.checkLinks = function() {
      var promises = [];
      var filesBySite = {};
      var filesToCheck = $();

      this.each(function() {
        var thisTable = $( this );

        var linksToCheck = thisTable.find('span.check_link');
        filesToCheck = filesToCheck.add(linksToCheck);
        var imgsToCheck  = thisTable.find('span.check_img');
        filesToCheck = filesToCheck.add(imgsToCheck);

        filesToCheck.each(function (idx, ele) {
          var site     = (ele.getAttribute('data-check_site') !== null)? ele.getAttribute('data-check_site') : "";
          var fileName = (ele.getAttribute('data-check_file') !== null)? ele.getAttribute('data-check_file') : "";

          if (site === "" || fileName === "") {
            return;
          }
              
          var server =  mxdSiteServerLookup(site);
              
          if (filesBySite[server] === undefined) {
            filesBySite[server] = {};
          }
            
          if (filesBySite[server][fileName] === undefined) {
            filesBySite[server][fileName] = [];
          }

          filesBySite[server][fileName].push(ele);
        });
      });

        var sandboxString = getUriRoot();

        var auth_data = {}; 

        if (location.search.indexOf('webuser=')>=0) {
           auth_data = {'webuser':mxd_user}; 
        }

        var ajax_opts = {
          type     : "GET",
          cache    : false,
          url      : 'https://'+window.location.hostname+'/'+sandboxString+'connect/rest.php/authKey',
          datatype : 'json',
          data     : auth_data
        };

        ajax_opts.success = function(result) {
          key = result.result.key;
          $.each(filesBySite, function(server, linksToDelete) {
            var dfd = jQuery.Deferred();
            promises.push(dfd);
            var sandboxSite = (sandboxString != '') ? 'site/' : '';
            var url = server + sandboxString + sandboxSite + "files/check_files.php";

            var post_opts = {
              type : 'POST',
              cache : false,
              url : url,
              data : {
                filePaths : Object.keys(linksToDelete)
              },
              crossDomain: true,
              headers : {
                'MXD_OAUTH_USER' : mxd_user,
                'MXD_OAUTH_KEY'  : key
              },
              success : function(data) {
                JSON.parse(data).forEach(function (fileName) {
                  // Check if the link exists before you try to delete it
                  if (fileName in linksToDelete) {
                    linksToDelete[fileName].forEach(function (span) {
                      var src = server + sandboxString + sandboxSite + "files/view.php?file=" + fileName;
                      var href = (span.getAttribute('data-check_link') !== null)? span.getAttribute('data-check_link') : src;

                      if ($(span).hasClass('check_link')) {
                        var wrapper = "<a href='" + href + "'></a>";
                        $(span).wrap(wrapper); //Wrap the span in the link (maintains displayed data)
                      } else if( $(span).hasClass('check_img') ) {
                        var no_cache = (span.getAttribute('data-no_cache') !== null)? "&no-cache" : "";
                        var width = (span.getAttribute('data-img_width') !== null)? "width:"+span.getAttribute('data-img_width') : "max-width:150px";
                        var img = "<a href='" + href + no_cache + "'><img src='" + src + "&thumbnail"+ no_cache + "' style='" + width + "'/></a>";
                        $(span).html(img); //Put image and link in the span
                      }
                    });
                    delete linksToDelete[fileName]; //Clear out the list if it's been found
                  }
                });

                $.each(linksToDelete, function(fileName, deleteFile) {
                  deleteFile.forEach(function (span) {
                    if( $(span).hasClass('check_img') ) {
                      $(span).text('Image Not Found');
                    }
                  });
                });

                window.setTimeout(function() {
                  dfd.resolve();
                }, 3000);
              },
              error : function(e) {
                dfd.resolve();
              }
            };

            // check_files.php call (per site)
            $.ajax(post_opts);
          });
        };

        // MXDConnect call to authKey
        $.ajax(ajax_opts);
      return promises;
    };
  }( jQuery ));

  var thisHtmlPage = (location.search.split('html=')[1]||'').split('&')[0];

  var stateSaveCallback = function ( settings, data ) {
    try {
      (settings.iStateDuration === -1 ? sessionStorage : localStorage).setItem(
        'DataTables_splunk_'+settings.sInstance+'_'+thisHtmlPage,
          JSON.stringify( data )
      );
    } catch (e) {}
  }

  var stateLoadCallback = function ( settings ) {
    try {
      return JSON.parse(
        (settings.iStateDuration === -1 ? sessionStorage : localStorage).getItem(
          'DataTables_splunk_'+settings.sInstance+'_'+thisHtmlPage
        )
      );
    } catch (e) {}
  }

  function show_hide_column(element,class_name) {
    $(oTables).each(function(){
      $(this).DataTable().columns('.'+class_name).visible(element.checked? true: false, false);
      this.api().draw();
    });
  }

  function show_hide_row(element,class_name) {
    $('tr.'+class_name).toggle();
  }

  function updateHeaders() {
    $(oTables).each(function(){
      this.api().draw();
    });
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
    let table_pointer = $(table_selector);
                                         
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

            table_pointer.css('width', '100%');
            $($.fn.dataTable.tables( true ) ).css('width', '100%');
            $($.fn.dataTable.tables( true ) ).DataTable().columns.adjust().draw();            

            if (! rendered_datatables[subview.id]) {
              splunkDatatables(table_selector, i);
              rendered_datatables[subview.id] = true; 
            }

            set_token(`tbl${i + 1}_display`, true);
        }, 500);

    // extra check to make sure datatables are initialized
    setTimeout(function() {
      if (! $.fn.DataTable.isDataTable( table_selector )) {
        console.log(`init datatables again: ${table_selector}`);
        splunkDatatables(table_selector, i);
      }
      else {
        console.log(`${table_selector} table setup`);
      }                                   

    }, 5000);
  }

  var dataTablesTriggers = {};
  var oTables = [];
  var oHeader = [];
  function splunkDatatables(table_selector, i) {
    let splunk_datatable, splunk_datatable_thead_depth;
    let table_pointer = $(table_selector);
    let th_list = [];
    let toggle_column = '<div class="btn-toolbar scrollmenu" role="toolbar"><div id="toggle-columns" class="btn-group btn-group-justified" role="group">';

    $(table_selector + " thead tr th").each(function(index){
        let title = $(this).text();
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
    let datatable_search = {"regex": true};
    let datatable_searching = true;
    let border_targets = [];
    let datatable_left_fixed_columns = undefined;
    let datatable_right_fixed_columns = undefined;    

    let datatable_global_row_count_token = default_token_model.get("datatables.config.row_count");
    let datatable_global_search_token = default_token_model.get("datatables.config.search");
    let datatable_global_borders_token = default_token_model.get("datatables.config.borders");
    let datatable_global_fixed_header_token = default_token_model.get("datatables.config.fixed_header");
    let datatable_global_fixed_footer_token = default_token_model.get(`datatables.config.fixed_footer`);
    let datatable_global_left_fixed_columns_token = default_token_model.get("datatables.config.left_fixed_columns");
    let datatable_global_right_fixed_columns_token = default_token_model.get("datatables.config.right_fixed_columns");
    let datatable_global_callback_token = default_token_model.get("datatables.config.callback");
    let datatable_global_buttons_token = default_token_model.get("datatables.config.buttons");

    // globals take less precendence than specific tables by index
    // and may be overwritten in following code block
    if (datatable_global_row_count_token) {
      datatable_row_count = eval(datatable_global_row_count_token);
    }

    if (datatable_global_search_token) {
      datatable_searching = eval(datatable_global_search_token);      
    }

    if (datatable_global_borders_token) {
      let datatable_borders_cols = String(eval(datatable_global_borders_token));
      datatable_borders_cols = datatable_borders_cols.split(",");

      datatable_borders_cols.forEach(function(col) {
        border_targets.push(parseInt(col.trim()));
      });
    }

    if (datatable_global_left_fixed_columns_token) {
      datatable_left_fixed_columns = eval(datatable_global_left_fixed_columns_token);
    }

    if (datatable_global_right_fixed_columns_token) {
      datatable_right_fixed_columns = eval(datatable_global_right_fixed_columns_token);
    }

    // datatable config index specific
    let datatable_row_count_token = default_token_model.get(`datatables${i + 1}.config.row_count`);
    let datatable_search_token = default_token_model.get(`datatables${i + 1}.config.search`);
    let datatable_borders_token = default_token_model.get(`datatables${1 + 1}.config.borders`);
    let datatable_fixed_header_token = default_token_model.get(`datatables${i + 1}.config.fixed_header`);
    let datatable_fixed_footer_token = default_token_model.get(`datatables${i + 1}.config.fixed_footer`);
    let datatable_left_fixed_columns_token = default_token_model.get(`datatables${i + 1}.config.left_fixed_columns`);
    let datatable_right_fixed_columns_token = default_token_model.get(`datatables${i + 1}.config.right_fixed_columns`);
    let datatable_callback_token = default_token_model.get(`datatables${i + 1}.config.callback`);
    let datatable_buttons_token = default_token_model.get(`datatables${i + 1}.config.buttons`);
    let datatable_columns_toggle_token = default_token_model.get(`datatables${i + 1}.config.columns_toggle`);

    if (datatable_row_count_token) {
      datatable_row_count = eval(datatable_row_count_token);
    }

    if (datatable_search_token) {
      datatable_searching = eval(datatable_search_token);
    }

    if (! datatable_searching) {
      datatable_search = false;
    }

    if (datatable_borders_token) {
      let datatable_borders_cols = String(eval(datatable_borders_token));
      datatable_borders_cols = datatable_borders_cols.split(",");

      datatable_borders_cols.forEach(function(col) {
        border_targets.push(parseInt(col.trim()));
      });
    }

    if (datatable_left_fixed_columns_token) {
      datatable_left_fixed_columns = eval(datatable_left_fixed_columns_token);
    }

    if (datatable_right_fixed_columns_token) {
      datatable_right_fixed_columns = eval(datatable_right_fixed_columns_token);
    }      

    let columnDefs = [{ className: "border_left", "targets": border_targets }];      

    let table_options = {
        "order": [],
        "scrollY":       "800px",
        "scrollX":      true,
        "scrollCollapse": true,
        "scroller":       true,
        "keys":           true,
        "searching": datatable_searching,
        "search": datatable_search,        
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
        "buttons": [],
        "select": {
          style: 'multi'
        },
        "stateSaveCallback": stateSaveCallback,
        "stateLoadCallback": stateLoadCallback,        
        "fnDrawCallback": function ( oSettings ) {
          if (oSettings.aaSorting.length > 0) { // Check if sorting a column
            $.each(oSettings.aoData, un_fake_rowspan);
          }

          setTimeout(function() {                           
            if (splunk_datatable.data().count() > datatable_row_count) {
              if ($("#temp_remove_length_paginate").length) {                
                $('#temp_remove_length_paginate').remove();
              }
            }                                        
            else {                  
             hide_length_paginate();
            }
          }, 500);
        },
        columnDefs
    };

    // FIXME: use for row sum feature
    // "footerCallback": function ( row, data, start, end, display ) {
    //         var api = this.api(), data;
 
    //         // Total over all pages
    //         total = api
    //             .column( 4 )
    //             .data()
    //             .reduce( function (a, b) {
    //                 return intVal(a) + intVal(b);
    //             }, 0 );
 
    //         // Total over this page
    //         pageTotal = api
    //             .column( 4, { page: 'current'} )
    //             .data()
    //             .reduce( function (a, b) {
    //                 return intVal(a) + intVal(b);
    //             }, 0 );
 
    //         // Update footer
    //         $( api.column( 4 ).footer() ).html(
    //             '$'+pageTotal +' ( $'+ total +' total)'
    //         );
    //     }

    if (datatable_left_fixed_columns) {
      if ( !table_options.fixedColumns ) {
        table_options.fixedColumns = {};
      }

      table_options.fixedColumns.leftColumns = datatable_left_fixed_columns;
    }

    if (datatable_right_fixed_columns) {
      if ( !table_options.fixedColumns ) {
        table_options.fixedColumns = {};
      }

      table_options.fixedColumns.rightColumns = datatable_right_fixed_columns;
    }

    if ( (datatable_global_buttons_token && eval(datatable_global_buttons_token)) || (datatable_buttons_token && eval(datatable_buttons_token)) ) {
      table_options.dom   = '<"top"Bfl>rt<"bottom"piT>';
      table_options.buttons = ["copyHtml5", "csvHtml5"];

      table_options.buttons.push({extend: 'excelHtml5',
        autoFilter: true,
        createEmptyCells: true,        
        // Map HTML colors to Excel
        customize: function ( xlsx ) {
          let sheet = xlsx.xl.worksheets['sheet1.xml']; 
          let styles = xlsx.xl['styles.xml'];
          let namespace = styles.lookupNamespaceURI(null);
          // Get dom objects with [0]
          let fills = $('fills', styles)[0];
          let cellXfs = $('cellXfs', styles)[0];     
          let create_excel_color_style = function ( hex ) {                        
            let fill = styles.createElementNS(namespace, 'fill');
            let patternFill = styles.createElementNS(namespace, 'patternFill');
            patternFill.setAttribute("patternType", "solid");
            
            let fgColor = styles.createElementNS(namespace, 'fgColor');
            fgColor.setAttribute("rgb", hex);
            patternFill.appendChild(fgColor);
            fill.appendChild(patternFill);
            fills.appendChild(fill);
            // Get count and increase count by 1
            let fillscount = $('fills', styles).attr('count');
            $('fills', styles).attr('count', fillscount + 1);

            let xf = styles.createElementNS(namespace, 'xf');
            xf.setAttribute("numFmtId", "0");
            xf.setAttribute("fontId", "0");
            xf.setAttribute("fillId", fillscount);
            xf.setAttribute("borderId", "1");
            xf.setAttribute("applyFont", "1");
            xf.setAttribute("applyFill", "1");
            xf.setAttribute("applyBorder", "1");

            cellXfs.appendChild(xf);
          }

          // lt_yellow
          create_excel_color_style('FBF1BA');          
          // lt_greenyellow
          create_excel_color_style('C8FB85');
          // lt_orange
          create_excel_color_style('FCC585');
          // lt_purple
          create_excel_color_style('DDA0DD');
                  
          // Get count
          let cellXfCount = $('cellXfs', styles).attr('count');
          
          // Built-in datatables excel styles
          // https://datatables.net/reference/button/excelHtml5#Built-in-styles          
          let lt_green_style       = 40;
          let let_red_style        = 35;
          let lt_blue_style        = 46;
          let lt_yellow_style      = cellXfCount;          
          let lt_greenyellow_style = cellXfCount + 1;
          let lt_orange_style      = cellXfCount + 2;
          let lt_purple_style      = cellXfCount + 3;
          let light_style          = 31;

          // Increase count by 4 for the 4 new styles above
          $('cellXfs', styles).attr('count', cellXfCount + 4);          
          
          const excel_color_mapping = {
            'lt_green'        : lt_green_style,
            'lt_red'          : let_red_style,
            'lt_blue'         : lt_blue_style,
            'lt_yellow'       : lt_yellow_style,            
            'lt_greenyellow'  : lt_greenyellow_style,
            'lt_orange'       : lt_orange_style,
            'lt_purple'       : lt_purple_style,            
            'light'           : light_style
          };
   
          splunk_datatable.rows().every( function ( row_idx, table_loop, row_loop ) {
            let $row = $(this.node());
            $row.find('td').each(function(idx) {
              const valid_styles = ['lt_green', 'lt_red', 'lt_blue', 'lt_yellow', 'lt_greenyellow', 'lt_orange', 'lt_purple', 'light'];                 
              if ($(this).hasAnyClass(valid_styles)) {                
                // Plus 1 because excel index starts from index 1
                let excel_column_name = to_excel_column_name(idx + 1);
                // Plus 2 because we need to skip the title row and excel index starts from index 1
                let excel_row_idx = row_idx + splunk_datatable_thead_depth + 2;                                
                // console.log(`c[r=${excel_column_name}${excel_row_idx}]`);

                let color_class = get_color_class_td($(this));
                let excel_color_code = '';                
                
                if (color_class) {
                  excel_color_code = excel_color_mapping[color_class];
                }
                
                $(`c[r=${excel_column_name}${excel_row_idx}]`, sheet).attr('s', excel_color_code);
              }
            }); 
          });
        }
      });

      table_options.buttons.push("print");

      table_options.buttons.push({extend: 'pdfHtml5', 
                                  orientation : 'landscape',
                                  pageSize : 'A3',
                                  customize: function(doc) {
                                    // Make text size smaller to fit on page
                                    doc.styles.tableHeader.fontSize = 8;
                                    doc.defaultStyle.fontSize = 8;
                                    doc.styles.tableFooter.fontSize = 8;
                                  } 
      });
      
      table_options.buttons.push({extend: 'colvis', 
                                  columnText: function ( dt, idx, title ) { 
                                     let header = dt.column( idx ).header();                                     
                                     return $(header).data( "sort-key" );
                                  }});

      table_options.buttons.push("colvisRestore");
    }

    if (datatable_buttons_token && (!eval(datatable_buttons_token))) {
      table_options.dom = '<"top"f>rt<"bottom"ilp><"clear">';
      table_options.buttons = [];
    }
    
    table_pointer.addClass("stripe");
    // built-in datatables class to remove excess padding and spaces
    table_pointer.addClass("compact");
    table_pointer.addClass("cell-border");
    table_pointer.addClass("hover");

    // Datatables object created
    splunk_datatable = table_pointer.DataTable(table_options);

    let $thead = $(splunk_datatable.table().header());
    splunk_datatable_thead_depth = $thead.find('tr').length;

    // Click on drilldowns when enter pressed
    splunk_datatable.on( 'key', function ( e, datatable, key, cell, originalEvent ) {
      if (key === 13) {        
        cell.node().click();
      }
    });    

    oTables[i] = splunk_datatable;

    // row selection
    $(table_selector + " tbody").on( 'click', 'tr', function () {
      $(this).toggleClass('selected');
    });

    $(table_selector + " tbody").on( 'click', 'tr', function () {
      if ( $(this).hasClass('selected') ) {
        $(this).removeClass('selected');
      }
      else {
        splunk_datatable.$('tr.selected').removeClass('selected');
        $(this).addClass('selected');
      }
    });

    $(table_selector + '_delete_button').click( function () {
      splunk_datatable.row('.selected').remove().draw( false );
    });

    if (datatable_global_callback_token && !datatable_callback_token) {
      datatable_callback = eval(datatable_global_callback_token);
      eval(datatable_callback + "('" + table_selector + "')");
    }

    if (datatable_callback_token) {
      datatable_callback = eval(datatable_callback_token);
      eval(datatable_callback + "('" + table_selector + "')");
    }

    if (typeof(dataTablesTriggers['init']) === 'function')
      dataTablesTriggers['init'](table_pointer);

    if( typeof(createEditableFields) === 'function' ) {
      createEditableFields(table_pointer);
    }

    if (typeof(dataTablesTriggers['afterRender']) === 'function') {
      dataTablesTriggers['afterRender'](table_pointer);
    }

    table_pointer.DataTable().columns('.splunk_hide_me').visible(false, false);    

    table_pointer.dataTable().fnFakeRowspan();

    if (typeof(dataTablesTriggers['afterShow']) === 'function') {
      dataTablesTriggers['afterShow'](table_pointer);
    }

    $('button.toggle-vis').on( 'click', function (e) {
      e.preventDefault();

      // Get the column API object
      var column = splunk_datatable.column( $(this).attr('data-column') );

      // Toggle the visibility
      column.visible( ! column.visible() );

      $(this).toggleClass('active',function(index,current_class) { if (current_class === 'active') {return 'disabled'} else { return 'active' } });
    });

    // Adjust header width for initially hidden tables
    // Datatables headers comes out deformed because it doesn't initially know width
    $('a[data-toggle="tab"]').on( 'shown.bs.tab', function (e) {
      $.fn.dataTable.tables( {visible: true, api: true} ).columns.adjust();
      $.fn.dataTable.tables( {visible: true, api: true} ).fixedColumns().relayout();
    });

  }

  return {
    setupDataTable
  };

});
