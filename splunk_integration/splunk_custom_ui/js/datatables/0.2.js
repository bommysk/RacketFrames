///////////////////////////////////////////////////////////////////
//                                                               //
// DataTables JS functions to get DataTables to work with Splunk //
//                                                               //
///////////////////////////////////////////////////////////////////

// Translations for en_US
i18n_register({
  "plural": function (n) {
    return n == 1 ? 0 : 1;
  },
  "catalog": {}
});

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
    'datatables.net-buttons-html5': '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.html5.min',
    'datatables.net-buttons-print': '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.print.min',
    'datatables.net-buttons-colvis': '../app/search/gizmos/DataTables/Buttons-1.6.1/js/buttons.colVis.min',
    'datatables.net-col-reorder': '../app/search/gizmos/DataTables/ColReorder-1.5.2/js/dataTables.colReorder.min',
    'datatables.net-key-table': '../app/search/gizmos/DataTables/KeyTable-2.5.1/js/dataTables.keyTable.min',
    'datatables.net-responsive': '../app/search/gizmos/DataTables/Responsive-2.2.3/js/dataTables.responsive.min',
    'datatables.net-row-reorder': '../app/search/gizmos/DataTables/RowReorder-1.2.6/js/dataTables.rowReorder.min',
    'datatables.net-scroller': '../app/search/gizmos/DataTables/Scroller-2.0.1/js/dataTables.scroller.min',
    'datatables.net-select': '../app/search/gizmos/DataTables/Select-1.3.1/js/dataTables.select.min',
    'datatables.rows-group': '../app/search/gizmos/DataTables/RowsGroup/js/dataTables.rowsGroup'
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
    },
    'datatables.rows-group': {
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
  'datatables.net-select',
  'datatables.rows-group'
];

define(libs, function ($, jszip, mvc, TableElement) {
  "use strict";

  // Needed to make DataTables export to Excel work
  window.JSZip = jszip;

  $.fn.hasAnyClass = function () {
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
    for (var ret = '', a = 1, b = 26;
      (idx -= a) >= 0; a = b, b *= 26) {
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
    if (style.styleSheet) {
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
  jQuery.extend(jQuery.fn.dataTableExt.oSort, {
    "html-numeric-pre": function (a) {
      var x = String(a).replace(/<[\s\S]*?>/g, "");
      if (x === 'NA')
        x = -9007199254740992; // -2^64 - smallest number possible
      return parseFloat(x);
    },

    "html-numeric-asc": function (a, b) {
      return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    },

    "html-numeric-desc": function (a, b) {
      return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
  });

  // Create auto detect of numbers inside of HTML
  // Put this at the beginning of the check-array
  var default_sort_datatypes = function (sData) {
    /* Allow zero length strings as a number */
    if (typeof sData === 'number') {
      return 'html-numeric';
    } else if (typeof sData !== 'string') {
      return null;
    } else if (sData === 'NA') {
      /* SPLUNK NULL */
      return 'html-numeric';
    }

    sData = String(sData).replace(/<[\s\S]*?>/g, "");

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
    for (let i = 1; i < sData.length; i++) {
      Char = sData.charAt(i);
      if (sValidChars.indexOf(Char) == -1) {
        return null;
      }

      /* Only allowed one decimal place... */
      if (Char == ".") {
        if (bDecimal) {
          return null;
        }
        bDecimal = true;
      }
    }

    return 'html-numeric';
  };

  jQuery.fn.dataTableExt.aTypes.unshift(default_sort_datatypes);

  // Object for (un_)fake_rowspan functions
  // - holds indexes of the previous row's rowspans
  //   that are also max_rowspans for that row
  // - means no need to un_fake the rowspan when sorting
  var prev_rowspan = {
    rows_remain: 0,
    max_rowspan: 0,
    idxs: []
  };

  // DataTables cannot handle rowspans 
  // - using attributes data-rowspan and data-hide in the HTML emulate rowspan
  //   after table loaded into DataTables

  // Parse a table
  function fake_rowspan(index, oData) {
    var max_rowspan = 0;
    var max_rowspan_cells = [];
    var covered_idxs = [];

    if (prev_rowspan.rows_remain != 0) {
      max_rowspan = prev_rowspan.max_rowspan;
      covered_idxs = prev_rowspan.idxs;
    }

    for (var iColumn = 0; iColumn < oData.nTr.childNodes.length; iColumn++) {
      var cell = oData.nTr.childNodes[iColumn];
      var rowspan = typeof ($(cell).data('rowspan')) != "undefined" ? $(cell).data('rowspan') : 1;
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
        if (covered_idxs.indexOf(iColumn) != -1) {
          max_rowspan_cells.push($(cell));
        }
        cell.style.display = "none";
      } else if (rowspan > 1) {
        cell.rowSpan = rowspan;
      }
    }

    if (prev_rowspan.rows_remain != 0) {
      prev_rowspan.rows_remain--;
    } else {
      prev_rowspan.rows_remain = max_rowspan - 1;
      prev_rowspan.max_rowspan = max_rowspan;
      prev_rowspan.idxs = covered_idxs;
    }

    $(max_rowspan_cells).each(function () {
      $(this).addClass('max_rowspan');
    });
  }

  function un_fake_rowspan(index, oData) {
    var sort_idx = oData._aSortData.length - 1;
    var is_max = $(oData.nTr.children[sort_idx]).hasClass('max_rowspan');
    if (is_max) {
      return;
    }

    for (var iColumn = 0; iColumn < oData.nTr.childNodes.length; iColumn++) {
      var cell = oData.nTr.childNodes[iColumn];
      var rowspan = $(cell).data('rowspan');
      var hide = $(cell).data('hide');

      if (hide) {
        cell.style.display = "";
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

  (function ($) {
    //context_id= then link to show_context.php is created
    $.fn.ConvertRowsToLinks = function (attr, call, double_click) {
      return this.each(function () {
        $(this).find('tbody  > tr').each(function (index, row) {
          if (double_click == "1") {
            var cid = $(row).data('context_id');
            if (cid) {
              if (typeof (cid) == "number")
                cid = "context_id=" + cid;
              else if (typeof (cid) == "string")
                cid = ((cid.search(',') == -1) ? "context_id" : "context_ids") + "=" + cid;
              row.ondblclick = new Function("document.location.href=\"" + call + "?" + cid + attr + '"');
            }
          }
          row.onmouseover = new Function("$(this).addClass('highlight')");
          row.onmouseout = new Function("$(this).removeClass('highlight')");
        });
      });
    };

    //Function to check for the existence of files on disk prior to showing the link to the file based on data attributes attached to td tags
    $.fn.checkLinks = function () {
      var promises = [];
      var filesBySite = {};
      var filesToCheck = $();

      this.each(function () {
        var thisTable = $(this);

        var linksToCheck = thisTable.find('span.check_link');
        filesToCheck = filesToCheck.add(linksToCheck);
        var imgsToCheck = thisTable.find('span.check_img');
        filesToCheck = filesToCheck.add(imgsToCheck);

        filesToCheck.each(function (idx, ele) {
          var site = (ele.getAttribute('data-check_site') !== null) ? ele.getAttribute('data-check_site') : "";
          var fileName = (ele.getAttribute('data-check_file') !== null) ? ele.getAttribute('data-check_file') : "";

          if (site === "" || fileName === "") {
            return;
          }

          var server = mxdSiteServerLookup(site);

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

      if (location.search.indexOf('webuser=') >= 0) {
        auth_data = {
          'webuser': mxd_user
        };
      }

      var ajax_opts = {
        type: "GET",
        cache: false,
        url: 'https://' + window.location.hostname + '/' + sandboxString + 'connect/rest.php/authKey',
        datatype: 'json',
        data: auth_data
      };

      ajax_opts.success = function (result) {
        key = result.result.key;
        $.each(filesBySite, function (server, linksToDelete) {
          var dfd = jQuery.Deferred();
          promises.push(dfd);
          var sandboxSite = (sandboxString != '') ? 'site/' : '';
          var url = server + sandboxString + sandboxSite + "files/check_files.php";

          var post_opts = {
            type: 'POST',
            cache: false,
            url: url,
            data: {
              filePaths: Object.keys(linksToDelete)
            },
            crossDomain: true,
            headers: {
              'MXD_OAUTH_USER': mxd_user,
              'MXD_OAUTH_KEY': key
            },
            success: function (data) {
              JSON.parse(data).forEach(function (fileName) {
                // Check if the link exists before you try to delete it
                if (fileName in linksToDelete) {
                  linksToDelete[fileName].forEach(function (span) {
                    var src = server + sandboxString + sandboxSite + "files/view.php?file=" + fileName;
                    var href = (span.getAttribute('data-check_link') !== null) ? span.getAttribute('data-check_link') : src;

                    if ($(span).hasClass('check_link')) {
                      var wrapper = "<a href='" + href + "'></a>";
                      $(span).wrap(wrapper); //Wrap the span in the link (maintains displayed data)
                    } else if ($(span).hasClass('check_img')) {
                      var no_cache = (span.getAttribute('data-no_cache') !== null) ? "&no-cache" : "";
                      var width = (span.getAttribute('data-img_width') !== null) ? "width:" + span.getAttribute('data-img_width') : "max-width:150px";
                      var img = "<a href='" + href + no_cache + "'><img src='" + src + "&thumbnail" + no_cache + "' style='" + width + "'/></a>";
                      $(span).html(img); //Put image and link in the span
                    }
                  });
                  delete linksToDelete[fileName]; //Clear out the list if it's been found
                }
              });

              $.each(linksToDelete, function (fileName, deleteFile) {
                deleteFile.forEach(function (span) {
                  if ($(span).hasClass('check_img')) {
                    $(span).text('Image Not Found');
                  }
                });
              });

              window.setTimeout(function () {
                dfd.resolve();
              }, 3000);
            },
            error: function (e) {
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
  }(jQuery));

  var thisHtmlPage = (location.search.split('html=')[1] || '').split('&')[0];

  var stateSaveCallback = function (settings, data) {
    try {
      (settings.iStateDuration === -1 ? sessionStorage : localStorage).setItem(
        'DataTables_splunk_' + settings.sInstance + '_' + thisHtmlPage,
        JSON.stringify(data)
      );
    } catch (e) {}
  }

  var stateLoadCallback = function (settings) {
    try {
      return JSON.parse(
        (settings.iStateDuration === -1 ? sessionStorage : localStorage).getItem(
          'DataTables_splunk_' + settings.sInstance + '_' + thisHtmlPage
        )
      );
    } catch (e) {}
  }

  function show_hide_column(view_id, element, cols, fields) {

    let table_selector = "#" + view_id + " table";
    let table_pointer = $(table_selector);
    let class_name = element.id + "_col";
    let all_th = $("#" + view_id + " table thead tr th");

    // we have a number column as the first column    
    if ($(all_th[0]).hasClass('row-number')) {
      fields.unshift("");
    }

    for (let col_name of cols) {
      let col_idx = fields.indexOf(col_name);
      let $td = $(`${table_selector} td:nth-child(${col_idx})`);

      all_th.each(function (index, th) {
        if (!$(all_th[index]).data("sort-key")) {
          if ($(all_th[index]).text().trim() === col_name) {
            $(all_th[index]).addClass(class_name);
          }
        } else {
          if ($(all_th[index]).data("sort-key").trim() === col_name) {
            $(all_th[index]).addClass(class_name);
          }
        }
      });

      $td.addClass(class_name);
    }

    console.log(table_pointer.DataTable().columns('.' + class_name));

    table_pointer.DataTable().columns('.' + class_name).visible(element.checked ? true : false, false);
    table_pointer.DataTable().columns.adjust();
    table_pointer.DataTable().fixedColumns().relayout();
    table_pointer.DataTable().scroller.measure();
  }

  function show_hide_row(element, class_name) {
    $('tr.' + class_name).toggle();
  }

  $(function () {
    $('input[name=hide]').each(function (index, value) {
      var id = value.id;
      var val = localStorage.getItem(id);

      if (val !== null && val != value.checked.toString()) {
        $(value).click();
      }
    });

    $('input[name=hide_row]').each(function (index, value) {

      $(value).click();

    });
  });

  function show_hide_save(view_id, element, cols, fields) {
    show_hide_column(view_id, element, cols, fields);
    localStorage.setItem(element.id, element.checked);
  }

  function hide_row_with_value(element, column, value) {
    var remove_rows = element.checked ? true : false;
    $(oTables).each(function () {
      var table = $(this).DataTable();
      console.log("Column: " + column);
      console.log("Value: " + value);

      var table_header = table.columns().header().map(function (header) {
        return $(header).html();
      });

      var column_idx = table_header.indexOf(column);
      var indexes = table.rows().eq(0).filter(function (rowIdx) {
        var cell_data = table.cell(rowIdx, column_idx).data();
        return $(cell_data).text() === value ? true : false;
      });

      console.log("remove rows");

      if (remove_rows) {
        table.rows(indexes)
          .nodes()
          .to$()
          .hide();
      } else {
        table.rows(indexes)
          .nodes()
          .to$()
          .show()
      }
    });
  }

  function updateHeaders() {
    $(oTables).each(function () {
      this.api().draw();
    });
  }

  // global keep track of rendered datatables
  var rendered_datatables = {};

  function setupDataTable(subview, view_id, i, fields) {
    let datatables_token_value_id = default_token_model.get(`datatables#${view_id}`);
    let datatables_token_value_idx = default_token_model.get(`datatables${i + 1}`);

    if (datatables_token_value_id && (!eval(datatables_token_value_id))) {
      set_token(`tbl#${view_id}_display`, true);
      set_token(`tbl${i + 1}_display`, true);
      return;
    } else if (datatables_token_value_idx && (!eval(datatables_token_value_idx))) {
      set_token(`tbl#${view_id}_display`, true);
      set_token(`tbl${i + 1}_display`, true);
      return;
    }

    let table_selector = "#" + subview.id + " table";
    let table_pointer = $(table_selector);


    console.log(`${table_selector} render compelete`);
    console.log(`${table_selector} nested headers complete`);

    // remove Splunk sorting
    var table_headers = $(`${table_selector} th`);
    table_headers.removeClass("sorts");
    $(`${table_selector} th .icon-sorts`).remove();

    table_headers.find("a[href='#']")
      .removeAttr("href")
      .css("cursor", "pointer")
      .css("pointer-events", "none")
      .css("color", "black");

    // remove Splunk paginator and just use Datatables
    $(".splunk-paginator").remove();

    table_pointer.css('width', '100%');

    splunkDatatables(table_selector, subview, view_id, i, fields);
    table_pointer.DataTable().columns.adjust();
    table_pointer.DataTable().fixedColumns().relayout();
    table_pointer.DataTable().scroller.measure();

    rendered_datatables[subview.id] = true;

    set_token(`tbl#${view_id}_display`, true);
    set_token(`tbl${i + 1}_display`, true);


    // extra check to make sure datatables are initialized
    setTimeout(function () {
      if (!$.fn.DataTable.isDataTable(table_selector)) {
        console.log(`init datatables again: ${table_selector}`);
        splunkDatatables(table_selector, subview, view_id, i, fields);
      } else {
        console.log(`${table_selector} table setup`);
      }

    }, 5000);
  }

  function getDatatableConfig(global_config_token, local_id_config_token, local_idx_config_token) {
    // globals take less precendence than specific tables by index and by id
    // and may be overwritten in following code block
    let config_val = undefined;
    if (global_config_token) {
      config_val = eval(global_config_token);
    }

    if (local_idx_config_token) {
      config_val = eval(local_idx_config_token);
    }

    if (local_id_config_token) {
      config_val = eval(local_id_config_token);
    }

    return config_val;
  }

  var dataTablesTriggers = {};
  var oTables = [];
  var oHeader = [];

  function splunkDatatables(table_selector, subview, view_id, i, fields) {
    let splunk_datatable, splunk_datatable_thead_depth;
    let table_pointer = $(table_selector);
    let th_list = [];
    let toggle_column = '<div class="btn-toolbar scrollmenu" role="toolbar"><div id="toggle-columns" class="btn-group btn-group-justified" role="group">';

    $(table_selector + " thead tr th").each(function (index) {
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
    let datatable_search = {
      "regex": true
    };
    let datatable_searching = true;
    let datatable_paging = true;
    let border_targets = [];
    let datatable_fixed_header = true;
    let datatable_fixed_footer = true;
    let datatable_left_fixed_columns = undefined;
    let datatable_right_fixed_columns = undefined;
    let datatable_col_total = false;
    let datatable_col_total_text = false;
    let datatable_col_avg = false;
    let datatable_col_avg_text = false;
    let datatable_rows_group = [];

    // datatable config global
    let datatable_global_row_count_token = default_token_model.get("datatables.config.row_count");
    let datatable_global_search_token = default_token_model.get("datatables.config.search");
    let datatable_global_paging_token = default_token_model.get("datatables.config.paging");
    let datatable_global_borders_token = default_token_model.get("datatables.config.borders");
    let datatable_global_fixed_header_token = default_token_model.get("datatables.config.fixed_header");
    let datatable_global_fixed_footer_token = default_token_model.get(`datatables.config.fixed_footer`);
    let datatable_global_left_fixed_columns_token = default_token_model.get("datatables.config.left_fixed_columns");
    let datatable_global_right_fixed_columns_token = default_token_model.get("datatables.config.right_fixed_columns");
    let datatable_global_callback_token = default_token_model.get("datatables.config.callback");
    let datatable_global_buttons_token = default_token_model.get("datatables.config.buttons");
    let datatable_global_col_total_token = default_token_model.get("datatables.config.col_total");
    let datatable_global_col_total_text_token = default_token_model.get("datatables.config.col_total_text");
    let datatable_global_col_avg_token = default_token_model.get("datatables.config.col_avg");
    let datatable_global_col_avg_text_token = default_token_model.get("datatables.config.col_avg_text");
    let datatable_global_rows_group_token = default_token_model.get("datatables.config.rows_group");

    // datatable config ID specific
    let datatable_id_row_count_token = default_token_model.get(`datatables#${view_id}.config.row_count`);
    let datatable_id_search_token = default_token_model.get(`datatables#${view_id}.config.search`);
    let datatable_id_paging_token = default_token_model.get(`datatables#${view_id}.config.paging`);
    let datatable_id_borders_token = default_token_model.get(`datatables#${view_id}.config.borders`);
    let datatable_id_fixed_header_token = default_token_model.get(`datatables#${view_id}.config.fixed_header`);
    let datatable_id_fixed_footer_token = default_token_model.get(`datatables#${view_id}.config.fixed_footer`);
    let datatable_id_left_fixed_columns_token = default_token_model.get(`datatables#${view_id}.config.left_fixed_columns`);
    let datatable_id_right_fixed_columns_token = default_token_model.get(`datatables#${view_id}.config.right_fixed_columns`);
    let datatable_id_callback_token = default_token_model.get(`datatables#${view_id}.config.callback`);
    let datatable_id_buttons_token = default_token_model.get(`datatables#${view_id}.config.buttons`);
    let datatable_id_columns_toggle_token = default_token_model.get(`datatables#${view_id}.config.columns_toggle`);
    let datatable_id_col_total_token = default_token_model.get(`datatables#${view_id}.config.col_total`);
    let datatable_id_col_total_text_token = default_token_model.get(`datatables#${view_id}.config.col_total_text`);
    let datatable_id_col_avg_token = default_token_model.get(`datatables#${view_id}.config.col_avg`);
    let datatable_id_col_avg_text_token = default_token_model.get(`datatables#${view_id}.config.col_avg_text`);
    let datatable_id_rows_group_token = default_token_model.get(`datatables#${view_id}.config.rows_group`);

    // datatable config index specific
    let datatable_idx_row_count_token = default_token_model.get(`datatables${i + 1}.config.row_count`);
    let datatable_idx_search_token = default_token_model.get(`datatables${i + 1}.config.search`);
    let datatable_idx_paging_token = default_token_model.get(`datatables${i + 1}.config.paging`);
    let datatable_idx_borders_token = default_token_model.get(`datatables${1 + 1}.config.borders`);
    let datatable_idx_fixed_header_token = default_token_model.get(`datatables${i + 1}.config.fixed_header`);
    let datatable_idx_fixed_footer_token = default_token_model.get(`datatables${i + 1}.config.fixed_footer`);
    let datatable_idx_left_fixed_columns_token = default_token_model.get(`datatables${i + 1}.config.left_fixed_columns`);
    let datatable_idx_right_fixed_columns_token = default_token_model.get(`datatables${i + 1}.config.right_fixed_columns`);
    let datatable_idx_callback_token = default_token_model.get(`datatables${i + 1}.config.callback`);
    let datatable_idx_buttons_token = default_token_model.get(`datatables${i + 1}.config.buttons`);
    let datatable_idx_columns_toggle_token = default_token_model.get(`datatables${i + 1}.config.columns_toggle`);
    let datatable_idx_col_total_token = default_token_model.get(`datatables${i + 1}.config.col_total`);
    let datatable_idx_col_total_text_token = default_token_model.get(`datatables${i + 1}.config.col_total_text`);
    let datatable_idx_col_avg_token = default_token_model.get(`datatables${i + 1}.config.col_avg`);
    let datatable_idx_col_avg_text_token = default_token_model.get(`datatables${i + 1}.config.col_avg_text`);
    let datatable_idx_rows_group_token = default_token_model.get(`datatables${i + 1}.config.rows_group`);

    let mail_format = default_token_model.get('mail_format');

    // global configs take less precendence than specific table config
    let datatable_row_count_config = getDatatableConfig(datatable_global_row_count_token, datatable_id_row_count_token, datatable_idx_row_count_token);
    if (datatable_row_count_config) {
      datatable_row_count = datatable_row_count_config;
    }

    let datatable_searching_config = getDatatableConfig(datatable_global_search_token, datatable_id_search_token, datatable_idx_search_token);
    if (!datatable_searching_config) {
      datatable_search = false;
      datatable_searching = false;
    }

    let datatable_paging_config = getDatatableConfig(datatable_global_paging_token, datatable_id_paging_token, datatable_idx_paging_token);
    if (datatable_paging_config === false) {
      datatable_paging = false;
    }

    let datatable_fixed_header_config = getDatatableConfig(datatable_global_fixed_header_token, datatable_id_fixed_header_token, datatable_idx_fixed_header_token);
    if (datatable_fixed_header_config) {
      datatable_fixed_header = datatable_fixed_header_config;
    }

    let datatable_fixed_footer_config = getDatatableConfig(datatable_global_fixed_footer_token, datatable_id_fixed_footer_token, datatable_idx_fixed_footer_token);
    if (datatable_fixed_footer_config) {
      datatable_fixed_footer = datatable_fixed_footer_config;
    }

    let datatable_borders_cols = undefined;
    let datatable_borders_config = getDatatableConfig(datatable_global_borders_token, datatable_id_borders_token, datatable_idx_borders_token)
    if (datatable_borders_config) {
      datatable_borders_cols = datatable_borders_config;
    }
    if (datatable_borders_cols) {
      datatable_borders_cols = String(datatable_borders_cols);
      datatable_borders_cols = datatable_borders_cols.split(",");

      datatable_borders_cols.forEach(function (col) {
        border_targets.push(parseInt(col.trim()));
      });
    }

    let datatable_col_total_config = getDatatableConfig(datatable_global_col_total_token, datatable_id_col_total_token, datatable_idx_col_total_token);
    if (datatable_col_total_config) {
      datatable_col_total = datatable_col_total_config;
    }

    let datatable_col_total_text_config = getDatatableConfig(datatable_global_col_total_text_token, datatable_id_col_total_text_token, datatable_idx_col_total_text_token);
    if (datatable_col_total_text_config) {
      datatable_col_total_text = datatable_col_total_text_config;
    }

    let datatable_col_avg_config = getDatatableConfig(datatable_global_col_avg_token, datatable_id_col_avg_token, datatable_idx_col_avg_token);
    if (datatable_col_avg_config) {
      datatable_col_avg = datatable_col_avg_config;
    }

    let datatable_col_avg_text_config = getDatatableConfig(datatable_global_col_avg_text_token, datatable_id_col_avg_text_token, datatable_idx_col_avg_text_token);
    if (datatable_col_avg_text_config) {
      datatable_col_avg_text = datatable_col_avg_text_config;
    }

    let datatable_rows_group_config = getDatatableConfig(datatable_global_rows_group_token, datatable_id_rows_group_token, datatable_idx_rows_group_token);
    if (datatable_rows_group_config) {
      datatable_rows_group = datatable_rows_group_config;
    }

    let columnDefs = [{
      className: "border_left",
      "targets": border_targets
    }];

    // "stateSaveCallback": stateSaveCallback,
    // "stateLoadCallback": stateLoadCallback,
    //"stateSave": true,
    let table_options = {
      "order": [],
      "scrollY": "800px",
      "scrollX": true,
      "scrollCollapse": true,
      "scroller": true,
      "keys": true,
      "searching": datatable_searching,
      "search": datatable_search,
      "paging": datatable_paging,
      "pagingType": "full_numbers",
      "pageLength": datatable_row_count,
      "lengthMenu": [
        [datatable_row_count, -1],
        [datatable_row_count, "All"]
      ],
      "info": false,
      "fixedHeader": {
        header: datatable_fixed_header,
        footer: datatable_fixed_footer
      },
      "destroy": true,
      "dom": '<"top"f>rt<"bottom"ilp><"clear">',
      "deferRender": true,
      "colReorder": true,
      "buttons": [],
      "select": {
        style: 'multi'
      },
      "drawCallback": function (oSettings) {
        if (oSettings.aaSorting.length > 0) { // Check if sorting a column
          $.each(oSettings.aoData, un_fake_rowspan);
        }

        setTimeout(function () {
          if (splunk_datatable.data()) {
            if (splunk_datatable.data().count() > datatable_row_count) {
              if ($("#temp_remove_length_paginate").length) {
                $('#temp_remove_length_paginate').remove();
              }
            } else {
              hide_length_paginate();
            }
          }
        }, 500);
      },      
      columnDefs      
    };

    if (datatable_rows_group.length > 0) {
      table_options.rowsGroup = datatable_rows_group;
      table_options.deferRender = false;
      table_options.scroller = false;
    }

    table_options.footerCallback = function (row, data, start, end, display) {
      let api = this.api();

      /* This function checks checks whether the given value is a string and 
       * converts to a valid number if possible else returns 0.
       * If the value is a valid number then the number is returned, else
       * returns 0. 
       */
      let intVal = function (i) {
        return typeof i === 'string' ?
          (isNaN(i.replace(/[\$,]/g, '') * 1) ? 0 : i.replace(/[\$,]/g, '') * 1) :
          typeof i === 'number' ?
          i : 0;
      };

      /* This function checks if the given value is a string and fetches the
       * index of the string in the fields array. If the value is a number,
       * the number is simply returned.
       */
      let colIdx = function (i) {
        return typeof i === 'string' ? fields.indexOf(i) :
          typeof i === 'number' ? i : 0;
      };

      let colTotal = function (col) {
        return api.column(col, {
            filter: "applied"
          })
          .data()
          .reduce(function (a, b) {
            if ($(b).text())
              b = $(b).text();
            return intVal(a) + intVal(b);
          }, 0);
      }

      if (datatable_col_total) {
        datatable_col_total.forEach(function (col) {
          col = colIdx(col);
          // Remove the formatting to get integer data for summation          
          if (col > -1) {
            // Total over page with filters
            let total = colTotal(col);

            // Update footer
            $(api.column(col).footer()).html(
              total
            );
          }
        });

        const total_footer_text = (datatable_col_total_text === true) ? "Total " : datatable_col_total_text;
        $(api.column(0).footer()).html(total_footer_text);
        $(api.column(0).footer()).addClass("total_footer");
      }

      if (datatable_col_avg) {
        datatable_col_avg.forEach(function (col) {
          col = colIdx(col);

          if (col > -1) {
            let data_length = api.column(col, {
                filter: "applied"
              })
              .data().flatten().toArray().filter(val => val !== "NA" && val !== "N/A").length;

            let total = colTotal(col);

            let avg = (total / data_length).toFixed(2);

            // Update footer
            $(api.column(col).footer()).html(
              avg
            );
          }
        });

        const avg_footer_text = (datatable_col_avg_text === true) ? "Average " : datatable_col_avg_text;
        $(api.column(0).footer()).html(avg_footer_text);          
        $(api.column(0).footer()).addClass("avg_footer");
      }
    }

    let datatable_left_fixed_columns_config = getDatatableConfig(datatable_global_left_fixed_columns_token, datatable_id_left_fixed_columns_token, datatable_idx_left_fixed_columns_token);
    if (datatable_left_fixed_columns_config) {
      datatable_left_fixed_columns = datatable_left_fixed_columns_config;
    }
      
    if (mail_format) {
      delete table_options.fixedHeader;
      delete table_options.scrollY;
      delete table_options.scrollX;      
      delete table_options.scrollCollapse;
    }
 
    if (datatable_left_fixed_columns && !mail_format) {
      if (!table_options.fixedColumns) {
        table_options.fixedColumns = {};
      }

      table_options.fixedColumns.leftColumns = datatable_left_fixed_columns;
    }

    let datatable_right_fixed_columns_config = getDatatableConfig(datatable_global_right_fixed_columns_token, datatable_id_right_fixed_columns_token, datatable_idx_right_fixed_columns_token);
    if (datatable_right_fixed_columns_config) {
      datatable_right_fixed_columns = datatable_right_fixed_columns_config;
    }

    if (datatable_right_fixed_columns) {
      if (!table_options.fixedColumns) {
        table_options.fixedColumns = {};
      }

      table_options.fixedColumns.rightColumns = datatable_right_fixed_columns;
    }

    let datatable_buttons_config = getDatatableConfig(datatable_global_buttons_token, datatable_id_buttons_token, datatable_idx_buttons_token);
    if (datatable_buttons_config) {
      table_options.dom = '<"top"Bfl>rt<"bottom"piT>';
      table_options.buttons = ["copyHtml5", "csvHtml5"];

      table_options.buttons.push({
        extend: 'excelHtml5',
        autoFilter: true,
        createEmptyCells: true,
        // Map HTML colors to Excel
        customize: function (xlsx) {
          let sheet = xlsx.xl.worksheets['sheet1.xml'];
          let styles = xlsx.xl['styles.xml'];
          let namespace = styles.lookupNamespaceURI(null);
          // Get dom objects with [0]
          let fills = $('fills', styles)[0];
          let cellXfs = $('cellXfs', styles)[0];
          let create_excel_color_style = function (hex) {
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
          let lt_green_style = 40;
          let let_red_style = 35;
          let lt_blue_style = 46;
          let lt_yellow_style = cellXfCount;
          let lt_greenyellow_style = cellXfCount + 1;
          let lt_orange_style = cellXfCount + 2;
          let lt_purple_style = cellXfCount + 3;
          let light_style = 31;

          // Increase count by 4 for the 4 new styles above
          $('cellXfs', styles).attr('count', cellXfCount + 4);

          const excel_color_mapping = {
            'lt_green': lt_green_style,
            'lt_red': let_red_style,
            'lt_blue': lt_blue_style,
            'lt_yellow': lt_yellow_style,
            'lt_greenyellow': lt_greenyellow_style,
            'lt_orange': lt_orange_style,
            'lt_purple': lt_purple_style,
            'light': light_style
          };

          splunk_datatable.rows().every(function (row_idx, table_loop, row_loop) {
            let $row = $(this.node());
            $row.find('td').each(function (idx) {
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

      table_options.buttons.push({
        extend: 'pdfHtml5',
        orientation: 'landscape',
        pageSize: 'A3',
        customize: function (doc) {
          // Make text size smaller to fit on page
          doc.styles.tableHeader.fontSize = 8;
          doc.defaultStyle.fontSize = 8;
          doc.styles.tableFooter.fontSize = 8;
        }
      });

      table_options.buttons.push({
        extend: 'colvis',
        columnText: function (dt, idx, title) {
          let header = dt.column(idx).header();
          return $(header).data("sort-key");
        }
      });

      table_options.buttons.push("colvisRestore");
    }

    if (!datatable_buttons_config) {
      table_options.dom = '<"top"f>rt<"bottom"ilp><"clear">';
      table_options.buttons = [];
    }


    if (datatable_col_total || datatable_col_avg) {
      let fields_length = fields.length;
      let tfoot_tr_html = '<tr>';

      for (let i = 0; i < fields_length; i++) {
        tfoot_tr_html += '<th></th>';
      }

      let all_th = $(`${table_selector} thead tr th`);

      if ($(all_th[0]).hasClass('row-number')) {
        tfoot_tr_html += '<th></th>';
      }

      tfoot_tr_html += '</tr>';

      table_pointer.append(
        $('<tfoot/>').append($(tfoot_tr_html))
      );

      // clear out all headers we don't want the text again    
      $(`${table_selector} tfoot tr th`).html("");

      // remove footer borders
      $(`${table_selector} tfoot tr th`).css("border", "none");
      // only have border on top of footer
      $(`${table_selector} tfoot tr th`).css("border-top", "2px solid black");
    }

    table_pointer.addClass("stripe");
    // built-in datatables class to remove excess padding and spaces
    table_pointer.addClass("compact");
    table_pointer.addClass("cell-border");
    table_pointer.addClass("hover");

    let display = {};
    // display.sql_host = "scv-mxdsql-replica1";
    // display.sql_db = "mxd_11_3";
    // display.double_click = 1;
    // table_pointer.dataTable().ConvertRowsToLinks("&sql_host=" + display.sql_host + "&sql_db=" + display.sql_db, "https://mxd.csg.apple.com/scripts/show_context.php", display.double_click);

    // Datatables object created
    splunk_datatable = table_pointer.DataTable(table_options);

    let $thead = $(splunk_datatable.table().header());
    splunk_datatable_thead_depth = $thead.find('tr').length;

    // Click on drilldowns when enter pressed
    splunk_datatable.on('key', function (e, datatable, key, cell, originalEvent) {
      if (key === 13) {
        cell.node().click();
      }
    });

    oTables[i] = splunk_datatable;

    // row selection
    $(table_selector + " tbody").on('click', 'tr', function () {
      $(this).toggleClass('selected');
    });

    $(table_selector + " tbody").on('click', 'tr', function () {
      if ($(this).hasClass('selected')) {
        $(this).removeClass('selected');
      } else {
        splunk_datatable.$('tr.selected').removeClass('selected');
        $(this).addClass('selected');
      }
    });

    $(table_selector + '_delete_button').click(function () {
      splunk_datatable.row('.selected').remove().draw(false);
    });

    if (datatable_global_callback_token && !datatable_idx_callback_token) {
      datatable_callback = eval(datatable_global_callback_token);
      eval(datatable_callback + "('" + table_selector + "')");
    }

    if (datatable_id_callback_token) {
      datatable_callback = eval(datatable_id_callback_token);
      eval(datatable_callback + "('" + table_selector + "')");
    }

    if (datatable_idx_callback_token) {
      datatable_callback = eval(datatable_idx_callback_token);
      eval(datatable_callback + "('" + table_selector + "')");
    }

    if (typeof (dataTablesTriggers['init']) === 'function')
      dataTablesTriggers['init'](table_pointer);

    if (typeof (createEditableFields) === 'function') {
      createEditableFields(table_pointer);
    }

    if (typeof (dataTablesTriggers['afterRender']) === 'function') {
      dataTablesTriggers['afterRender'](table_pointer);
    }

    table_pointer.DataTable().columns('.splunk_hide_me').visible(false, false);

    table_pointer.dataTable().fnFakeRowspan();

    if (typeof (dataTablesTriggers['afterShow']) === 'function') {
      dataTablesTriggers['afterShow'](table_pointer);
    }

    $('button.toggle-vis').on('click', function (e) {
      e.preventDefault();

      // Get the column API object
      var column = splunk_datatable.column($(this).attr('data-column'));

      // Toggle the visibility
      column.visible(!column.visible());

      $(this).toggleClass('active', function (index, current_class) {
        if (current_class === 'active') {
          return 'disabled'
        } else {
          return 'active'
        }
      });
    });

    // Adjust header width for initially hidden tables on bootstrap tab click
    // Datatables headers comes out deformed because it doesn't initially know width
    $('a.toggle-tab').on('click shown', function (e) {
      $.fn.dataTable.tables({
        visible: true,
        api: true
      }).columns.adjust();
      $.fn.dataTable.tables({
        visible: true,
        api: true
      }).fixedColumns().relayout();
      $.fn.dataTable.tables({
        visible: true,
        api: true
      }).scroller.measure();

      // ensure width is 100%
      table_pointer.css('width', '100%');
    });
  }

  return {
    setupDataTable,
    show_hide_save,
    show_hide_column
  };

});
