////////////////////////////////////////////////////////////
//
// DataTables JS function to get DataTables to work with MXD
//
////////////////////////////////////////////////////////////

// Make sure searches only occur after return is hit
jQuery.fn.dataTableExt.oApi.fnFilterOnReturn = function (oSettings) {
    var _that = this;
  
    this.each(function (i) {
        $.fn.dataTableExt.iApiIndex = i;
        var $this = this;
        var anControl = $('input', _that.fnSettings().aanFeatures.f);
        anControl.unbind('keyup').bind('keypress', function (e) {
            if (e.which == 13) {
                $.fn.dataTableExt.iApiIndex = i;
                _that.fnFilter(anControl.val());
            }
        });
        return this;
    });
    return this;
};

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
} );

// Create auto detect of numbers inside of HTML
// Put this at the beginning of the check-array
var mxd_default_sort_datatypes = function ( sData ) {
                          /* Allow zero length strings as a number */
                          if ( typeof sData === 'number' )
                          {
                                  return 'html-numeric';
                          }
                          else if ( typeof sData !== 'string' )
                          {
                                  return null;
                          }
                          else if (sData === 'NA')  /* MXD NULL */
                          {
                            return 'html-numeric';
                          }
              
                          var sData = String(sData).replace( /<[\s\S]*?>/g, "" );
                          
                          var sValidFirstChars = "0123456789-";
                          var sValidChars = "0123456789.";
                          var Char;
                          var bDecimal = false;
                          
                          /* Check for a valid first char (no period and allow negatives) */
                          Char = sData.charAt(0); 
                          if (sValidFirstChars.indexOf(Char) == -1) 
                          {
                                  return null;
                          }
                          
                          /* Check all the other characters are valid */
                          for ( var i=1 ; i<sData.length ; i++ ) 
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

jQuery.fn.dataTableExt.aTypes.unshift( mxd_default_sort_datatypes );

// Object for (un_)fake_rowspan functions
// - holds indexes of the previous row's rowspans
//   that are also max_rowspans for that row
// - means no need to un_fake the rowspan when sorting
prev_rowspan = {
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

function MXDDrawCallback( oSettings ) {
  if (oSettings.aaSorting.length >0) { // Check if sorting a column
    $.each(oSettings.aoData, un_fake_rowspan);
  }
}

// JQuery extensions  for proper rendering of links in datatables; Run prior to DT rendering

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
      'DataTables_mxd2_'+settings.sInstance+'_'+thisHtmlPage,
        JSON.stringify( data )
    );
  } catch (e) {}
}

var stateLoadCallback = function ( settings ) {
  try {
    return JSON.parse(
      (settings.iStateDuration === -1 ? sessionStorage : localStorage).getItem(
        'DataTables_mxd2_'+settings.sInstance+'_'+thisHtmlPage
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



if (typeof(dataTablesTriggers) === 'undefined'){ dataTablesTriggers = {} };
if (typeof(oTables) === 'undefined'){ oTables = [] };
if (typeof(oHeader) === 'undefined'){ oHeader = [] };
function mxdDatatables(table_index, display) {
  var dataTableOpts = {
    "paginate"     : false, 
    "filter"       : false, 
    "info"         : false,
    "sort"         : true, 
    "fixedColumns" : false,
    "colReorder"   : false,
    "buttons"      : [],
    "stateSave"    : false,
    "columnsToggle"    : false,
    "dom"          : '<"top"fl>rt<"bottom"piT>',

    "order"     : [], 
    "deferRender"  : true, 
    "fnDrawCallback": MXDDrawCallback,
    "stateSaveCallback": stateSaveCallback,
    "stateLoadCallback": stateLoadCallback,
  };

  if (display['no_auto_width']) {
    dataTableOpts.autoWidth = false;
  }

  if (display['left_sticky']) {
    if (dataTableOpts.fixedColumns == false)
      dataTableOpts.fixedColumns = {};
    dataTableOpts.scrollX = true;
    dataTableOpts.scrollCollapse = true;
    dataTableOpts.fixedColumns.leftColumns = display['left_sticky'];
  }

  if (display['right_sticky']) {
    if (dataTableOpts.fixedColumns == false)
      dataTableOpts.fixedColumns = {};
    dataTableOpts.scrollX = true;
    dataTableOpts.scrollCollapse = true;
    dataTableOpts.fixedColumns.rightColumns = display['right_sticky'];
  }

  if (display['fixed_header']) {
    dataTableOpts.fixedHeader = true;
  }

  if (display['colReorder']) {
    dataTableOpts.colReorder = true;
  }

  if (display['stateSave']) {
    dataTableOpts.stateSave = true;
    // <khusid.020917.01> save table state indefinitely, RT#373978
    dataTableOpts.stateDuration = 0;
  }

  if (display['scrollbar']) {
    dataTableOpts.scrollY = "80vh";
    dataTableOpts.scrollCollapse = true;
  } else if (display['paginate']) {
    delete dataTableOpts.paginate;
    dataTableOpts.info = true;
    dataTableOpts.lengthMenu = [ [10, 25, 50, -1], [10, 25, 50, "All"] ];
  }


  if (display['buttons']) {
    dataTableOpts.dom   = '<"top"fl>rt<"bottom"piTB>';
    dataTableOpts.buttons = ["copyHtml5", "csvHtml5", "excelHtml5", "print"];
    if (display['columnsToggle']) {
       dataTableOpts.buttons.push("columnsToggle"); 
    }
  }

  if (display['search']) {
    delete dataTableOpts.filter;
    dataTableOpts.info = true;
  }

  if (display['regex']) {
    dataTableOpts.search = {"regex":true};
  }
  
  var table_id = 'tbl' + table_index;
  var table_pointer = $('#' + table_id);
  table_pointer.addClass("stripe");
  table_pointer.addClass("compact");
  table_pointer.addClass("cell-border");
  table_pointer.addClass("hover");

  try {
    var promises = [];
    if (!display['noCheckLinks']) {
      promises = table_pointer.checkLinks();
    }

    if (typeof(dataTablesTriggers['init']) === 'function')
      dataTablesTriggers['init'](table_pointer);

    if( typeof(createEditableFields) === 'function' ) {
      createEditableFields(table_pointer);
    }

    table_pointer.ConvertRowsToLinks("&sql_host=" + display.sql_host + "&sql_db=" + display.sql_db, "../scripts/show_context.php", display.double_click);
    table_pointer.show();
    oTables[table_index] = table_pointer.dataTable(dataTableOpts);

    if (typeof(dataTablesTriggers['afterRender']) === 'function')
      dataTablesTriggers['afterRender'](table_pointer);

    if (display['search'])
      table_pointer.fnFilterOnReturn();

    if (display['callback'])
      eval(display['callback'] + "('" + table_id + "')");

    table_pointer.DataTable().columns('.mxd_hide_me').visible(false, false);
    table_pointer.fnFakeRowspan();

    if (typeof(dataTablesTriggers['afterShow']) === 'function') {
      dataTablesTriggers['afterShow'](table_pointer);
    }

    promises.forEach(function (promise) {
      console.log(promise.state());
    });
    $.when.apply($, promises).then(function() {
      table_pointer.api().draw();
      promises.forEach(function (promise) {
        console.log(promise.state());
      });
    });
  }
  catch(err) {
    console.log(err)
    table_pointer.show();
  };
}
