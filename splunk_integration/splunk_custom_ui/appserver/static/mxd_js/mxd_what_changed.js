/**
 * Highlight 'what_changed' elements in a table
 *  Backwards Compatible function
 */
var highlight_what_changed = function () {
  WC.overlay_toggle();
};

/**
 * Generate the what_changed buttons inside a div element
 */
$(document).ready(function(){
  var cur_table_mode = WC.get_table_mode();

  var wc_mode_items = WC.get_modes().map(function(mode){
    var mode_name = WC.get_mode_name(mode);
    var selected = (mode === cur_table_mode) ? 'glyphicon-ok' : 'glyphicon-none';
    return '<li><a href="javascript:void(0);" onclick="WC.set_table_mode(\''+mode+'\'); WC.table_on()"><span class="glyphicon '+selected+'"></span>'+mode_name+'</a></li>';
  });
  var wc_mode_list = '<ul class="dropdown-menu">' + wc_mode_items.join('') + '</ul>';

  var wc_interface_html = '<div class="btn-group">'
                        + '  <button id="mxd-wc-button-overlay"'
                        + '          class="btn btn-sm btn-default"'
                        + '          type="button"'
                        + '          title="Toggle What Changed? Overlay">'
                        + '    <span class="glyphicon glyphicon-eye-open">'
                        + '    </span>'
                        + '  </button>'
                        + '  <button id="mxd-wc-button-table"'
                        + '          class="btn btn-sm btn-default"'  
                        + '          type="button"'
                        + '          title="Toggle Comparison Table">'
                        + '    <span class="glyphicon glyphicon-object-align-left">'
                        + '    </span>'
                        + '  </button>'
                        + '  <button class="btn btn-sm btn-default dropdown-toggle"'
                        + '          data-toggle="dropdown"'
                        + '          type="button">'
                        + '    <span class="glyphicon glyphicon-option-vertical">'
                        + '    </span>'
                        + '  </button>'
                        + wc_mode_list;
                        + '</div>'

  $('#mxd-wc-toolbar').html(wc_interface_html);
  $('#mxd-wc-toolbar .dropdown-toggle').dropdown();

  $('#mxd-wc-button-overlay').on('click', function(e){
    WC.overlay_toggle();
  });
  $('#mxd-wc-button-table').on('click', function(e){
    WC.table_toggle();
  });
  $('#mxd-wc-toolbar .dropdown-menu li a').click(function(){
    var old_selected = $(this).closest('.dropdown-menu').find('.glyphicon-ok');
    old_selected.removeClass('glyphicon-ok');
    old_selected.addClass('glyphicon-none');

    var new_selected = $(this).find('.glyphicon-none');
    new_selected.removeClass('glyphicon-none');
    new_selected.addClass('glyphicon-ok');
  });
});

var WC = WC || {};

/**
 * Toggle the comparison overlay on/off
 */
WC.overlay_toggle = function() {
  if (WC._overlay_state === 'on') {
    WC.overlay_off();
  } else {
    WC.overlay_on();
  }
};

/**
 * Turn off comparison overlay
 *  remove classes from table cells that affect styling
 *  disable comparison popover
 */
WC.overlay_off = function() {
  WC._overlay_state = 'off';
  $('#mxd-wc-button-overlay').removeClass('active');
  $('table.dataTable tbody td').each(function(i, el) {
    if (WC._is_wc_column($(el))) {
      WC._del_popover($(el));
      $(el).removeClass('mxd_wc_greater mxd_wc_less mxd_wc_equal mxd_wc_null_to_value mxd_wc_value_to_null mxd_wc_string');
    }
  });
};

/**
 * Turn on comparison overlay
 *  add classes table cells to affect styling
 *  enable comparison popover
 */
WC.overlay_on = function() {
  WC._overlay_state = 'on';
  $('#mxd-wc-button-overlay').addClass('active');
  $('table.dataTable tbody td').each(function(i, el) {
    if (WC._is_wc_column($(el))) {
      var popover = 'insufficient data to compare';
      if (WC._has_wc_data($(el))) {
        var is_flipped = ($(el).hasClass('greater_better')) ? 1 : 0;
        var old_data = $(el).data('whatchanged').toString().split('-|,|-');
            old_data = old_data.slice(-1)[0]; // get the last element
        var cur_data = $(el).attr('wc-cur-text') || $(el).text();

        var tolerance = $(el).attr('wc-tolerance') || null;
        var threshold = $(el).attr('wc-threshold') || null;

        
        $(el).addClass(WC._get_class(cur_data, old_data, threshold, tolerance));
        popover = WC._build_popover(cur_data, old_data, is_flipped);
      }
      WC._add_popover(el, popover);
    }
  });
};

/**
 * Toggle the comparison table on/off
 */
WC.table_toggle = function() {
  if (WC._table_state === 'on') {
    WC.table_off();
  } else {
    WC.table_on(WC._table_mode);
  }
};

/**
 * Turn on comparison table
 *  replace table cells with result of comparison function
 *  update internal dataTable state
 */
WC.table_on = function() {
  WC._table_state = 'on';
  $('#mxd-wc-button-table').addClass('active');
  $('table.dataTable tbody td').each(function(i, el) {
    if ($(el).hasClass('mxd_wc_col')) {
      var popover = 'insufficient data to compare';
      var new_text = '';
      var cur_text_attr = $(el).attr('wc-cur-text');
      if (typeof cur_text_attr === typeof undefined || cur_text_attr === false) {
        $(el).attr('wc-cur-text', $(el).text());
        $(el).attr('wc-cur-html', $(el).html());
      }

      var cur_data = $(el).attr('wc-cur-text') || $(el).text();

      if (WC._has_wc_data(el)) {
        var is_flipped = ($(el).hasClass('greater_better')) ? 1 : 0;
        var old_data = $(el).data('whatchanged').toString().split('-|,|-');
            old_data = old_data.slice(-1)[0]; // get the last element

        new_text = WC._compare_data(cur_data, old_data, WC._table_mode);
        popover  = WC._build_popover(cur_data, old_data, is_flipped);
      } else {
        $(el).addClass('mxd_wc_equal');
      }
      $(el).text(new_text);
      WC._add_popover(el, popover);
    }
  });
  jQuery.fn.dataTableExt.aTypes.unshift(function(){ return 'html-wc'; });
  jQuery.extend(jQuery.fn.dataTableExt.oSort, { 'html-wc-pre' : WC._compare_mode_lut[WC._table_mode].sort_op });
  $('table.dataTable').DataTable().rows().invalidate().draw();
};

/**
 * Turn off comparison table
 *  restore original table content
 *  update internal dataTable state
 */
WC.table_off = function() {
  WC._table_state = 'off';
  $('#mxd-wc-button-table').removeClass('active');
  $('table.dataTable tbody td').each(function(i, el) {
    if (WC._is_wc_column(el)) {
      var restore_html = $(el).attr('wc-cur-html');
      $(el).html(restore_html);

      WC._del_popover(el);
      if (!WC._has_wc_data(el)) {
        $(el).removeClass('mxd_wc_equal');
      }
    }
  });
  jQuery.fn.dataTableExt.aTypes.unshift(mxd_default_sort_datatypes);
  $('table.dataTable').DataTable().rows().invalidate().draw();
};

/**
 * Set the mode to use when comparison table enabled
 *
 * @param {string} comparison mode
 */
WC.set_table_mode = function(mode) {
  if (!mode in WC._compare_mode_lut) {
    console.error('Mode: '+mode+' not defined');
    return;
  }

  WC._table_mode = mode;
};

/**
 * Get the mode to use when comparison table enabled
 *
 * @returns {string} current table compare mode
 */
WC.get_table_mode = function() {
  return WC._table_mode;
};

/**
 * Get all comparison modes
 *
 * @returns {stringArray} list of all modes
 */
WC.get_modes = function() {
  var rvalue = Object.keys(WC._compare_mode_lut).sort();
  return rvalue;
};

/**
 * Get all comparison modes
 *
 * @returns {stringArray} list of all modes
 */
WC.get_mode_name = function(mode) {
  var rvalue;
  if (!mode in WC._compare_mode_lut) {
    console.error('Mode: '+mode+' not defined');
    return;
  }

  rvalue =  WC._compare_mode_lut[mode].name;
  return rvalue;
};

/** --------------------------------------------------------------------------------- **/
WC._table_mode    = 'pcnt';
WC._overlay_state = 'off';
WC._table_state   = 'off';
WC._sort_order_smallest = -9007199254740992; // -2^64 - smallest number possible

WC._sort_order_lut = {
  'divBy0'  : function() { return WC._sort_order_smallest + 5; },
  'missing' : function() { return WC._sort_order_smallest + 4; },
  'new'     : function() { return WC._sort_order_smallest + 3; },
  'same'    : function() { return WC._sort_order_smallest + 2; },
  'diff'    : function() { return WC._sort_order_smallest + 1; },
  'NA'      : function() { return WC._sort_order_smallest;     },
  ''        : function() { return WC._sort_order_smallest;     }
};

WC._compare_string_lut = {
  'pass'    : function() { return 'mxd_wc_less';          },
  'fail'    : function() { return 'mxd_wc_greater';       },
  'missing' : function() { return 'mxd_wc_value_to_null'; },
  'new'     : function() { return 'mxd_wc_null_to_value'; },
  'same'    : function() { return 'mxd_wc_equal';         },
  'diff'    : function() { return 'mxd_wc_string';        }
};

WC._compare_mode_lut = {
  'pcnt'    : { 'name'    : 'Percent',
                'cmp_op'  : function(a, b) { return WC._mode_pcnt(a, b) + '%'; },
                'sort_op' : function(a) {
                              var rvalue;
                              if (a.substr(-1,1) === '%') { a = a.slice(0,-1); }
                              if (a in WC._sort_order_lut) {
                                rvalue = WC._sort_order_lut[a]();
                              } else {
                                rvalue = parseFloat(a);
                              }
                              return rvalue;
                            }
              },
  'real'    : { 'name'    : 'Absolute',
                'cmp_op'  : function(a, b) { return WC._mode_real(a, b); },
                'sort_op' : function(a) {
                              var rvalue;
                              if (a in WC._sort_order_lut) {
                                rvalue = WC._sort_order_lut[a]();
                              } else {
                                rvalue = parseFloat(a);
                              }
                              return rvalue;
                            }
              }
};

/**
 * Tests whether Bootstrap is enabled
 *
 * @returns {boolean} Does Bootstrap functionality exist?
 */
WC._is_bootstrap_enabled = function() {
  return (typeof($.fn.popover) === 'function');
};

/**
 * Tests if DOM element is in a what_changed column
 *
 * @param {domElement} DOM element
 * @returns {boolean} Is DOM element in a what_changed column?
 */
WC._is_wc_column = function(el) {
  return $(el).hasClass('mxd_wc_col');
}

/**
 * Tests if DOM element has what_changed data
 *
 * @param {domElement} DOM element
 * @returns {boolean} Does DOM element have what_changed data?
 */
WC._has_wc_data = function(el) {
  return $(el).hasClass('mxd_wc_data');
}

/**
 * Sets up Bootstrap Tooltip
 *
 * @param {domElement} DOM element
 * @param {string} Information to be displayed on popover (can be HTML-rich)
 */
WC._add_popover = function(el, popover) {
  if(!WC._is_bootstrap_enabled()) {
    return;
  }

  $(el).attr('data-toggle','popover');
  $(el).popover({
    container : 'body',
    html      : 'true',
    placement : 'auto right',
    title     : 'What Changed?',
    content   : popover,
    trigger   : 'hover',
  });
};

/**
 * Removes Bootstrap Tooltip
 *
 * @param {domElement} DOM element
 */
WC._del_popover = function(el) {
  if(!WC._is_bootstrap_enabled()) {
    return;
  }

  if (WC._table_state === 'off' && WC._overlay_state === 'off') {
    $(el).popover('destroy');
  }
};

/**
 * Return style class(es) based on comparison of data
 *
 * @param {domElement} DOM element
 * @param {string} Current data value
 * @param {string} Previous data value
 * @param {string} Value a comparison must exceed before marking as a high-priority change
 * @param {string} Value a comparison must exceed before marking as different
 * @returns {string} Space delimited list of style classes to apply to element
 */
WC._get_class = function(cur_data, old_data, threshold, tolerance) {
  var rvalue;

  cmp_real = WC._compare_data(cur_data, old_data, 'real');
  cmp_pcnt = WC._compare_data(cur_data, old_data, 'pcnt');

  if ($.isNumeric(cmp_real)) {
    var cmp_real_float  = parseFloat(cmp_real);
    var cmp_pcnt_float  = parseFloat(cmp_pcnt);
    var threshold_float = parseFloat(threshold);
    var tolerance_float = parseFloat(tolerance);
    var tolerance_cmp   = (tolerance !== null && tolerance.substr(-1,1) === '%') ? cmp_pcnt_float : cmp_real_float;
    var threshold_cmp   = (tolerance !== null && threshold.substr(-1,1) === '%') ? cmp_pcnt_float : cmp_real_float;

    if (cmp_real_float === 0) {
      rvalue = 'mxd_wc_equal';
    } else if ($.isNumeric(tolerance_float) &&  Math.abs(tolerance_cmp) < tolerance_float) {
      rvalue = 'mxd_wc_equal';
    } else if (cmp_real_float > 0) {
      rvalue = 'mxd_wc_greater';
    } else if (cmp_real_float < 0) {
      rvalue = 'mxd_wc_less';
    } else {
      rvalue = '';
    }
    if ($.isNumeric(threshold_float) && Math.abs(threshold_cmp) >= threshold_float) {
      rvalue = rvalue + ' ' + 'mxd_wc_alert';
    }
  } else {
    rvalue = WC._compare_string_lut[cmp_real]();
  }

  return rvalue;
};

/**
 * Build popover display
 *
 * @param {string} new data
 * @param {string} old data
 * @param {boolean} Is new < old bad?
 * @returns {domString} HTML string containing information to display on popover
 */
WC._build_popover = function(cur_data, old_data, is_flipped) {
  var new_value_row = '<tr><td style="text-align:right;">New Value:</td><td>'+cur_data+'</td></tr>';
  var old_value_row = '<tr><td style="text-align:right;">Old Value:</td><td>'+old_data+'</td></tr>';
  var msg_value_row = [];

  if ($.isNumeric(cur_data) && $.isNumeric(old_data)) {
    msg_value_row = WC.get_modes().map(function(mode){
      var cmp = WC._compare_data(cur_data, old_data, mode);
      return '<tr><td style="text-align:right;">'+WC._compare_mode_lut[mode].name+':</td><td>'+cmp+'</td></tr>';
    });

    var arrow_class = '';
    var color_class = '';

    var cur_f = parseFloat(cur_data);
    var old_f = parseFloat(old_data);

    if (cur_f < old_f) {
      arrow_class = 'glyphicon-triangle-bottom';
      color_class = is_flipped ? 'mxd_wc_negative' : 'mxd_wc_positive';
    } else if (cur_f > old_f) {
      arrow_class = 'glyphicon-triangle-top';
      color_class = is_flipped ? 'mxd_wc_positive' : 'mxd_wc_negative';
    }

    if (arrow_class) {
      var indicator = ' <span class="mxd_wc_arrow glyphicon '+arrow_class+' '+color_class+'"></span>';
      msg_value_row.push('<tr><td></td><td>'+indicator+'</td></tr>');
    }
  }

  if (cur_data === null || cur_data.toUpperCase() === 'NA') { cur_data = 'missing'; }
  if (old_data === null || old_data.toUpperCase() === 'NA') { old_data = 'missing'; }

  var rvalue = '<table class="compact">' + new_value_row + old_value_row + msg_value_row.join('') + '</table>';
  return rvalue;
};

/**
 * Compare data wrapper
 *
 * @param {string} current data value
 * @param {string} old data value
 * @param {string} comparison mode to utilize
 * @returns {string} result of comparison
 */
WC._compare_data = function(cur_data, old_data, mode) {
  var rvalue;

  if ($.isNumeric(cur_data) && $.isNumeric(old_data)) {
    rvalue = WC._compare_mode_lut[mode].cmp_op(cur_data, old_data);
  } else {
    if (cur_data === old_data) {
      rvalue = 'same';
    } else if (old_data === null || old_data.toUpperCase() === 'NA') {
      rvalue = 'new';
    } else if (cur_data === null || cur_data.toUpperCase() === 'NA') {
      rvalue = 'missing';
    } else if (cur_data.toUpperCase() === 'FAIL') {
      rvalue = 'fail';
    } else if (cur_data.toUpperCase() === 'PASS') {
      rvalue = 'pass';
    } else {
      rvalue = 'diff';
    }
  }

  return rvalue;
};

/**
 * Calculate the percent change between two values
 *
 * @param {number} new value
 * @param {number} old value
 * @returns {number} percent change between old and new
 */
WC._mode_pcnt = function(a, b) {
  var a_f = parseFloat(a);
  var b_f = parseFloat(b);
  var rvalue;

  if (b_f === 0) {
    rvalue = 'divBy0';
  } else {
    rvalue = (100 * (a_f - b_f) / b_f).toFixed(2);
  }

  return rvalue;
};

/**
 * Calculate the difference between two values
 *
 * @param {number} new value
 * @param {number} old value
 * @returns {number} difference between old and new
 */
WC._mode_real = function(a, b) {
  var a_f = parseFloat(a);
  var b_f = parseFloat(b);
  var rvalue  = (a_f - b_f).toFixed(2);

  return rvalue;
};

