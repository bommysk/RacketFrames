/**
 * Generate elements for viewing links within inset page modal
 **/
$(document).ready(function() {
  var style = '<!-- Style for MXD Lightbox -->'
            + '<style>'
            + '  #mxlb_modal .modal-header {'
            + '    height: 45px;'
            + '    padding: 5px;'
            + '  }'
            + '  #mxlb_modal .modal-body {'
            + '    height: calc(100% - 45px);'
            + '    padding: 10px;'
            + '  }'
            + '  #mxlb_overlay {'
            + '    height: calc(100% - 20px);'
            + '    width: calc(100% - 50px);'
            + '    position: absolute;'
            + '    padding: 10px;'
            + '    background: rgba(0, 0, 0, 0.4);'
            + '    top: 10px;'
            + '    left: 25px;'
            + '  }'
            + '</style>';

  var modal = '<!-- Modal for MXD Lightbox -->'
            + '<div id="mxlb_modal" class="modal fade" tabindex="-1" role="dialog">'
            + '  <div class="modal-dialog" style="height: 95%; width: 95%;">'
            + '    <div class="modal-content" style="height: 100%;">'
            + '      <div class="modal-header">'
            + '        <div class="pull-right">'
            + '          <button class="btn btn-default btn-sm glyphicon glyphicon-refresh"'
            + '                  id="mxlb_refresh_btn"'
            + '                  title="Refresh">'
            + '          </button>'
            + '          <button class="btn btn-default btn-sm glyphicon glyphicon-new-window"'
            + '                  id="mxlb_tab_btn"'
            + '                  title="Open in a new tab">'
            + '          </button>'
            + '          <button class="btn btn-default btn-sm glyphicon glyphicon-remove"'
            + '                  id="mxlb_close_btn"'
            + '                  title="Close"'
            + '                  data-dismiss="modal">'
            + '          </button>'
            + '        </div>'
            + '        <h4 style="text-align: left; word-wrap: break-word;">'
            + '          <span id="mxlb_title"></span>'
            + '        </h4>'
            + '      </div>'
            + '      <div class="modal-body">'
            + '        <div class="container" style="height: 100%; width: 100%">'
            + '          <div id="mxlb_content" style="height: 100%; width: 100%"></div>'
            + '          <div id="mxlb_overlay"></div>'
            + '        </div>'
            + '      </div>'
            + '    </div>'
            + '  </div>'
            + '</div>';

  $(document.body).append(style + modal);

  $('.open-popup-link').click(MxdLightbox.show);
  $('#mxlb_tab_btn').click(MxdLightbox.open_tab); 
  $('#mxlb_refresh_btn').click(MxdLightbox.refresh); 
});

var MxdLightbox = MxdLightbox || {};

MxdLightbox.getUriParams = function(url) {
  var params = {};
  var url_parts = decodeURIComponent(url).split('?',2);
  if ( url_parts.length > 1 ) {
    url_parts.pop().split('&').map(function(res) {
      var pairs = res.split('=',2);
      var key   = pairs[0];
      var value = pairs[1];

      params[key] = value
    });
  }

  return params;
}

MxdLightbox.open_tab = function() {
  var win = window.open(MxdLightbox.url, '_blank');

  if (win) {
    win.focus();
  } else {
    alert('Please allow popups for this website');
  }
}

MxdLightbox.build= function(event) {
  var spinner    = new Spinner().spin();
  var title      = 'View file';
  var php_re     = /\/(mxd_report_parser|view)\.php/;
  var php_script = php_re.exec(MxdLightbox.url);

  if ( php_script !== null ) {
    var delim = ( MxdLightbox.url.lastIndexOf('?') == -1 ) ? '?' : '&';
    MxdLightbox.url += delim + 'href_target=false';

    var params = MxdLightbox.getUriParams(MxdLightbox.url);
    if ( php_script[1] === 'mxd_report_parser' ) {
      title = params.report.split(/[\\/]/).pop();
    } else if ( php_script[1] === 'view' ) {
      title = params.file.split(/[\\/]/).pop();
    }
  }

  var content_el = $('#mxlb_content');
  var overlay_el = $('#mxlb_overlay');

  var remote_obj = '<object id="mxlb_object" type="text/html"'
                 + '        data="' + MxdLightbox.url  + '"'
                 + '        width="100%" height="100%"'
                 + '        style="overflow:auto; border: 1px ridge lightblue">'
                 + '</object>';

  var error_msg  = '<div style="text-align: center;">'
                 + '  <span class="glyphicon glyphicon-eye-close"'
                 + '        style="font-size: 50px;">'
                 + '  </span><br/>'
                 + '  <h2>Unable to load</h2>'
                 + '  <h2 style="word-wrap: break-word;">' + MxdLightbox.url
                 + '  </h2>'
                 + '</div>'

  var load_timout_fn = setTimeout(function() {
    content_el.html(error_msg);
    spinner.stop();
    overlay_el.hide();
  }, 30000);

  $('#mxlb_title').html(title);
  content_el.html(remote_obj);
  overlay_el.html(spinner.el);
  overlay_el.show();

  $('#mxlb_object')[0].addEventListener('load', function() {
    clearTimeout(load_timout_fn);
    spinner.stop();
    overlay_el.hide();
  });

  $('#mxlb_modal').on('hidden.bs.modal', function () {
    clearTimeout(load_timout_fn);
    overlay_el.hide();
    spinner.stop();
    $('#mxlb_object').detach();
    MxdLightbox.modal_open = false;
  });
}

MxdLightbox.refresh = function(event) {
  if ( !MxdLightbox.modal_open ) {
    return false;
  }

  MxdLightbox.build();

  return false;
}

MxdLightbox.show = function(event) {
  if ( MxdLightbox.modal_open ) {
    return false;
  }

  MxdLightbox.url = event.target.href || event.target.closest('a').href;

  if ( !MxdLightbox.url ) {
    return false;
  }

  MxdLightbox.modal_open = true;

  MxdLightbox.build();

  $('#mxlb_modal').modal();

  return false;
}

