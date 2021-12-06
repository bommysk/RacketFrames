//Generic functions for setup and use of mxd site servers

function create_multiselect_with_custom(id_name, selected) {

  $("#"+id_name).select2({
    multiple: true,
    tags: true,
    closeOnSelect: false,
    insertTag: function (data, tag) {
        data.push(tag);
    },
    placeholder: "Filter",
    allowClear: true,
  });
  /* add css for min width */
  const select2elements = document.getElementsByClassName('select2-container');
  Array.from(select2elements, function (node) {
    node.style.minWidth = "130px";
    node.querySelector("input").style.minWidth = "130px";
  })

  var isCleared = false;
  var isChanged = false;

  /* create options for any custom values */
  if (selected) {
    const selectArr = selected.split(",");
    selectArr.forEach( function(curr) {
        if ($("#"+id_name).find("option[value='" + curr + "']").length) {            
        }
        else {
            var newOption = new Option(curr, curr, true, true);
            $("#"+id_name).append(newOption).trigger('change');
        }
    })
  }

  $("#"+id_name).on('select2:selecting', function(e) {
    isChanged = true;
  });

  $("#"+id_name).on('select2:unselecting', function(e) {
    isChanged = true;
  });

  $("#"+id_name).on('select2:clear', function(e) {
    //fired off when all selection is cleared
    isCleared = true;
    isChanged = true;
  });

  $("#"+id_name).on('select2:closing', function(e) {
    /* if cleared, don't immediately search */
    if (isCleared) {
        e.preventDefault();
        isCleared = false;
    }
  });

  $("#"+id_name).on('select2:close', function(e) {
    /* only search on changed values */
    if (isChanged) {
      this.form.submit();
    }
  });

  //Fix bug when clearing text with Safari X
  $('div.'+id_name+' :input[type=search]').on('search', function(){
    $(this).trigger('keyup');
  })
}

function create_multiselect(id_name) {
  // setup style
  $("#"+id_name).multiselect({
    header: true,
    minWidth: 130,
    checkAllText: 'All',
    uncheckAllText: 'None',
    classes: id_name
  }).multiselectfilter({
    label: '',
    width: 115,
    placeholder: 'Filter'
  });
  var changed = false;
  $("#"+id_name).multiselect({
    click: function(event, ui){ changed = true; },
    checkAll: function(){ changed = true; },
    uncheckAll: function(){ changed = true; },
    open: function(event, ui){
      $('div.'+id_name+' :input[type=search]').focus();
    },
    close: function(){
      if (changed)
        this.form.submit();
      changed=false;
    }
  });
  //Fix bug when clearing text with Safari X
  $('div.'+id_name+' :input[type=search]').on('search', function(){
    $(this).trigger('keyup');
  })
}

$(document).ajaxSend(function (event, xhr, settings) {
    settings.xhrFields = {
        withCredentials: true,
    };
});

$(document).ajaxError(function( event, jqXHR, settings, thrownError ) {
  var errorText = "Something has gone wrong!\nWhile this page was loading a resource at:\n  " + settings.url
  + "\n\nThe following error was thrown:\n  " + thrownError 
  + "\n\nThe following data was provided:\n  " + jqXHR.responseText;
  if (jqXHR.readyState < 4) {
    //Connection started but never completed (likely navigated away)
    console.log(errorText);
  } else {
    alert(errorText);
  }

/*
  var dialogBox = $('<div id="ajaxErrorDialog" title="AJAX Error"> </div>');
  dialogBox.html("<pre>" + errorText + "</pre>");
  $("body").append(dialogBox);
  dialogBox.dialog({
    close: function( event, ui ) {
      dialogBox.dialog("destroy");
    }
  });
*/
});

// Function: mxdAjax
// Description: Generate authentication key valid on alternate MXD servers for AJAX calls
// Input: callBack (function) - Any callable object, will be passed one parameter; the authentication key to be used for any subsequent AJAX calls (only valid for XX seconds)
// Return: null
function mxdAjax(callBack) {
  var sandboxString = getUriRoot();
  var auth_opts = {
    type     : "GET",
    cache    : false,
    url      : 'https://'+window.location.hostname+'/'+sandboxString+'connect/authKey',
    datatype : 'json',
    data     : {}
  };

  auth_opts.success = function(result) {
    key = result.result.key;
    callBack(key);
  };

  auth_opts.failure = function(e) {
    console.log('ERROR: Auth key generation failed');
  };

  $.ajax(auth_opts);
}

var mxdCheckedSiteServers = {};
function mxdCheckSiteServer(url, callBack) {
  var parser = document.createElement('a');
  parser.href = url;
  var hostname = parser.hostname;

  if (typeof(mxdCheckedSiteServers[hostname]) === "undefined") {
    var tester=new Image();

    tester.onload = function() {
      mxdCheckedSiteServers[hostname] = tester;
      callBack();
    }

    tester.onerror = function(err) {
      console.log("Access to server " + hostname + " appears to not be working; some elements of this page may not work correctly.\n");
    }
    
    tester.src='https://' + hostname + '/gizmos/transparent_pixel.png?' + (new Date).getTime();
  } else {
    callBack();
  }
}

function mxdSiteServerLookup(site) {
    switch(site) {
        case 'soc':
        case 'cdm':
        case 'mobsi':
            site = "scv";
            break;
        case 'ldc':
            site = "austin";
            break;
        case 'jdc':
            site = "tokyo";
            break;
        case 'hdc':
        case 'algo':
            site = "israel";
            break;
        case 'msc':
            site = "mesa";
            break;
        case 'germany':
            site = "germany";
            break;
        case 'ams':
            site = "amsterdam";
            break;
    }
    return "https://" + site + "-mxd.csg.apple.com/";
}

function getUriRoot() {
  var sandBoxString = location.pathname.match(/^\/(sandbox\/[^\/]+\/)/);

  if (sandBoxString)
    sandBoxString = sandBoxString[1];
  else
    sandBoxString = "";

  return sandBoxString;
}

// setup a link to send post data
// id - the id of the form to use
function submitPostAsLink(id) {
  var object = document.getElementById(id);
  object.submit();
}

function refresh(form) {
  form.submit();
}

// refresh the MXD page
function refreshpage(obj, page_number) {
  obj.form['page'].value = page_number;
  obj.form.submit();
}



//Required: puppet, script_name, params. Optional: site
function dod_launch(puppet, script_name, params, site) {
  //console.log("dod_launch: Starting...");

  project_in_url = (new RegExp('project=(.+?)(?:&|$)')).exec( window.location.search );
  project = project_in_url !== null ? project_in_url[1] : 'NULL';
  webuser = "${webuser}";
  domain  = window.location.hostname;
  path    = window.location.pathname.split("/");

  if( typeof(site) == 'undefined' ) {
    if( typeof(mxd_site) == 'undefined' ) {
      // Default to SCV
      site = 'scv';
    } else {
      // Defined by dashboard.php
      site = mxd_site;
    }
  } // Else passed into function

  if( path.indexOf("sandbox") != -1 ) {
    // In Sandbox (Stay Local)
    site_url = path.slice(0, 3).join("/") + '/';
  } else {
    // Everwhere else (Go to Site Server)
    site_url = mxdSiteServerLookup(site);
  }

  relative_path = site_url + 'apps/dod/dod.php';
  //console.log("PHP URL: " + relative_path);

  if( typeof(window["dod_success"]) == 'function' ) { 
    // Success function defined by user
    var success = "dod_success";
  } else {
    // Default to empty function
    var success = "function(){}";
  }

  if( typeof(window["dod_error"]) == 'function' ) {
    // Error function defined by user
    var error = "dod_error";
  } else {
    // Default to empty function
    var error   = "function(){}";
  }

  var ajax_opts = {
    type  : "POST",
    cache : false,
    url   : relative_path,
    data  : {
      PROJECT : project,
      SITE    : site,
      WEBUSER : webuser,
      PUPPET  : puppet,
      SCRIPT  : script_name,
      PARAMS  : params,
    },
    headers : {
      'MXD_OAUTH_USER' : mxd_user
     //MXD_OAUTH_KEY to be added by mxdAjax callBack function
    },
    success : window[success],
    error   : window[error]
  };

  mxdAjax(function(key) {
    ajax_opts['headers']['MXD_OAUTH_KEY'] = key;
    $.ajax(ajax_opts);
  });
}

