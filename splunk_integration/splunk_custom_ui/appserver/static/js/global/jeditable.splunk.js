// Edit an "editable" class element in the document
function createEditableFields(tbl) {
  var button='<button type="submit" style="border: 0; background: transparent">';

  //Editable Cells
  $(tbl).find('div.editable').editable(submitEdit,  { 
    indicator : "<img src='mxd/css/images/indicator.gif'>",
    tooltip   : "Double click to edit...",
    type      : 'textarea',
    rows      : '3',
    cols      : '40',
    event     : "dblclick",
    select    : true,
    submit    : button+' <img src="../css/images/accept-icon.png" alt="submit" /> </button>',
    cancel    : button+' <img src="../css/images/cross-icon.png" alt="submit" /> </button>',
    onblur    : 'ignore',
    submitdata: function (value, settings) { return {orig : value }}
  });

  //Editable Select Cells
  $(tbl).find('div.editable_select').editable(submitEdit,  { 
    indicator : "<img src='../css/images/indicator.gif'>",
    tooltip   : "Click to edit...",
    type      : 'select',
    event     : "click",
    select    : true,
    data      : function(value, settings) {
      var str = this.getAttribute("data");
      if (! value || str.indexOf(value) == -1) 
      str = str.replace('}', ",'selected':''}"); 
      return str;
    },
    submitdata: function (value, settings) { return {orig : value }}
  });

  //Editable Select Cells with NO tooltip
  $(tbl).find('div.editable_select_no_tooltip').editable(submitEdit,  { 
    indicator : "<img src='../css/images/indicator.gif'>",
    type      : 'select',
    event     : "click",
    select    : true,
    data      : function(value, settings) {
      var str = this.getAttribute("data");
      if (! value || str.indexOf(value) == -1) {
        str = str.replace('}', ",'selected':''}");
      }
      return str;
    },
    submitdata: function (value, settings) { return {orig : value }}
  });

  //Editable Date Cells
  $(tbl).find('div.editable_date').editable(submitEdit,  { 
    type: 'datepicker',
    tooltip: 'Click to edit...',
    event: 'click',
    submitdata: function (value, settings) { return {orig : value }}
  });

  //Editable Multi-Select Cells
  var index = 0
  $(tbl).find('div.editable_multi_select').each( function() {
    var id = $(this).attr('id');
    var multiForm = '<FORM action="javascript:editEvent('+"'"+id+"')"+'">';
    var options = $(this).attr('data');
    var delimiter = $(this).attr('data-delimiter');
    var selected = $(this).text().split(delimiter);
    options = options.substr(1, options.length-2).split(','); //Removes the { } from around the options
    var multiSelect = "<select name='TEAMS' id='multi-select-column-"+index+"' multiple>";
    $(options).each(function() {
      var keyValue = this.replace(/'/g, "").split(':');
      var dtext = (selected.indexOf(keyValue[0])!==-1)? " selected" : "";
      multiSelect += "<option value='"+keyValue[0]+"'"+dtext+">"+keyValue[1]+"</option>";
    });
    multiSelect += "</select>";
    //$(this).append(multiSelect).wrap(multiForm);
    $(this).html(multiSelect).wrap(multiForm);
    create_multiselect('multi-select-column-'+index);
    index++;
  });
}

//str: ex. id:303122;prop:TEAMS;
//ID value of div.editable_multi_select (and all editable classes)
function editEvent(str) {
  mxd_connect = {
    url : '/connect/rest.php/',
    get : function(urlin, params) {
      var rv = {};
      $.ajax({
        url: urlin,
        type: 'POST',
        data: params,
        success: function(data) {
          alert('SUCCESS');
          //console.log("RETURNED:"+JSON.stringify(data, null, 2));
        },
        error: function() {
          alert('FAILURE');
        },
      });
      return rv.result;
    },

    set_event: function(cid, name, value, split) {
      split = (split === 'true')? true : false;
      var my_url = this.url+'db/'+mxd_sql_db+'/context/'+cid+'/event/'+name;
      //console.log(my_url);
      return this.get(my_url, {'value': value, 'split': split});
    },
  };

  var cell = $(document.getElementById(str));
  var split = cell.attr('data-split');
  var val = cell.find('select').val();
  val = (val === null)? '' : val.join(',');
  str = str.split(';');
  var cid = str[0].split(':')[1];
  var name = str[1].split(':')[1];
  mxd_connect.set_event(cid, name, val, split);
}

function submitEdit(value, settings) { 
    var submitdata = {};
    var id_data = {};
    submitdata[settings.name] = value;
    // Convert id to something useful
    var store_id = this.id;
    var id_values = this.id.split(";"); 
    for (var c1 = 0, len=id_values.length; c1 < len; c1++) {
       var hold = id_values[c1].split(":"); 
       id_data[hold[0]] = hold[1];
    }
    submitdata[settings.id] = JSON.stringify(id_data);

    var data = this.getAttribute("data");
    if (data && data.indexOf('split') != -1) {
      submitdata['split']=true;
    }
    submitdata['sql_host'] = mxd_sql_host; // set in html_library.php
    submitdata['sql_db'] = mxd_sql_db; // set in html_library.php

    if ($.isFunction(settings.submitdata)) {
      $.extend(submitdata, settings.submitdata.apply(this, [this.revert, settings]));
    } else {
      $.extend(submitdata, settings.submitdata);
    }
    if (submitdata['orig'] == value) // value did not change, just return
      return value;

    {
      /* Defaults for ajaxoptions. */
      var ajaxoptions = {
        url     : "../scripts/edit_property.php",
        type    : 'POST',
        data    : submitdata,
        dataType: 'json',
        async   : true,  // This used to be false
        success : function(result, status) {
          if (result != "") {
            // get position of second comma
            value = result['value'];
            if (result['error']) {
              alert(result['error']+'\nOrginal:\n'+result['original']);
              console.log('error: '+result['error']);
              console.log('original: '+result['original']);
            }
            if (store_id.search("EDIT") != -1) {
              var time=store_id.replace("EDIT", "EDIT/TIME");
              var element=document.getElementById(time);
              if (element) {
                element.innerHTML=result['date'];
                if (element.onchange != null)
                  element.onchange();
              }

              var user=store_id.replace("EDIT", "EDIT/USER");
              element=document.getElementById(user);
              if (element) {
                element.innerHTML=result['webuser'];
                if (element.onchange != null)
                  element.onchange();
              }

              var log=store_id.replace("EDIT", "EDIT/LOG");
              element=document.getElementById(log);
              if (element) 
                element.innerHTML=result['log'];
              if (typeof(signoff_percentage) == 'function')
                signoff_percentage(submitdata.id);
            }
          }
        },
        error   : function(xhr, status, error) {
                    onerror.apply(form, [settings, this, xhr]);
                  }
      };

      $.ajax(ajaxoptions);          
    }

    return value;
  }
