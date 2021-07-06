// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

define([
     "jquery",
     "splunkjs/mvc",
     "splunkjs/mvc/simplexml/ready!"
     ], function($, mvc) {

     	function row_offset_sum(row_offset, idx) {
     		let result = 0;
        
	        while (idx >= 0) {
	        	result += row_offset[idx];
	            idx -= 1;
	        }	            
	            
	        return result;
     	}

     	function array_sum(array) {
     		let sum = array.reduce(function(a, b){
				return a + b;
			}, 0);

			return sum;
     	}

     	function range(start, stop, step) {
		    if (typeof stop == 'undefined') {
		        // one param defined
		        stop = start;
		        start = 0;
		    }

		    if (typeof step == 'undefined') {
		        step = 1;
		    }

		    if ((step > 0 && start >= stop) || (step < 0 && start <= stop)) {
		        return [];
		    }

		    let result = [];
		    for (let i = start; step > 0 ? i < stop : i > stop; i += step) {
		        result.push(i);
		    }

		    return result;
		}

     	function get_colspan_offset(table_id) {
     		let table = $(table_id);
     		let thead = table.find('thead').eq(0);
			let th = thead.find('th');			

            let $th_list = [];
			let th_list = [];

			let th_row_span = [];

			$.each(th, function(index, header) {
				let $header = $(header);
				$th_list.push($header);
			    if ($header.attr('rowspan')) {    
			        th_row_span.push(Number.parseInt($header.attr('rowspan')));			        
			    }
			    else {
			        th_row_span.push(1);
			    }	    
			});

			console.log($th_list);          

			let max_row_span = Math.max(...th_row_span);			

			// ignore empty headers but keep separators
			$.each(th, function(index, header) {
				let $header = $(header);
			    if ($header.attr('style')) {
			    	if ($header.attr('style').includes('display:none') || $header.attr('style').includes('display: none')) {
			            return false;
			        }
			    }			        
			    
			    th_list.push($header.text());
			});

			//console.log(th_list);

			// construct colspan offset
			// colspan offset: [row1,row2,row3...]
			// row: [{th_idx : (sub_cols_start, sub_cols_end)}, ...]
			// holds number of th per tr
			let row_offset = {};
			let colspan_offset = [];					

			let thead_tr = thead.find('tr');
			let thead_tr_len = thead_tr.length;

			for (let i = 0; i < thead_tr_len; i++) {
			   colspan_offset.push({});			   
			}            

			for (let row_idx = 0; row_idx < thead_tr_len; row_idx++) {
				let tr = thead_tr[row_idx];
				let $tr = $(tr);
			    let all_th = $tr.find('th');
			    let th_tr = [];

			    // respect any filtering that happened before
			    $.each(all_th, function(index, header) {
			    	let $header = $(header);			    	
			        if (th_list.includes($header.text())) {
			            th_tr.push(header);
			        }
			    })			    
			    
			    row_offset[row_idx] = th_tr.length;			    
			    
			    let prev_colspan = [0];
			    for (let th_idx = 0; th_idx < th_tr.length; th_idx++) {
			    	let theader = th_tr[th_idx];
			    	let $theader = $(theader);
			        // the last row can't have subheaders
			        if (row_idx < thead_tr_len - 1) {
			            let th_offset = 0;

			            // calculate indicies of sub headers
			            let theader_rowspan = 1;
			            if ($theader.attr('rowspan')){
			            	theader_rowspan = Number.parseInt($theader.attr('rowspan'));
			            }

			            let theader_colspan = 1;
			            if ($theader.attr('colspan')) {
			                theader_colspan = Number.parseInt($theader.attr('colspan'));	
			            }	                

			            if (theader_rowspan < (max_row_span - row_idx)) {
			            	th_idx_offset = 0;

			                if (row_idx > 0) {
			                    th_idx_offset = row_offset_sum(row_offset, row_idx - 1);	
			                }			                    

			                colspan_offset[row_idx][th_idx + th_idx_offset] = [array_sum(prev_colspan) + row_offset_sum(row_offset, row_idx), array_sum(prev_colspan) + row_offset_sum(row_offset, row_idx) + theader_colspan];
			                
			                prev_colspan.push(theader_colspan);
			            }
			            else if (theader_rowspan === max_row_span) {
			            	colspan_offset[row_idx][th_idx] = [];
			            }		                
			        }
			    }
			}
				
			// sanitize colspan_offset
			for (idx of range(colspan_offset.length).reverse()) {
			    if (idx === 0)
			        break
			    
			    // skip last entry
			    if (colspan_offset[idx].length === 0)
			        continue;

			    let offset_dict = colspan_offset[idx];
			    let prev_offset_dict = colspan_offset[idx - 1];

			    let prev_offset_dicts = colspan_offset.slice(0,idx);

			    let prev_offset_keylist = Object.keys(prev_offset_dict);
			    prev_offset_keylist.sort();

			    // sort offset key list
			    let offset_keylist = Object.keys(offset_dict);
			    offset_keylist.sort();
			    
			    for (let prev_key of prev_offset_keylist) {
			        trickle_down_update = false;
			        
			        for (let key of offset_keylist) {
			            let header_range = offset_dict[key];
			            start = header_range[0];
			            end = header_range[1];
			            // - 1 to save the actual child
			            range_diff = (end - start) - 1; 
			            
			            updated_key = Number.MAX_SAFE_INTEGER;

			            if (range(prev_offset_dict[prev_key][0], prev_offset_dict[prev_key][1]).includes(key)) {
			            	trickle_down_update = true;
			                updated_end = prev_offset_dict[prev_key][1] - range_diff;
			                prev_offset_dict[prev_key] = [prev_offset_dict[prev_key][0], updated_end];

			                updated_key = prev_key;
			            }			                
			            
			            if (trickle_down_update) {
			                prev_key_copy = prev_key;

			                prev_offset_dicts_counter = Object.keys(prev_offset_dicts).length - 1;

			                while (prev_offset_dicts_counter >= 0) {
			                    let prev_offset_dict_trickle = prev_offset_dicts[prev_offset_dicts_counter];

			                    prev_offset_trickle_keylist = prev_offset_dict_trickle.keys();
			                    prev_offset_trickle_keylist.sort();

			                    updated_key_copy = updated_key;
			                    for (let update_key of prev_offset_trickle_keylist) {
			                        if (update_key > updated_key) {
			                            updated_end = prev_offset_dict_trickle[update_key][1] - range_diff;
			                            prev_offset_dict_trickle[update_key] = [prev_offset_dict_trickle[updated_key_copy][1], updated_end];

			                            updated_key_copy = update_key;
			                        }
			                    }

			                    prev_offset_dicts_counter -= 1;

			                    if (prev_offset_dicts_counter < 0)
			                        break;

			                    // the dict right before the one that was just updated
			                    prev_prev_offset_dict = prev_offset_dicts[prev_offset_dicts_counter];

			                    prev_prev_offset_keylist = prev_prev_offset_dict.keys();
			                    prev_prev_offset_keylist.sort();

			                    // set prev_key_copy to the next prev offset dict
			                    for (let prev_prev_key of prev_prev_offset_keylist) {
			                        if (prev_key in range(prev_prev_offset_dict[prev_prev_key][0], prev_prev_offset_dict[prev_prev_key][1])) {
			                            updated_end = prev_prev_offset_dict[prev_prev_key][1] - range_diff;
			                            prev_prev_offset_dict[prev_prev_key] = [prev_prev_offset_dict[prev_prev_key][0], updated_end];

			                            updated_key = prev_prev_key;
			                            break;
			                        }
			                    }
			                }
			            }

			            trickle_down_update = false;
			        }
			    }
			}
			
			return colspan_offset;
     	}

        function row_value(row, field) {
          return row[field];
        }

		function get_th_positions(all_th) {
			let data = all_th.map(function() {
			  var th = $(this);
			  return {
			    el: this,
			    left: th.offset().left
			  };
			}).toArray();

			return data;
		}
        function show_hide_save(view_id, element, cols, fields) {
        	fields = fields.filter(function(field) {
        		return (!field.startsWith('_'));
        	});

			show_hide_column(view_id, element, cols, fields);
			localStorage.setItem(element.id, element.checked);
		}

		// IMPLEMENTATION NOT COMPLETE
		// show_hide_column is only enabled for datatables, not Splunk
		// native tables
        // global var to hold parent heade indexes
        var parent_th_indexes = {};
        function show_hide_column(view_id, element, cols, fields) {
        	$(document).ready(function() {        		
        		console.log(element.checked);        		
      			let $table = $("#" + view_id + " table");
      			let table_selector = "#" + view_id + " table";
				let column_class = element.id + "_class";
				let header_column_class = `header_${column_class}`;
				let column_class_selector = `.${column_class}`;   
				let header_column_class_selector = `.${header_column_class}`;   
				let all_th = $("#" + view_id + " table thead tr th");
				let all_th_DOM = []

				$.each(all_th, function(index, header) {
				    let $header = $(header);
				    all_th_DOM.push($header[0]);
				});

				let colspan_offset = get_colspan_offset(table_selector);
				let flattened_colspan_offset = {};

                for (let idx = 0; idx < colspan_offset.length; idx++) {				  
				  let offset_dict = colspan_offset[idx];

				  for (const [key, value] of Object.entries(offset_dict)) {
                      flattened_colspan_offset[Number.parseInt(key)] = value;
				  }
                }

				console.log("COLSPAN OFFSET");
				console.log(colspan_offset);
				console.log("FLATTENED COLSPAN OFFSET");
				console.log(flattened_colspan_offset);	
				// store all th names, for the higher the row of the header
				// the lower the nesting in the name
				// i.e. top row is only the text of header, while the lowest
				// row is data("sort-key"), and the colon separation decrements
				// by 1 as we go up the rows

                /* ***** Could be useful in the future, leaving commented out for now ***** */
				// let nested_th_names = [];
				// let all_th_visited_indexes = [];
				// for (let idx = 0; idx < colspan_offset.length; idx++) {				  

				//   let offset_dict = colspan_offset[idx];

				//   for (const [key, value] of Object.entries(offset_dict)) {
				//   	// first row of header, just store text
				//   	if (idx === 0) {
				//   		//nested_th_names.push($(all_th[key]).text().trim());
				//   		all_th_visited_indexes.push(Number.parseInt(key));
				//   	}
				//   	else {
				//   		//nested_th_names.push($(all_th[key]).data("sort-key").trim().split('::').slice(0, idx + 1).join('::'));
				//   		all_th_visited_indexes.push(Number.parseInt(key));
				//   	}

				//   	all_th_visited_indexes.push(Number.parseInt(key));
				//   }
				// }

				// all_th.each(function(index, th) {
				// 	if (!all_th_visited_indexes.includes(index)) {

    	        //      if (!$(all_th[index]).data("sort-key")) {							
				// 			nested_th_names.splice(index, 0, $(all_th[index]).text());
				// 		}
				// 		else {
                //          nested_th_names.splice(index, 0, $(all_th[index]).data("sort-key").trim());
				// 		}
												
				// 		all_th_visited_indexes.push(index);
				// 	}
				// });

				// console.log("ALL TH");
				// console.log(nested_th_names);

                // we have a number column as the first column
				if ($(all_th[0]).hasClass('row-number')) {
                    fields.unshift("");
				}			
								                
				for (let col_name of cols) {					
					let col_idx = fields.indexOf(col_name);
					console.log(`col_name: ${col_name}`);
					console.log(`col_idx: ${col_idx}`);
					let $td = $(`${table_selector} td:nth-child(${col_idx})`);					

					$td.addClass(column_class);					

					if (element.checked) {
						$(column_class_selector).show();

						// adjust colspans using flattened_colspan_offset                    
						// increase parent colspan by 1
						if (!$(all_th[parent_th_idx]).attr('colspan') || $(all_th[parent_th_idx]).attr('colspan') === 1) {
                            $(header_column_class_selector).show();
						}

						let th_children_offset = flattened_colspan_offset[parent_th_idx];
                        
                        if (th_children_offset) {
                            for (const child_th_idx of th_children_offset) {
								$(all_th[child_th_idx]).show();
							}
                        } 				
					}
					else {
						// hoisting
						var $parent_th = $td.closest('tbody').prev('thead').find('> tr > th:eq(' + $td.index() + ')');	                    
						$parent_th.addClass(header_column_class);
						
						// index flattened_colspan_offset and traverse children					
	                    var parent_th_idx = all_th_DOM.indexOf($parent_th[0]);						
						$(column_class_selector).hide();
						// adjust colspans using flattened_colspan_offset                    
						// reduce parent colspan by 1
						if (!$(all_th[parent_th_idx]).attr('colspan') || $(all_th[parent_th_idx]).attr('colspan') === 1) {
                            $(header_column_class_selector).hide();

                            let th_children_offset = flattened_colspan_offset[parent_th_idx];
                            
                            if (th_children_offset) {
								for (const child_th_idx of th_children_offset) {
									$(all_th[child_th_idx]).hide();
								}
                            }
						}
						else {
							// need to traverse children and reduce colspans appropriately
							let th_children_offset = flattened_colspan_offset[parent_th_idx];
                            

                            while (th_children_offset) {

                            }
						}
					}                                                                    										    
				}
        	});        	
        }

        function show_hide_rows(element, table_id, row_idx) {
			let $rows = $(`#${table_id} tr`);

        	if (element.checked) {
        		$rows.eq(row_idx).show();
        	}
        	else {
        		$rows.eq(row_idx).hide();
        	}		
        }    

        return { row_value, show_hide_save, show_hide_column, show_hide_rows };
    }
 );
