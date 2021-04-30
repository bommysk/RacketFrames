define([
	 "jquery",
	 "splunkjs/mvc",
	 "splunkjs/mvc/simplexml/ready!"
	 ], function($, mvc) {
		$.ajax({
			method: 'post',
		    url: 'https://at.apple.com/api/v1/shortlink',
		    data: {"destination": window.location.href},
		    beforeSend: function(xhr) {
		    	xhr.setRequestHeader("Content-Type", "application/json");
		    	xhr.setRequestHeader("Accept", "application/json");
		        xhr.setRequestHeader("Authorization", "ief7FwsTtEB894rkwp6ywuZUR6vVk7Il07x9cHkd");
		    }, success: function(data){
		        alert(data);
		        //process the JSON data etc
		    }
		})
	}
);