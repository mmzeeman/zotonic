<html>
  <head>
	<meta name="robots" content="noindex, nofollow" />
	<title>Bus subdomain handler of: {{ m.site.title }} </title>
  </head>

  <body>
	{#
		See: http://fettig.net/weblog/2005/11/28/how-to-make-xmlhttprequest-connections-to-another-server-in-your-domain/
		http://fettig.net/playground/ajax-subdomain/test5-iframe.html
	#}
	{% include "_js_include_jquery.tpl" %}

  <script type="text/javascript"> 	
    function z_bus_host() {
    	var url = window.location.pathname + '?m=p';   
    	$.ajax({ 
			url: url, 
			type: "post",
			data: "z_pageid={{ q.z_pageid|urlencode }}", 
			dataType: "text",
			async: true,
            cache: false,
            headers: {"cache-control": "no-cache" }
		}).done(function(data, textStatus) {
			if('bus_message' in window) 
				window.bus_message(data);
			setTimeout(function() { z_bus_host(); }, 200);
		}).fail(function(xmlHttpRequest, textStatus, errorThrown) {
			setTimeout(function() { z_bus_host(); }, 1000);
		});
	}
	</script>
  </body>
</html>
