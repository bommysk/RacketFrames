require([
     "splunkjs/mvc",
     "splunkjs/mvc/simplexml/ready!"
     ], function(mvc) {
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

        function setSessionValue(name, value) {
            try {
                 if (typeof(sessionStorage) !== "undefined") {
                    sessionStorage.setItem(name, value);
                 }
            } 
            catch (error) {
                if (error == QUOTA_EXCEEDED_ERR) {
                     console.log("Error: Session Storage limit exceeds.");
                } 
                else {
                    console.log("Error: Saving to session storage.");
                }
            }
        }

        function getSessionValue(name) {
            try {
             if (typeof(sessionStorage) !== "undefined") {
                 return sessionStorage.getItem(name);
             }
             return "";
            } catch (error) {
             console.log("Session Storage Error :GET: " + error);
             return "";
            }
        }

        var tokens = mvc.Components.get("default");
        var submitted = mvc.Components.get("submitted");

        token_value = getSessionValue("project");

        // give local storage priority
        if (token_value) {
            set_token("project", token_value);            
            set_token("form.project", token_value);
        }
        else {
            if (tokens.get("project")) {
                setSessionValue("project", tokens.get("project"));   
            }
        }

        // on filter (token) change write function to set value on sessionStorage
        tokens.on("change:project", function() {
         setSessionValue("project", tokens.get("project"));
        });
 });