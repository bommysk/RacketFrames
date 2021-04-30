require(["jquery", "underscore", "splunkjs/mvc", "bootstrap.tab", "splunkjs/mvc/simplexml/ready!"],
function($, _, mvc) {
    var visitedTokens = {};

    /**
     * The below defines the tab handling logic.
     */

    // The normal, auto-magical Bootstrap tab processing doesn"t work for us since it requires a particular
    // layout of HTML that we cannot use without converting the view entirely to simpleXML. So, we are
    // going to handle it ourselves.
    var hideTabTargets = function() {

        var tabs = $("a[data-elements]");

        // Go through each toggle tab
        for (var c = 0; c < tabs.length; c++) {

            // Hide the targets associated with the tab
            var targets = $(tabs[c]).data("elements").split(",");

            for (var d = 0; d < targets.length; d++) {
                $("#" + targets[d], this.$el).hide();
            }
        }
    };

    var selectTab = function(e) {
        // Stop if the tabs have no elements
        if ($(e.target).data("elements") === undefined) {
            console.warn("Yikes, the clicked tab has no elements to hide!");
            return;
        }

        // Get the IDs that we should enable for this tab
        var toToggle = $(e.target).data("elements").split(",");

        // Hide the tab content by default
        hideTabTargets();

        // Now show this tabs toggle elements
        for (var c = 0; c < toToggle.length; c++) {
            $("#" + toToggle[c], this.$el).show();
        }
    };

    /**
     * The code below handles the tokens that trigger when searches are kicked off for a tab.
     */

    // Get the tab token for a given tab name
    var getTabTokenForTabName = function(tab_name) {
        return tab_name; //"tab_" +
    }

    // Get all of the possible tab control tokens
    var getTabTokens = function() {
        var tabTokens = [];

        var tabLinks = $("#tabs > li > a");

        for (var c = 0; c < tabLinks.length; c++) {
            tabTokens.push(getTabTokenForTabName($(tabLinks[c]).data("token")));
        }

        return tabTokens;
    }


    var setTokenForTab = function(e) {
        // Get the token for the tab
        var tabToken = getTabTokenForTabName($(e.target).data("token"));
        var tokens = mvc.Components.getInstance("submitted");

        if (tabToken && !visitedTokens[tabToken]) {
            // Set the token
            tokens.set(tabToken, "");
            //Mark as set
            visitedTokens[tabToken] = 1;
            //Log to console
            //console.info("Set the token for the active tab (" + tabToken + ")");
        }

        //Delaying background load of rest of the tokens by 1 sec
        setTimeout(setAllTokens, 1000);
    }

    var setAllTokens = function() {
        var allTokens = getTabTokens();
        var tokens = mvc.Components.getInstance("submitted");
        for (var c = 0; c < allTokens.length; c++) {
            var curToken = allTokens[c];
            if (curToken && curToken.trim().length && !visitedTokens[curToken]) {
                tokens.set(curToken, "");
                visitedTokens[curToken] = 1;
                //console.info("Set the token for the tab (" + curToken + ")");
            }
        }
    }

    // Wire up the function to show the appropriate tab
    $('a[data-toggle="tab"]').on("shown", selectTab);

    // Make the tabs into tabs
    $("#tabs", this.$el).tab();

    // listen on tab show events
    $('a[data-toggle="tab"]').on("shown", setTokenForTab);

    // Show the first active tab on page load
    $("#tabs > li.active > a").trigger("shown");

});
