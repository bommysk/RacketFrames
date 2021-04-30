require(['jquery', 'underscore', 'splunkjs/mvc', 'bootstrap.tab', 'splunkjs/mvc/simplexml/ready!'],
    function ($, _, mvc) {

        let tabsInitialzed = [];

        /**
         * The below defines the tab handling logic.
         */

        /**
         * This hides the content associated with the tabs.
         *
         * The normal, auto-magical Bootstrap tab processing doesn't work for us since it requires a particular
         * layout of HTML that we cannot use without converting the view entirely to simpleXML. So, we are
         * going to handle it ourselves.
         * @param {string} tabSetClass
         * @param {string} tab_id
         */
        let hideTabTargets = function (tabSetClass, tab_id) {

            let tokens = mvc.Components.getInstance("submitted");
            let tabs = $(tab_id + ' a[data-elements]');

            // If we are only applying this to a particular set of tabs, then limit the selector accordingly
            if (typeof tabSetClass !== 'undefined' && tabSetClass) {
                tabs = $(tab_id + ' a.' + tabSetClass + '[data-elements]');
            }

            if (!tokens.get('mail_format')) {
                // Go through each toggle tab
                for (let c = 0; c < tabs.length; c++) {

                    // Hide the targets associated with the tab
                    let targets = $(tabs[c]).data("elements").split(",");

                    for (let d = 0; d < targets.length; d++) {
                        $('#' + targets[d], this.$el).hide();
                    }
                }
            }
        };

        /**
         * Force a re-render of the panels with the given row ID.
         *
         * @param {string} row_id The ID of the row to force a rerender on
         * @param {bool} force Force the tab to re-render even if it was already rendered once (defaults to true)
         */
        let rerenderPanels = function (row_id, force) {

            // Set a default argument for dont_rerender_until_needed
            if (typeof force === 'undefined') {
                force = true;
            }

            // Don't do both if the panel was already rendered
            if (!force && _.contains(tabsInitialzed, row_id)) {
                return;
            }

            // Get the elements so that we can find the components to re-render
            let elements = $('#' + row_id + ' .dashboard-element');

            // Iterate the list and re-render the components so that they fill the screen
            for (let d = 0; d < elements.length; d++) {

                // Determine if this is re-sizable
                if ($('#' + row_id + ' .ui-resizable').length > 0) {

                    let component = mvc.Components.get(elements[d].id);

                    if (component) {
                        component.render();
                    }
                }
            }

            // Remember that we initialized this tab
            tabsInitialzed.push(row_id);
        };

        /**
         * Handles the selection of a particular tab by setting
         * the active class on the selected li element.
         *
         * @param {*} e 
         */
        let selectTab = function (e) {
            let tab_id = $(e.target).closest("ul").attr("id");
            // Update which tab is considered active
            $('#' + tab_id + ' > li.active').removeClass("active");
            $(e.target).closest("li").addClass("active");

            // clearTabControlTokens();
            setActiveTabToken(tab_id);

            // Stop if the tabs have no elements
            if ($(e.target).data("elements") === undefined) {
                console.warn("Yikes, the clicked tab has no elements to hide!");
                return;
            }

            // Determine if the set of tabs has a restriction on the classes to manipulate
            let tabSet = null;

            if ($(e.target).data("tab-set") !== undefined) {
                tabSet = $(e.target).data("tab-set");
            }

            // Get the IDs that we should enable for this tab
            let toToggle = $(e.target).data("elements").split(",");

            // Hide the tab content by default
            hideTabTargets(tabSet, '#' + tab_id);

            // Now show this tabs toggle elements
            for (let c = 0; c < toToggle.length; c++) {

                // Show the items
                $('#' + toToggle[c], this.$el).show();

                // Re-render the panels under the item if necessary
                rerenderPanels(toToggle[c]);
            }

        };

        /**
         * The code below handles the tokens that trigger when searches are kicked off for a tab.
         */

        /**
         * Get the tab token for a given tab name
         * @param {string} tab_name The name of the tab
         */
        let getTabTokenForTabName = function (tab_name) {
            return tab_name;
        };

        // Get all of the possible tab control tokens
        let getTabTokens = function (tab_id) {
            let tabTokens = [];

            let tabLinks = $(tab_id + ' > li > a');

            for (let c = 0; c < tabLinks.length; c++) {
                tabTokens.push(getTabTokenForTabName($(tabLinks[c]).data('token')));
            }

            return tabTokens;
        };

        /**
         * Clear all but the active tab control tokens
         */
        let clearTabControlTokens = function (tab_id) {
            console.info("Clearing tab control tokens");

            //tabsInitialzed = [];
            let tabTokens = getTabTokens(tab_id);
            let activeTabToken = getActiveTabToken(tab_id);
            let tokens = mvc.Components.getInstance("submitted");

            // Clear the tokens for all tabs except for the active one
            for (let c = 0; c < tabTokens.length; c++) {
                if (activeTabToken !== tabTokens[c]) {
                    tokens.set(tabTokens[c], undefined);
                }
            }
        };

        /**
         * Get the tab control token for the active tab
         */
        let getActiveTabToken = function (tab_id) {
            return $(tab_id + ' > li.active > a').data('token');
        };

        /**
         * Set the token for the active tab
         */
        let setActiveTabToken = function (tab_id) {
            let activeTabToken = getActiveTabToken(tab_id);
            let tokens = mvc.Components.getInstance("submitted");

            if (activeTabToken) {
                // Set each token if necessary
                activeTabToken.split(",").forEach(function (token) {

                    // If the token wasn't set, set it so that the searches can run
                    if (!tokens.toJSON()[token] || tokens.toJSON()[token] == undefined) {
                        tokens.set(token, "");
                    }
                });
            }
        };

        /**
         * Handle the setting of the token for the clicked tab.
         * @param {*} e 
         */
        let setTokenForTab = function (e) {

            // Get the token for the tab
            let tabToken = getTabTokenForTabName($(e.target).data('token'));

            // Set the token
            let tokens = mvc.Components.getInstance("submitted");
            tokens.set(tabToken, '');

            console.info("Set the token for the active tab (" + tabToken + ")");
        };

        /**
         * Perform the initial setup for making the tabs work.
         */
        let firstTimeTabSetup = function (tab_id) {
            $('a.toggle-tab').on('shown', setTokenForTab);

            $('a.toggle-tab').on('click shown', selectTab);

            // Show the first tab in each tab set
            $.each($('.nav-tabs'), function (index, value) {
                $('.toggle-tab', value).first().trigger('shown');
            });

            // Make the tabs into tabs
            $(tab_id, this.$el).tab();

            // Wire up the tab control tokenization
            let submit = mvc.Components.get("submit");

            if (submit) {
                submit.on("submit", function () {
                    clearTabControlTokens(tab_id);
                });
            }

            // Set the token for the selected tab
            setActiveTabToken(tab_id);
        };

        let default_token_model = mvc.Components.get("default");
        let tabs = default_token_model.get("tabs");

        if (tabs) {
            tabs = eval(tabs);
        } else {
            tabs = ["#tabs"];
        }

        for (let tab of tabs) {
            firstTimeTabSetup(tab);
        }
    });