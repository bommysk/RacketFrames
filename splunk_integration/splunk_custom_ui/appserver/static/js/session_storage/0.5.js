require([
  "splunkjs/mvc/utils",
  "splunkjs/mvc",
  'splunkjs/ready!',
  "splunkjs/mvc/simplexml/ready!"
], function (splunk_utils, mvc) {
  // globals, var variables will be hoisted    
  // Obtain a reference to the tokens service
  const default_token_model = mvc.Components.get("default");
  const submitted_token_model = mvc.Components.get("submitted");
  const query_string = window.location.search;
  const url_params = new URLSearchParams(query_string);

  function set_token(name, value) {
    default_token_model.set(name, value);
    submitted_token_model.set(name, value);
  }

  function unset_token(name) {
    default_token_model.unset(name);
    submitted_token_model.unset(name);
  }

  const get_storage = key => {
    var now = Date.now(); //epoch time, lets deal only with integer

    var expiresIn = localStorage.getItem(`${key}_expiresIn`);
    if (expiresIn === undefined || expiresIn === null) {
      expiresIn = 0;
    }

    if (expiresIn !== "infinite" && expiresIn < now) {
      // Expired
      remove_storage(key);
      return null;
    } else {
      try {
        var value = localStorage.getItem(key);
        return JSON.parse(value);
      } catch (e) {
        console.log(
          `get_storage: Error reading key [${key}] from localStorage: ${JSON.stringify(e)}`
        );
        return null;
      }
    }
  };

  const save_storage = (key, value, expires = 60) => {
    try {
      const now = Date.now(); //millisecs since epoch time, lets deal only with integer
      const schedule = expires === "infinite" ? expires : now + Math.abs(expires) * 1000;
      const serializedState = JSON.stringify(value);
      localStorage.setItem(key, serializedState);
      localStorage.setItem(`${key}_expiresIn`, schedule);
    } catch (err) {
      console.log('err', err);
    }
  };

  const remove_storage = name => {
    try {
      localStorage.removeItem(name);
      localStorage.removeItem(`${name}_expiresIn`);
    } catch (e) {
      console.log(`remove_storage: Error removing key [${name}] from localStorage: ${JSON.stringify(e)}`);
      return false;
    }
    return true;
  };

  const remove_all_storage = () => {
    try {
      for (var key in localStorage) {
        let value = localStorage.getItem(key);
        remove_storage(key);
      }
    } catch (e) {
      console.log(`remove_all_storage: Error removing all items from localStorage: ${JSON.stringify(e)}`);
      return false;
    }
    return true;
  }

  const page_info = splunk_utils.getPageInfo();
  const app = page_info.app;
  const page = page_info.page;
  const dashboard_name = `${app}_${page}`;

  let session_persist_tokens = get_storage(`${app}_persist_tokens`);
  let session_persist_tokens_dashboard = get_storage(`${dashboard_name}_persist_tokens`);
  let persist_tokens = default_token_model.get("persist_tokens");
  let persist_tokens_dashboard = default_token_model.get("persist_tokens_dashboard");

  let persist_tokens_infinite = default_token_model.get("persist_tokens_infinite");

  if (persist_tokens_infinite) {
    persist_tokens_infinite = JSON.parse(persist_tokens_infinite);
  }

  let persist_tokens_dashboard_infinite = default_token_model.get("persist_tokens_dashboard_infinite");

  if (persist_tokens_dashboard_infinite) {
    persist_tokens_dashboard_infinite = JSON.parse(persist_tokens_dashboard_infinite);
  }

  // dashboard specific persisting
  if (!persist_tokens_dashboard) {
    if (session_persist_tokens_dashboard) {
      persist_tokens_dashboard = JSON.parse(session_persist_tokens_dashboard);
      set_token("persist_tokens_dashboard", persist_tokens_dashboard);
    } else {
      // set to empty, app specific persisting will be in place
      persist_tokens_dashboard = [];
    }
  } else {
    persist_tokens_dashboard = JSON.parse(persist_tokens_dashboard);
    save_storage(`${dashboard_name}_persist_tokens`, JSON.stringify(persist_tokens_dashboard));
  }

  // app specific persisting
  if (!persist_tokens) {
    if (session_persist_tokens) {
      persist_tokens = JSON.parse(session_persist_tokens);
      set_token("persist_tokens", persist_tokens);
    } else {
      // just persist project by default                
      persist_tokens = ["form.project", "project"];
      save_storage(`${app}_persist_tokens`, JSON.stringify(persist_tokens));
    }
  } else {
    persist_tokens = JSON.parse(persist_tokens);
    save_storage(`${app}_persist_tokens`, JSON.stringify(persist_tokens));
  }

  // app specific
  persist_tokens.forEach(function (persist_token) {
    let get_token_value = url_params.getAll(persist_token);
    let session_token_value = get_storage(`${app}_${persist_token}`);
    let ttl = 60;

    if (persist_tokens_infinite && persist_tokens_infinite.includes(persist_token)) {
      ttl = "infinite";
    }

    // give GET parameter top priority
    if (get_token_value.length > 0) {
      if (get_token_value.length === 1) {
        get_token_value = get_token_value[0];
      }

      // if (persist_token.startsWith("form.")) {
      //   let no_form_persist_token = persist_token.slice("form.".length);
      //   set_token(no_form_persist_token, get_token_value);
      //   save_storage(`${app}_${no_form_persist_token}`, get_token_value, ttl);
      // }

      save_storage(`${app}_${persist_token}`, get_token_value, ttl);
    }
    // give local storage priority
    else if (!url_params.get("ignore_session_storage") && session_token_value) {
      set_token(persist_token, session_token_value);
    }
    // lastly default token value has preference
    else {
      let token_value = default_token_model.get(persist_token);
      if (token_value) {
        save_storage(`${app}_${persist_token}`, token_value, ttl);
      }
    }

    // on filter (token) change write function to set value on localStorage
    default_token_model.on(`change:${persist_token}`, function () {
      let token_value = default_token_model.get(persist_token);
      save_storage(`${app}_${persist_token}`, token_value, ttl);
      if (!persist_tokens.includes(`form.${persist_token}`) && !persist_token.startsWith("form.")) {
        save_storage(`${app}_form.${persist_token}`, token_value, ttl);
      }
    });
  });

  // dashboard specific gets precedence, will overrite app specific
  persist_tokens_dashboard.forEach(function (persist_token) {
    let get_token_value = url_params.getAll(persist_token);
    let session_token_value = get_storage(`${dashboard_name}_${persist_token}`);
    let ttl = 60;

    if (persist_tokens_dashboard_infinite && persist_tokens_dashboard_infinite.includes(persist_token)) {
      ttl = "infinite";
    }

    // give GET parameter top priority
    if (get_token_value.length > 0) {
      if (get_token_value.length === 1) {
        get_token_value = get_token_value[0];
      }

      set_token(persist_token, get_token_value);

      // if (persist_token.startsWith("form.")) {
      //   let no_form_persist_token = persist_token.slice("form.".length);
      //   set_token(no_form_persist_token, get_token_value);
      //   save_storage(`${dashboard_name}_${no_form_persist_token}`, get_token_value, ttl);
      // }

      save_storage(`${dashboard_name}_${persist_token}`, get_token_value, ttl);
    }
    // give local storage priority
    else if (!url_params.get("ignore_session_storage") && session_token_value) {
      set_token(persist_token, session_token_value);
    }
    // lastly default token value has preference
    else {
      let token_value = default_token_model.get(persist_token);
      if (token_value) {
        save_storage(`${dashboard_name}_${persist_token}`, token_value, ttl);
      }
    }

    // on filter (token) change write function to set value on localStorage
    default_token_model.on(`change:${persist_token}`, function () {
      let token_value = default_token_model.get(persist_token);
      save_storage(`${dashboard_name}_${persist_token}`, token_value, ttl);
      if (!persist_tokens_dashboard.includes(`form.${persist_token}`) && !persist_token.startsWith("form.")) {
        save_storage(`${dashboard_name}_form.${persist_token}`, token_value, ttl);
      }
    });
  });
});