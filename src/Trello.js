// module Trello

exports._client = function(credentials) {
  var trello = require('node-trello');
  return new trello(credentials.key, credentials.token);
};

exports._get =
  function(client) {
    return function(path) {
      return function(opts) {
        return function(error) {
          return function(success) {
            return function() {
              client.get(path, opts, function(errorValue, successValue) {
                if (errorValue)
                  error(errorValue)();
                else
                  success(successValue)();
              });
              return {};
            };
          };
        };
      };
    };
  };
