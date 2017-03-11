-define(DEFAULT_POLLING_TIMEOUT, 10). % 10 second
-define(MAX_POLLING_TIMEOUT, 60). % max timeout 60s

-define(WEBHOOK_MAX_CONNECTIONS, 100). %% 100 connections
-define(WEBHOOK_TIMEOUT, 15000). %% 15 sec

-define(FAILED_ENCODED_BINARY, <<"{\"error_message\":\"Internal Server Error\",\"reason\":\"failed to encode your post data\"}">>).
