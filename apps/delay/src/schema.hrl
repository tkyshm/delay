-record(job, {
          uid = undefined     :: binary() | undefined,
          pid = undefined     :: pid() | undefined,
          exec_time = 0       :: non_neg_integer(),
          data = undefined    :: term() | undefined,
          webhook = undefined :: binary() | undefined,
          status = waitting   :: waitting | ready
         }).

-record(acceptor, {
          pid  = undefined :: pid() | undefined,
          node = undefined :: node(),
          port = 0         :: inet:port_number(),
          ip   = undefined :: inet:ip_address() | undefined,
          ua   = <<"">>    :: binary(),
          created_at = 0   :: non_neg_integer() %% unix timestamp
         }).
