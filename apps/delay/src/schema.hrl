-record(job, {
          uid = undefined     :: binary() | undefined,
          pid = undefined     :: pid() | undefined,
          exec_time = 0       :: non_neg_integer(),
          data = undefined    :: term() | undefined,
          webhook = undefined :: binary() | undefined,
          status = waitting   :: waitting | ready
         }).
