-record(job, {
          uid = undefined :: binary() | undefined,
          pid = undefined :: pid() | undefined,
          delay_time = 0 :: non_neg_integer(),
          event = <<"">> :: binary()
         }).
