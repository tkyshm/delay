-record(job, {
          uid = undefined :: binary() | undefined,
          delay_time = 0 :: non_neg_integer(),
          event = <<"">> :: binary()
         }).
