%% app generated at {2014,11,16} {19,40,9}
{application,bidder,
             [{description,[]},
              {vsn,"1"},
              {id,[]},
              {modules,[bidder_app,bidder_sup,bidder_worker,
                        bidswitch_bid_request_handler,
                        bidswitch_bid_request_parser,
                        bidswitch_bid_response_builder,
                        openrtb2_bid_request_handler,
                        openrtb2_bid_request_parser,router]},
              {registered,[]},
              {applications,[kernel,stdlib,mnesia,cowboy]},
              {included_applications,[]},
              {env,[{http_port,8080}]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{bidder_app,[]}}]}.

