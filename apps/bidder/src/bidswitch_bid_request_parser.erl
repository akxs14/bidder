-module(bidswitch_bid_request_parser).

-export([parse/1]).

%% ===================================================================
%% API functions
%% ===================================================================

parse(JsonBidReq) ->
  {DecodedBidReq} = jiffy:decode(JsonBidReq),
  ParsedBidReq = #{
    id => get_id(DecodedBidReq),
    imp => get_impressions(DecodedBidReq),
    device => get_device(DecodedBidReq),
    user => get_user(DecodedBidReq),
    at => get_auction_type(DecodedBidReq),
    site => get_site(DecodedBidReq),
    app => get_app(DecodedBidReq),
    bcat => get_blocked_advertiser_categories(DecodedBidReq),
    badv => get_blocked_domains(DecodedBidReq),    
    wseat => get_buyer_seats(DecodedBidReq),
    cur => get_allowed_currencies(DecodedBidReq),
    ext => get_ext(DecodedBidReq)
  },
  ParsedBidReq.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% parse the root object
get_id(DecodedBidReq) ->
  proplists:get_value(<<"id">>,DecodedBidReq, none).

get_auction_type(DecodedBidReq) ->
  proplists:get_value(<<"at">>,DecodedBidReq, none).

get_buyer_seats(DecodedBidReq) ->
  proplists:get_value(<<"wseat">>,DecodedBidReq, []).

get_allowed_currencies(DecodedBidReq) ->
  proplists:get_value(<<"cur">>,DecodedBidReq, []).

get_blocked_advertiser_categories(DecodedBidReq) ->
  proplists:get_value(<<"bcat">>,DecodedBidReq, []).

get_blocked_domains(DecodedBidReq) ->
  proplists:get_value(<<"badv">>,DecodedBidReq, []).

get_ext(DecodedBidReq) ->
  proplists:get_value(<<"ext">>,DecodedBidReq, none).

get_impressions(DecodedBidReq) ->
   JsonImps = proplists:get_value(<<"imp">>,DecodedBidReq, none),
   parse_impressions(JsonImps,[]).

%% parse impression objects
parse_impressions([], ParsedImps) ->
  ParsedImps;
parse_impressions([{HeadImp} | JsonImps], ParsedImps) ->
  parse_impressions(JsonImps, [parse_impression(HeadImp)|ParsedImps]).

parse_impression(DecodedImp) ->
  #{
    id => get_id(DecodedImp),
    banner => get_banner(<<"banner">>, DecodedImp),
    video => get_video(DecodedImp),
    bidfloor => get_bid_floor(DecodedImp),
    pmp => get_private_marketplace(DecodedImp),
    ext => get_impression_ext(DecodedImp)
  }.

get_private_marketplace(DecodedImp) ->
  #{
    private_auction => proplists:get_value(<<"private_auction">>,DecodedImp, 0),
    deals => proplists:get_value(<<"deals">>,DecodedImp, 0)    
  }.

get_bid_floor(DecodedImp) ->
  proplists:get_value(<<"bidfloor">>,DecodedImp, 0).


%% parse impression ext object
get_impression_ext(DecodedImp) ->
  #{
    rubicon => get_rubicon_ext(proplists:get_value(<<"rubicon">>,DecodedImp, none)),
    google => get_google_ext(proplists:get_value(<<"google">>,DecodedImp, none)),
    yieldone => get_yieldone_ext(proplists:get_value(<<"yieldone">>,DecodedImp, none)),
    site_size_session_count => proplists:get_value(<<"site_size_session_count">>,DecodedImp, none)
  }.

%% parse rubicon ext object
get_rubicon_ext(_RubiconExt) ->
  #{
  }.

%% parse google ext object
get_google_ext(DecodedGoogleExt) ->
  #{
    excluded_attribute => proplists:get_value(<<"excluded_attribute">>,DecodedGoogleExt, none),
    allowed_vendor_type => proplists:get_value(<<"allowed_vendor_type">>,DecodedGoogleExt, none)
  }.

%% parse google ext object
get_yieldone_ext(DecodedYieldOneExt) ->
  #{
    inventory_class => proplists:get_value(<<"inventory_class">>,DecodedYieldOneExt, none)
  }.


%% parse banner objects
get_banner(ElementName, DecodedImp) ->
  case proplists:lookup(ElementName,DecodedImp) of
    none ->
      #{};
    {_, {DecodedBanner}} ->
      #{
        id => get_id(DecodedBanner),
        w => get_width(DecodedBanner),
        h => get_height(DecodedBanner),
        btype => get_blocked_creative_types(DecodedBanner),
        pos => get_position(DecodedBanner),
        topframe => get_topframe(DecodedBanner),
        mimes => get_mime_whitelist(DecodedBanner),
        expdir => get_expandable_ad_properties(DecodedBanner),
        api => get_banner_api(DecodedBanner)
      }
  end.

get_width(DecodedBanner) ->
  proplists:get_value(<<"w">>,DecodedBanner, none).

get_height(DecodedBanner) ->
  proplists:get_value(<<"h">>,DecodedBanner, none).

get_position(DecodedBanner) ->
  proplists:get_value(<<"pos">>,DecodedBanner, unknown).

get_topframe(DecodedBanner) ->
  proplists:get_value(<<"topframe">>,DecodedBanner, 0).

get_blocked_creative_types(DecodedBanner) ->
  proplists:get_value(<<"btype">>,DecodedBanner, []).

get_mime_whitelist(DecodedBanner) ->
  proplists:get_value(<<"mimes">>,DecodedBanner, []).

get_expandable_ad_properties(DecodedBanner) ->
  proplists:get_value(<<"expdir">>,DecodedBanner, []).

get_banner_api(DecodedBanner) ->
  proplists:get_value(<<"api">>,DecodedBanner, []).


%% parse video objects
get_video(DecodedImp) ->
  case proplists:lookup(<<"video">>,DecodedImp) of
    none ->
      #{};
    {_, {DecodedVideo}} ->
      #{
        mimes => get_mime_whitelist(DecodedVideo),
        linearity => get_linearity(DecodedVideo),
        minduration => get_min_duration(DecodedVideo),
        maxduration => get_max_duration(DecodedVideo),
        protocols => get_protocols(DecodedVideo),
        w => get_width(DecodedVideo),
        h => get_height(DecodedVideo),
        startdelay => get_start_delay(DecodedVideo),
        battr => get_blocked_creative_attributes(DecodedVideo),
        minbitrate => get_min_bitrate(DecodedVideo),
        maxbitrate => get_max_bitrate(DecodedVideo),
        api => get_banner_api(DecodedVideo),
        companionad => get_companion_ads(DecodedVideo),
        companiontype => get_companion_type(DecodedVideo),
        ext => get_video_ext(DecodedVideo)
      }
  end.

%% parse video extended object
get_video_ext(DecodedVideo) ->
  #{
    skippable => proplists:get_value(<<"skippable">>,DecodedVideo, none)
  }.

get_blocked_creative_attributes(DecodedVideo) ->
  proplists:get_value(<<"battr">>,DecodedVideo, none).

get_linearity(DecodedVideo) ->
  proplists:get_value(<<"linearity">>,DecodedVideo, none).

get_min_duration(DecodedVideo) ->
  proplists:get_value(<<"minduration">>,DecodedVideo, none).

get_max_duration(DecodedVideo) ->
  proplists:get_value(<<"maxduration">>,DecodedVideo, none).

get_protocols(DecodedVideo) ->
  proplists:get_value(<<"protocols">>,DecodedVideo, none).

get_start_delay(DecodedVideo) ->
  proplists:get_value(<<"startdelay">>,DecodedVideo, none).

get_min_bitrate(DecodedVideo) ->
  proplists:get_value(<<"minbitrate">>,DecodedVideo, none).

get_max_bitrate(DecodedVideo) ->
  proplists:get_value(<<"maxbitrate">>,DecodedVideo, none).

get_companion_ads(DecodedVideo) ->
  get_banner(<<"companionad">>, DecodedVideo).

get_companion_type(DecodedVideo) ->
  get_banner(<<"companiontype">>, DecodedVideo).


%% parse site object
get_site(DecodedBidReq) ->
  case proplists:lookup(<<"site">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedSite}} ->
      #{
        id => get_id(DecodedSite),
        domain => get_domain(DecodedSite),
        cat => get_IAB_categories(DecodedSite),
        page => get_page(DecodedSite),
        publisher => get_publisher(DecodedSite),
        ext => get_site_ext(DecodedSite)
      }
  end.

get_name(DecodedSite) ->
  proplists:get_value(<<"name">>,DecodedSite, none).

get_domain(DecodedSite) ->
  proplists:get_value(<<"domain">>,DecodedSite, none).

get_IAB_categories(DecodedSite) ->
  proplists:get_value(<<"cat">>,DecodedSite, []).

get_page(DecodedSite) ->
  proplists:get_value(<<"page">>,DecodedSite, none).

get_keywords(DecodedSite) ->
  proplists:get_value(<<"keywords">>,DecodedSite, none).

get_site_ext(DecodedSite) ->
  #{
    mobile_site => proplists:get_value(<<"mobile_site">>,DecodedSite, none)
  }.

%% parse app object
get_app(DecodedBidReq) ->
  case proplists:lookup(<<"site">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedApp}} ->
      #{
        id => get_id(DecodedApp),
        name => get_name(DecodedApp),
        domain => get_domain(DecodedApp),
        cat => get_IAB_categories(DecodedApp),
        bundle => get_bundle(DecodedApp),
        publisher => get_publisher(DecodedApp),
        storeurl => get_app_store_url(DecodedApp),
        ver => get_version(DecodedApp)
      }
  end.

get_app_store_url(DecodedApp) ->
  proplists:get_value(<<"storeurl">>,DecodedApp, none).

get_version(DecodedApp) ->
  proplists:get_value(<<"ver">>,DecodedApp, none).

get_bundle(DecodedApp) ->
  proplists:get_value(<<"bundle">>,DecodedApp, none).

get_paid(DecodedApp) ->
  proplists:get_value(<<"paid">>,DecodedApp, none).


%% parse publisher object
get_publisher(DecodedSite) ->
  case proplists:lookup(<<"publisher">>,DecodedSite) of
    none ->
      #{};
    {_, {DecodedPublisher}} ->
      #{
        id => get_id(DecodedPublisher),
        name => get_name(DecodedPublisher),
        cat => get_IAB_categories(DecodedPublisher),
        domain => get_domain(DecodedPublisher)
      }
  end.

%% parse content object
get_content(DecodedSite) ->
  case proplists:lookup(<<"content">>,DecodedSite) of
    none ->
      #{};
    {_, {DecodedContent}} ->
      #{
        id => get_id(DecodedContent),
        episode => get_episode(DecodedContent),
        title => get_title(DecodedContent),
        series => get_series(DecodedContent),
        season => get_season(DecodedContent),
        url => get_url(DecodedContent),
        cat => get_IAB_categories(DecodedContent),
        videoquality => get_video_quality(DecodedContent),
        keywords => get_keywords(DecodedContent),
        contentrating => get_content_rating(DecodedContent),
        userrating => get_user_rating(DecodedContent),
        context => get_context(DecodedContent),
        livestram => get_live_stream(DecodedContent),
        sourcerelationship => get_source_relationship(DecodedContent),
        producer => get_producer(DecodedContent),
        len => get_len(DecodedContent)
      }
  end.

get_episode(DecodedContent) ->
  proplists:get_value(<<"episode">>,DecodedContent, none).

get_title(DecodedContent) ->
  proplists:get_value(<<"title">>,DecodedContent, none).

get_series(DecodedContent) ->
  proplists:get_value(<<"series">>,DecodedContent, none).

get_season(DecodedContent) ->
  proplists:get_value(<<"season">>,DecodedContent, none).

get_url(DecodedContent) ->
  proplists:get_value(<<"url">>,DecodedContent, none).

get_video_quality(DecodedContent) ->
  proplists:get_value(<<"videoquality">>,DecodedContent, none).

get_content_rating(DecodedContent) ->
  proplists:get_value(<<"contentrating">>,DecodedContent, none).

get_user_rating(DecodedContent) ->
  proplists:get_value(<<"userrating">>,DecodedContent, none).

get_context(DecodedContent) ->
  proplists:get_value(<<"context">>,DecodedContent, none).

get_live_stream(DecodedContent) ->
  proplists:get_value(<<"livestream">>,DecodedContent, none).

get_source_relationship(DecodedContent) ->
  proplists:get_value(<<"sourcerelationship">>,DecodedContent, none).

get_len(DecodedContent) ->
  proplists:get_value(<<"len">>,DecodedContent, none).


%% parse producer object
get_producer(DecodedContent) ->
  case proplists:lookup(<<"producer">>,DecodedContent) of
    none ->
      #{};
    {_, {DecodedProducer}} ->
      #{
        id => get_id(DecodedProducer),
        name => get_name(DecodedProducer),
        cat => get_IAB_categories(DecodedProducer),
        domain => get_domain(DecodedProducer)
      }
  end.


%% parse device object
get_device(DecodedBidReq) ->
  case proplists:lookup(<<"device">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedDevice}} ->
      #{
        ip => get_ip(DecodedDevice),
        geo => get_geo(DecodedDevice),
        ua => get_user_agent(DecodedDevice),
        language => get_language(DecodedDevice),
        carrier => get_carrier(DecodedDevice),
        connectiontype => get_connection_type(DecodedDevice),
        didsha1 => get_didsha1(DecodedDevice),
        didmd5 => get_didmd5(DecodedDevice),
        dpidsha1 => get_dpidsha1(DecodedDevice),
        dpidmd5 => get_dpidmd5(DecodedDevice),
        dnt => get_do_not_track(DecodedDevice),
        ifa => get_ifa(DecodedDevice),
        make => get_make(DecodedDevice),
        model => get_model(DecodedDevice),
        os => get_os(DecodedDevice),
        osv => get_osv(DecodedDevice)
      }
  end.

get_ifa(DecodedDevice) ->
  proplists:get_value(<<"ifa">>,DecodedDevice, none).

get_do_not_track(DecodedDevice) ->
  proplists:get_value(<<"dnt">>,DecodedDevice, none).

get_user_agent(DecodedDevice) ->
  proplists:get_value(<<"ua">>,DecodedDevice, none).

get_ip(DecodedDevice) ->
  proplists:get_value(<<"ip">>,DecodedDevice, none).

get_didsha1(DecodedDevice) ->
  proplists:get_value(<<"didsha1">>,DecodedDevice, none).

get_didmd5(DecodedDevice) ->
  proplists:get_value(<<"didmd5">>,DecodedDevice, none).

get_dpidsha1(DecodedDevice) ->
  proplists:get_value(<<"dpidsha1">>,DecodedDevice, none).

get_dpidmd5(DecodedDevice) ->
  proplists:get_value(<<"dpidmd5">>,DecodedDevice, none).

get_carrier(DecodedDevice) ->
  proplists:get_value(<<"carrier">>,DecodedDevice, none).

get_language(DecodedDevice) ->
  proplists:get_value(<<"language">>,DecodedDevice, none).

get_make(DecodedDevice) ->
  proplists:get_value(<<"make">>,DecodedDevice, none).

get_model(DecodedDevice) ->
  proplists:get_value(<<"model">>,DecodedDevice, none).

get_os(DecodedDevice) ->
  proplists:get_value(<<"os">>,DecodedDevice, none).

get_osv(DecodedDevice) ->
  proplists:get_value(<<"osv">>,DecodedDevice, none).

get_connection_type(DecodedDevice) ->
  proplists:get_value(<<"connectiontype">>,DecodedDevice, none).

%% parse geo object
get_geo(DecodedBidReq) ->
  case proplists:lookup(<<"geo">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedGeoData}} ->
      #{
        lat => get_lat(DecodedGeoData),
        lon => get_lon(DecodedGeoData),
        country => get_country(DecodedGeoData),
        geo => get_geo(DecodedGeoData),
        region => get_region(DecodedGeoData),
        city => get_city(DecodedGeoData),
        zip => get_zip(DecodedGeoData)
      }
  end.

get_lat(DecodedGeoData) ->
  proplists:get_value(<<"lat">>,DecodedGeoData, none).

get_lon(DecodedGeoData) ->
  proplists:get_value(<<"lon">>,DecodedGeoData, none).

get_country(DecodedGeoData) ->
  proplists:get_value(<<"country">>,DecodedGeoData, none).

get_region(DecodedGeoData) ->
  proplists:get_value(<<"region">>,DecodedGeoData, none).

get_city(DecodedGeoData) ->
  proplists:get_value(<<"city">>,DecodedGeoData, none).

get_zip(DecodedGeoData) ->
  proplists:get_value(<<"zip">>,DecodedGeoData, none).


%% parse user object
get_user(DecodedBidReq) ->
  case proplists:lookup(<<"content">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedUser}} ->
      #{
        id => get_id(DecodedUser),
        buyeruid => get_buyer_user_id(DecodedUser)
      }
  end.

get_buyer_user_id(DecodedUser) ->
  proplists:get_value(<<"buyeruid">>,DecodedUser, none).

%% parse data object
get_data(DecodedUser) ->
   JsonDataObjs = proplists:get_value(<<"data">>,DecodedUser, none),
   parse_data_objs(JsonDataObjs,[]).

%% parse segments objects
parse_data_objs([], JsonDataObjs) ->
  JsonDataObjs;
parse_data_objs([{HeadDataObj} | JsonDataObjs], ParsedDataObjs) ->
  parse_data_objs(JsonDataObjs, [parse_data_obj(HeadDataObj)|ParsedDataObjs]).

parse_data_obj(DecodedDataObj) ->
  #{
    id => get_id(DecodedDataObj),
    name => get_name(DecodedDataObj),
    segment => get_segments(DecodedDataObj)
  }.


%% parse segment object
get_segments(DecodedData) ->
   JsonSegs = proplists:get_value(<<"segment">>,DecodedData, none),
   parse_segments(JsonSegs,[]).

%% parse segments objects
parse_segments([], ParsedSegs) ->
  ParsedSegs;
parse_segments([{HeadSeg} | JsonSegs], ParsedSegs) ->
  parse_segments(JsonSegs, [parse_segment(HeadSeg)|ParsedSegs]).

parse_segment(DecodedSeg) ->
  #{
    id => get_id(DecodedSeg),
    name => get_name(DecodedSeg),
    value => get_value(DecodedSeg)
  }.

get_value(DecodedSeg) ->
  proplists:get_value(<<"value">>,DecodedSeg, none).
