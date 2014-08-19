-module(openrtb2_bid_request_parser).

-export([parse/1]).

%% ===================================================================
%% API functions
%% ===================================================================

parse(JsonBidReq) ->
  {DecodedBidReq} = jiffy:decode(JsonBidReq),
  ParsedBidReq = #{
    id => get_id(DecodedBidReq),
    imp => get_impressions(DecodedBidReq),
    site => get_site(DecodedBidReq),
    app => get_app(DecodedBidReq),
    device => get_device(DecodedBidReq),    
    user => get_user(DecodedBidReq),
    at => get_auction_type(DecodedBidReq),
    tmax => get_max_time(DecodedBidReq),
    wseat => get_buyer_seats(DecodedBidReq),
    allimps => get_all_impressions(DecodedBidReq),
    cur => get_allowed_currencies(DecodedBidReq),
    bcat => get_blocked_advertiser_categories(DecodedBidReq),
    badv => get_blocked_domains(DecodedBidReq),    
    ext => get_ext(DecodedBidReq)
  },
  io:format("~p~n",[ParsedBidReq]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% parse the root object
get_id(DecodedBidReq) ->
  proplists:get_value(<<"id">>,DecodedBidReq, none).

get_auction_type(DecodedBidReq) ->
  proplists:get_value(<<"at">>,DecodedBidReq, none).

get_max_time(DecodedBidReq) ->
  proplists:get_value(<<"tmax">>,DecodedBidReq, none).

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

get_all_impressions(DecodedBidReq) ->
  proplists:get_value(<<"allimps">>,DecodedBidReq, 0).

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
    displaymanager => get_display_manager(DecodedImp),
    displaymanagerserver => get_display_manager_server(DecodedImp),
    instl => get_interstitial(DecodedImp),
    tagid => get_tag_id(DecodedImp),
    bidfloor => get_bid_floor(DecodedImp),
    bidfloorcur => get_bid_floor_currency(DecodedImp),
    ext => get_ext(DecodedImp),
    video => get_video(DecodedImp)
  }.

get_display_manager(DecodedImp) ->
  proplists:get_value(<<"displaymanager">>,DecodedImp, none).

get_display_manager_server(DecodedImp) ->
  proplists:get_value(<<"displaymanagerserver">>,DecodedImp, none).

get_interstitial(DecodedImp) ->
  proplists:get_value(<<"instl">>,DecodedImp, 0).

get_tag_id(DecodedImp) ->
  proplists:get_value(<<"tagid">>,DecodedImp, none).

get_bid_floor(DecodedImp) ->
  proplists:get_value(<<"bidfloor">>,DecodedImp, 0).

get_bid_floor_currency(DecodedImp) ->
  proplists:get_value(<<"bidfloorcur">>,DecodedImp, <<"USD">>).


%% parse banner objects
get_banner(ElementName, DecodedImp) ->
  case proplists:lookup(ElementName,DecodedImp) of
    none ->
      #{};
    {_, {DecodedBanner}} ->
      #{
        w => get_width(DecodedBanner),
        h => get_height(DecodedBanner),
        id => get_id(DecodedBanner),
        pos => get_position(DecodedBanner),
        topframe => get_topframe(DecodedBanner),
        btype => get_blocked_creative_types(DecodedBanner),
        battr => get_blocked_creative_attributes(DecodedBanner),
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

get_blocked_creative_attributes(DecodedBanner) ->
  proplists:get_value(<<"battr">>,DecodedBanner, []).

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
        w => get_width(DecodedVideo),
        h => get_height(DecodedVideo),
        linearity => get_linearity(DecodedVideo),
        minduration => get_min_duration(DecodedVideo),
        maxduration => get_max_duration(DecodedVideo),
        protocol => get_protocol(DecodedVideo),
        mimes => get_mime_whitelist(DecodedVideo),
        startdelay => get_start_delay(DecodedVideo),
        sequence => get_sequence(DecodedVideo),
        battr => get_blocked_creative_attributes(DecodedVideo),
        maxextended => get_max_extended_video_duration(DecodedVideo),
        minbitrate => get_min_bitrate(DecodedVideo),
        maxbitrate => get_max_bitrate(DecodedVideo),
        boxingallowed => get_boxing_allowed(DecodedVideo),
        playbackmethod => get_playback_methods(DecodedVideo),
        delivery => get_delivery_methods(DecodedVideo),
        pos => get_position(DecodedVideo),
        companionad => get_companion_ads(DecodedVideo),
        api => get_banner_api(DecodedVideo)
      }
  end.

get_linearity(DecodedVideo) ->
  proplists:get_value(<<"linearity">>,DecodedVideo, none).

get_min_duration(DecodedVideo) ->
  proplists:get_value(<<"minduration">>,DecodedVideo, none).

get_max_duration(DecodedVideo) ->
  proplists:get_value(<<"maxduration">>,DecodedVideo, none).

get_protocol(DecodedVideo) ->
  proplists:get_value(<<"protocol">>,DecodedVideo, none).

get_start_delay(DecodedVideo) ->
  proplists:get_value(<<"startdelay">>,DecodedVideo, none).

get_sequence(DecodedVideo) ->
  proplists:get_value(<<"sequence">>,DecodedVideo, 1).

get_max_extended_video_duration(DecodedVideo) ->
  proplists:get_value(<<"maxextended">>,DecodedVideo, extension_not_allowed).

get_min_bitrate(DecodedVideo) ->
  proplists:get_value(<<"minbitrate">>,DecodedVideo, none).

get_max_bitrate(DecodedVideo) ->
  proplists:get_value(<<"maxbitrate">>,DecodedVideo, none).

get_boxing_allowed(DecodedVideo) ->
  proplists:get_value(<<"boxingallowed">>,DecodedVideo, 1).

get_playback_methods(DecodedVideo) ->
  proplists:get_value(<<"playbackmethod">>,DecodedVideo, []).

get_delivery_methods(DecodedVideo) ->
  proplists:get_value(<<"delivery">>,DecodedVideo, []).

get_companion_ads(DecodedVideo) ->
  get_banner(<<"companionad">>, DecodedVideo).


%% parse site object
get_site(DecodedBidReq) ->
  case proplists:lookup(<<"site">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedSite}} ->
      #{
        id => get_id(DecodedSite),
        name => get_name(DecodedSite),
        domain => get_domain(DecodedSite),
        cat => get_IAB_categories(DecodedSite),
        sectioncat => get_section_IAB_categories(DecodedSite),
        pagecat => get_page_IAB_categories(DecodedSite),
        page => get_page(DecodedSite),
        privacypolicy => get_privacy_policy(DecodedSite),
        ref => get_referrer_url(DecodedSite),
        search => get_search_string(DecodedSite),
        publisher => get_publisher(DecodedSite),
        content => get_content(DecodedSite),
        keywords => get_keywords(DecodedSite)
      }
  end.

get_name(DecodedSite) ->
  proplists:get_value(<<"name">>,DecodedSite, none).

get_domain(DecodedSite) ->
  proplists:get_value(<<"domain">>,DecodedSite, none).

get_IAB_categories(DecodedSite) ->
  proplists:get_value(<<"cat">>,DecodedSite, []).

get_section_IAB_categories(DecodedSite) ->
  proplists:get_value(<<"sectioncat">>,DecodedSite, []).

get_page_IAB_categories(DecodedSite) ->
  proplists:get_value(<<"pagecat">>,DecodedSite, []).

get_page(DecodedSite) ->
  proplists:get_value(<<"page">>,DecodedSite, none).

get_privacy_policy(DecodedSite) ->
  proplists:get_value(<<"privacypolicy">>,DecodedSite, none).

get_referrer_url(DecodedSite) ->
  proplists:get_value(<<"ref">>,DecodedSite, none).

get_search_string(DecodedSite) ->
  proplists:get_value(<<"search">>,DecodedSite, none).

get_keywords(DecodedSite) ->
  proplists:get_value(<<"keywords">>,DecodedSite, none).


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
        sectioncat => get_section_IAB_categories(DecodedApp),
        pagecat => get_page_IAB_categories(DecodedApp),
        ver => get_version(DecodedApp),
        bundle => get_bundle(DecodedApp),
        privacypolicy => get_privacy_policy(DecodedApp),
        paid => get_paid(DecodedApp),
        publisher => get_publisher(DecodedApp),
        content => get_content(DecodedApp),
        keywords => get_keywords(DecodedApp)
      }
  end.

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
        dnt => get_do_not_track(DecodedDevice),
        ua => get_user_agent(DecodedDevice),
        ip => get_ip(DecodedDevice),
        geo => get_geo(DecodedDevice),
        didsha1 => get_didsha1(DecodedDevice),
        didmd5 => get_didmd5(DecodedDevice),
        dpidsha1 => get_dpidsha1(DecodedDevice),
        dpidmd5 => get_dpidmd5(DecodedDevice),
        ipv6 => get_ipv6(DecodedDevice),
        carrier => get_carrier(DecodedDevice),
        language => get_language(DecodedDevice),
        make => get_make(DecodedDevice),
        model => get_model(DecodedDevice),
        os => get_os(DecodedDevice),
        osv => get_osv(DecodedDevice),
        js => get_js_support(DecodedDevice),
        connectiontype => get_connection_type(DecodedDevice),
        devicetype => get_device_type(DecodedDevice),
        flashver => get_flash_ver(DecodedDevice)
      }
  end.

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

get_ipv6(DecodedDevice) ->
  proplists:get_value(<<"ipv6">>,DecodedDevice, none).

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

get_js_support(DecodedDevice) ->
  proplists:get_value(<<"js">>,DecodedDevice, none).

get_connection_type(DecodedDevice) ->
  proplists:get_value(<<"connectiontype">>,DecodedDevice, none).

get_device_type(DecodedDevice) ->
  proplists:get_value(<<"devicetype">>,DecodedDevice, none).

get_flash_ver(DecodedDevice) ->
  proplists:get_value(<<"flashver">>,DecodedDevice, none).


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
        regionfips104 => get_regionfips104(DecodedGeoData),
        metro => get_metro(DecodedGeoData),
        city => get_city(DecodedGeoData),
        zip => get_zip(DecodedGeoData),
        type => get_type(DecodedGeoData)
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

get_regionfips104(DecodedGeoData) ->
  proplists:get_value(<<"regionfips104">>,DecodedGeoData, none).

get_metro(DecodedGeoData) ->
  proplists:get_value(<<"metro">>,DecodedGeoData, none).

get_city(DecodedGeoData) ->
  proplists:get_value(<<"city">>,DecodedGeoData, none).

get_zip(DecodedGeoData) ->
  proplists:get_value(<<"zip">>,DecodedGeoData, none).

get_type(DecodedGeoData) ->
  proplists:get_value(<<"type">>,DecodedGeoData, none).


%% parse user object
get_user(DecodedBidReq) ->
  case proplists:lookup(<<"content">>,DecodedBidReq) of
    none ->
      #{};
    {_, {DecodedUser}} ->
      #{
        id => get_id(DecodedUser),
        buyeruid => get_buyer_user_id(DecodedUser),
        yob => get_year_of_birth(DecodedUser),
        gender => get_gender(DecodedUser),
        keywords => get_keywords(DecodedUser),
        customdata => get_custom_data(DecodedUser),
        geo => get_geo(DecodedUser),
        data => get_data(DecodedUser)
      }
  end.

get_buyer_user_id(DecodedUser) ->
  proplists:get_value(<<"buyeruid">>,DecodedUser, none).

get_year_of_birth(DecodedUser) ->
  proplists:get_value(<<"yob">>,DecodedUser, none).

get_gender(DecodedUser) ->
  proplists:get_value(<<"gender">>,DecodedUser, none).

get_custom_data(DecodedUser) ->
  proplists:get_value(<<"customdata">>,DecodedUser, none).



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
