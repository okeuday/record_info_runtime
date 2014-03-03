-module(record_info_runtime_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([t_record_info_fieldtypes_test00/1,
         t_record_info_fieldtypes_test01/1]).

-compile([{parse_transform, record_info_runtime}]).

-record(test01,
    {
        field00 :: maybe_improper_list(integer(), integer()),
        field01 :: {integer(), integer()}
    }).

-record(test00,
    {
        field00 = 1,
        field01 = undefined :: list(pos_integer()),
        field02 :: string(),
        field03 :: <<_:128>>,
        field04 :: 0..2,
        field05 :: dict(),
        field06 :: erlang:timestamp(),
        field07 :: list(),
        field08 :: list(tuple()),
        field09 :: list({integer(), string(), atom()}),
        field10 :: binary(),
        field11 :: tuple(),
        field12 :: integer(),
        field13 :: pos_integer(),
        field14 :: non_neg_integer(),
        field15 :: atom(),
        field16 :: list(integer()),
        field17 :: list(#test01{}),
        field18 :: #test01{},
        field19 :: #test01{} | undefined
    }).

all() ->
    [{group, record_info_fieldtypes}].

groups() ->
    [{record_info_fieldtypes, [],
      [t_record_info_fieldtypes_test00,
       t_record_info_fieldtypes_test01]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 5100}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

group(_GroupName) ->
    [].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_record_info_fieldtypes_test00(_Config) ->
    FieldTypes = [{field00,undefined},
                  {field01,{list,[pos_integer]}},
                  {field02,{string,[]}},
                  {field03,{binary,[128,0]}},
                  {field04,{range,[0,2]}},
                  {field05,{dict,[]}},
                  {field06,{{erlang,timestamp},[]}},
                  {field07,{list,[]}},
                  {field08,{list,[{tuple,[any]}]}},
                  {field09,{list,[{tuple,[integer,string,atom]}]}},
                  {field10,{binary,[]}},
                  {field11,{tuple,[any]}},
                  {field12,{integer,[]}},
                  {field13,{pos_integer,[]}},
                  {field14,{non_neg_integer,[]}},
                  {field15,{atom,[]}},
                  {field16,{list,[integer]}},
                  {field17,{list,[{record,[test01]}]}},
                  {field18,{record,[test01]}},
                  {field19,{union,[{record,[test01]},undefined]}}],
    FieldTypes = record_info_fieldtypes(test00),
    ok.

t_record_info_fieldtypes_test01(_Config) ->
    FieldTypes = [{field00,{maybe_improper_list,[integer,integer]}},
                  {field01,{tuple,[integer,integer]}}],
    FieldTypes = record_info_fieldtypes(test01),
    ok.

