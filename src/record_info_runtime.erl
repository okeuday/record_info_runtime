%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Record Info Runtime Parse Transform==
%%% Insert private functions into a module if the module contains any
%%% record definitions (including those from header files).  The private
%%% functions inserted are record_info_size/1 and record_info_fields/1, so
%%% the record_info(size, _) and record_info(fields, _) can be called at
%%% runtime, but without exported functions.  The unused function warning
%%% is automatically turned off for the inserted functions, so they
%%% don't necessarily need to be used.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013 Michael Truog
%%% @version 1.0.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(record_info_runtime).
-author('mjtruog [at] gmail (dot) com').

-export([parse_transform/2]).

-record(state,
    {
        records = [], % stored in reverse order
        types = dict:new()
    }).

parse_transform(Forms, _CompileOptions) ->
    forms_process(Forms, [], #state{}).

forms_process([{eof, _} = EOF], L,
              #state{records = []}) ->
    % nothing to do (no records found)
    lists:reverse([EOF | L]);
forms_process([{eof, _} = EOF], L,
              #state{records = Records,
                     types = Types}) ->
    [{attribute, _, file, _} = FILE |
     NewForms] = lists:reverse([EOF,
                                record_info_size(Records),
                                record_info_fields(Records),
                                record_info_fieldtypes(Records, Types),
                                record_new(Records),
                                records(Records) | L]),
    [FILE, record_info_f_nowarn() | NewForms];
forms_process([{attribute, _Line, record,
                {Name, _Fields}} = H | Forms], L,
              #state{records = Records} = State) ->
  forms_process(Forms, [H | L],
                State#state{records = [Name | Records]});
forms_process([{attribute, _Line, type,
                {{record, Name}, Fields, _}} = H | Forms], L,
              State) ->
  forms_process(Forms, [H | L],
                type_record_fields(Fields, Name, State));
forms_process([H | Forms], L, State) ->
    forms_process(Forms, [H | L], State).

record_info_f_nowarn() ->
    {attribute, 1, compile,
     {nowarn_unused_function,
      [{records, 0},
       {record_new, 1},
       {record_info_fieldtypes, 1},
       {record_info_fields, 1},
       {record_info_size, 1}]}}.

records(Records) ->
    {function, 1, records, 0,
     [{clause, 1, [], [],
       [records_f(lists:reverse(Records))]}]}.

records_f([Name]) ->
    {cons, 1, {atom, 1, Name}, {nil, 1}};
records_f([Name | Records]) ->
    {cons, 1, {atom, 1, Name}, records_f(Records)}.

record_new(Records) ->
    {function, 1, record_new, 1,
     record_new_f([], Records)}.

record_new_f(L, []) ->
    L;
record_new_f(L, [Name | Records]) ->
    record_new_f([{clause, 1,
                   [{atom, 1, Name}], [],
                   [{record, 1, Name, []}]} | L], Records).

record_info_fieldtypes(Records, Types) ->
    {function, 1, record_info_fieldtypes, 1,
     record_info_fieldtypes_f([], Records, Types)}.

record_info_fieldtypes_f_entry_types([]) ->
    {nil, 1};
record_info_fieldtypes_f_entry_types([TypeInfo | TypeList]) ->
    {cons, 1,
     record_info_fieldtypes_f_entry_typeinfo(TypeInfo, false),
     record_info_fieldtypes_f_entry_types(TypeList)}.

record_info_fieldtypes_f_entry_typeinfo_type({M, T})
    when is_atom(M), is_atom(T) ->
    {tuple, 1, [{atom, 1, M}, {atom, 1, T}]};
record_info_fieldtypes_f_entry_typeinfo_type(T)
    when is_atom(T) ->
    {atom, 1, T}.

record_info_fieldtypes_f_entry_typeinfo({Type, []}, false) ->
    record_info_fieldtypes_f_entry_typeinfo_type(Type);
record_info_fieldtypes_f_entry_typeinfo({Type, TypeList}, _) ->
    {tuple, 1,
     [record_info_fieldtypes_f_entry_typeinfo_type(Type),
      record_info_fieldtypes_f_entry_types(TypeList)]};
record_info_fieldtypes_f_entry_typeinfo(Type, _)
    when is_atom(Type) ->
    % e.g.: any
    {atom, 1, Type};
record_info_fieldtypes_f_entry_typeinfo({Type, _Line, Constant}, _)
    when is_atom(Type) ->
    % e.g.: {integer, 1, 128}
    {Type, 1, Constant};
record_info_fieldtypes_f_entry_typeinfo(Literal, _) ->
    exit({badarg, Literal}).
    %{string, 1, lists:flatten(io_lib:format("~p",[Literal]))}.

record_info_fieldtypes_f_entry(FieldName, undefined) ->
    {tuple, 1,
     [{atom, 1, FieldName},
      {atom, 1, undefined}]};
record_info_fieldtypes_f_entry(FieldName, TypeInfo) ->
    {tuple, 1,
     [{atom, 1, FieldName},
      record_info_fieldtypes_f_entry_typeinfo(TypeInfo, true)]}.

record_info_fieldtypes_f_entries([{FieldName, TypeInfo}]) ->
    {cons, 1, record_info_fieldtypes_f_entry(FieldName, TypeInfo),
     {nil, 1}};
record_info_fieldtypes_f_entries([{FieldName, TypeInfo} | L]) ->
    {cons, 1, record_info_fieldtypes_f_entry(FieldName, TypeInfo),
     record_info_fieldtypes_f_entries(L)}.

record_info_fieldtypes_f(L, [], _Types) ->
    L;
record_info_fieldtypes_f(L, [Name | Records], Types) ->
    ClauseBody = case dict:find(Name, Types) of
        {ok, FieldTypes} ->
            [record_info_fieldtypes_f_entries(lists:reverse(FieldTypes))];
        error ->
            [{lc, 1,
              {tuple, 1,
               [{var, 1, 'FieldName'},
                {atom, 1, undefined}]},
              [{generate, 1,
                {var, 1, 'FieldName'},
                {call, 1,
                 {atom, 1, record_info},
                 [{atom, 1, fields},
                  {atom, 1, Name}]}}]}]
    end,
    record_info_fieldtypes_f([{clause, 1,
                               [{atom, 1, Name}], [],
                               ClauseBody} | L], Records, Types).

record_info_fields(Records) ->
    {function, 1, record_info_fields, 1,
     record_info_fields_f([], Records)}.

record_info_fields_f(L, []) ->
    L;
record_info_fields_f(L, [Name | Records]) ->
    record_info_fields_f([{clause, 1,
                           [{atom, 1, Name}], [],
                           [{call, 1,
                             {atom, 1, record_info},
                             [{atom, 1, fields},
                              {atom, 1, Name}]}]} | L], Records).

record_info_size(Records) ->
    {function, 1, record_info_size, 1,
     record_info_size_f([], Records)}.

record_info_size_f(L, []) ->
    L;
record_info_size_f(L, [Name | Records]) ->
    record_info_size_f([{clause, 1,
                         [{atom, 1, Name}], [],
                         [{call, 1,
                           {atom, 1, record_info},
                           [{atom, 1, size},
                            {atom, 1, Name}]}]} | L], Records).

type_record_field_info({type, _Line1, union,
                        [{atom, _Line2, undefined}, TypeInfo]}) ->
    type_record_field_info(TypeInfo);
type_record_field_info({type, _Line, Type, any}) ->
    {Type, [any]};
type_record_field_info({type, _Line, Type, TypeList}) ->
    {Type, [type_record_field_info(T) || T <- TypeList]};
type_record_field_info({remote_type, _Line1,
                        [{atom, _Line2, Module},
                         {atom, _Line3, Type}, Arguments]}) ->
    {{Module, Type}, Arguments};
type_record_field_info(Literal) ->
    Literal.

type_record_fields([], _RecordName, State) ->
    State;
type_record_fields([{record_field, _Line1,
                     {atom, _Line2, FieldName}, _Value} | Fields],
                   RecordName, #state{types = Types} = State) ->
    Entry = {FieldName, undefined},
    NewTypes = dict:update(RecordName, fun(L) ->
        [Entry | L]
    end, [Entry], Types),
    type_record_fields(Fields, RecordName, State#state{types = NewTypes});
type_record_fields([{typed_record_field, RecordField, TypeInfo} | Fields],
                   RecordName, #state{types = Types} = State) ->
    FieldName = case RecordField of
        {record_field, _Line1,
         {atom, _Line2, Name}, _Value} ->
            Name;
        {record_field, _Line1,
         {atom, _Line2, Name}} ->
            Name
    end,
    Entry = {FieldName, type_record_field_info(TypeInfo)},
    NewTypes = dict:update(RecordName, fun(L) ->
        [Entry | L]
    end, [Entry], Types),
    type_record_fields(Fields, RecordName, State#state{types = NewTypes}).

