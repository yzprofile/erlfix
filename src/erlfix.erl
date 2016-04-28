%% @author Max Lapshin <max@maxidoors.ru>
%% @copyright 2012 Max Lapshin
%% @doc Main module for fix usage.
%%

-module(erlfix).
-author('Max Lapshin <max@maxidoors.ru>').
-include("business.hrl").
-compile(export_all).


%% @doc returns value from application environment, or default value
-spec get_value(any(), any()) -> any().
get_value(Key, Default) ->
    case application:get_env(fix, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

%% @doc returns value from application environment, or raise error
-spec get_value(any()) -> any().
get_value(Key) ->
    case application:get_env(fix, Key) of
        {ok, Value} -> Value;
        undefined -> erlang:error({no_key,Key})
    end.



%% @doc starts fix connection by its well known name
-spec start_exec_conn(Name :: term()) -> {ok, Pid :: pid()}.
start_exec_conn(Name) ->
    Options = erlfix:get_value(Name),
    {ok, Fix} = fix_sup:start_exec_conn(Name, Options),
    {ok, Fix}.

-type fix_message() :: any().


%% @doc fix local reimplementation of UTC as a string
-spec now() -> string().
now() ->
    timestamp(to_date_ms(erlang:now())).

timestamp({{YY,MM,DD},{H,M,S,Milli}}) ->
    % 20120529-10:40:17.578
    lists:flatten(
      io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B.~3..0B",
                    [YY, MM, DD, H, M, S, Milli])).

to_date_ms({Mega, Sec, Micro}) ->
    Seconds = Mega*1000000 + Sec,
    Milli = Micro div 1000,
    {Date, {H,M,S}} = calendar:gregorian_seconds_to_datetime(
                        Seconds + calendar:datetime_to_gregorian_seconds(
                                    {{1970,1,1}, {0,0,0}}
                                   )
                       ),
    {Date, {H,M,S,Milli}}.


-spec utc_ms() -> non_neg_integer().
utc_ms() ->
    utc_ms(erlang:now()).

-spec utc_ms(erlang:timestamp()) -> non_neg_integer().
utc_ms({Mega, Sec, Micro}) ->
    (Mega*1000000+Sec)*1000 + Micro div 1000.



%% @doc packs fix message into binary
-spec pack(atom(), list(), non_neg_integer(), any(), any()) -> iolist().
pack(MessageType, Body, SeqNum, Sender, Target) when
      MessageType =/= undefined, is_list(Body),
      is_integer(SeqNum),
      Sender =/= undefined,
      Target =/= undefined ->

    Header2 = [{msg_type, MessageType},
               {sender_comp_id, Sender},
               {target_comp_id, Target},
               {msg_seq_num, SeqNum}
              % {poss_dup_flag, "N"}
              ] ++ case proplists:get_value(sending_time, Body) of
                       undefined -> [{sending_time, erlfix:now()}];
                       _ -> []
                   end,
    Body1 = encode(Header2 ++ Body),
    BodyLength = iolist_size(Body1),
    Body2 = iolist_to_binary([encode([{begin_string, "FIX.4.4"},
                                      {body_length, BodyLength}]),
                              Body1]),
    CheckSum = checksum(Body2),
    [Body2, encode([{check_sum, CheckSum}])].

checksum(Packet) ->
    S = [lists:sum([Char||<<Char>><=iolist_to_binary(encode(Packet))]) rem 256],
    lists:flatten(
      io_lib:format("~3..0B", S)).

encode(Packet) when is_binary(Packet) -> Packet;
encode([{_K,_V}|_] = Packet) ->
    [[fix_parser:number_by_field(Key),
      "=",
      fix_parser:encode_typed_field(Key, Value), 1] || {Key, Value} <- Packet].

encode_value(Value) when is_number(Value) -> integer_to_list(Value);
encode_value(Value) when is_float(Value) -> io_lib:format("~.2f", [Value]);
encode_value(Value) when is_list(Value) -> Value;
encode_value(Value) when is_binary(Value) -> Value.


dump(Bin) -> iolist_to_binary(Bin).
%% re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).


-spec decode(binary()) -> {ok, fix_message(), binary(), binary()} |
                          {more, non_neg_integer()} |
                          error.
decode(Bin) ->
    try decode0(Bin) of
        Result -> Result
    catch
        error: Error ->
            io:format("~p~n", [Error]),
            error(invalid_fix)
    end.

decode0(Bin) ->
    case decode_fields(Bin) of
        {ok, Fields, MessageBin, Rest} ->
            io:format("D1: ~p~n", [Fields]),
            MSG = fix_parser:decode_message(Fields),
            % io:format("D2: ~p~n", [MSG]),
            {ok, fix_group:postprocess(MSG), MessageBin, Rest};
        Else ->
            Else
    end.

% Ensures message is correct lengh, and checksum is OK
decode_fields(<<"8=FIX.4.4",1,"9=", Bin/binary>> = FullBin) ->
    case binary:split(Bin, <<1>>) of
        [BinLen, Rest1] ->
            BodyLength = list_to_integer(binary_to_list(BinLen)),
            case Rest1 of
                <<Message:BodyLength/binary,
                  "10=",
                  _CheckSum:3/binary,
                  1,
                  Rest2/binary>>
                ->
                    MessageLength = size(FullBin) - size(Rest2),
                    <<MessageBin:MessageLength/binary, _/binary>> = FullBin,
                    io:format("~s~n", [Message]),
                    T = fix_splitter:split(Message),
                    io:format("~p~n", [T]),
                    {ok, fix_splitter:split(Message), MessageBin, Rest2};
                _ ->
                    {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
            end;
        _ ->
            {more, 1}
    end;

decode_fields(<<"8", Rest/binary>>) when size(Rest) < 14 ->
    {more, 14 - size(Rest)};

decode_fields(<<"8", _/binary>>) ->
    {more, 1};

decode_fields(<<>>) ->
    {more, 14};

decode_fields(<<_/binary>>) ->
    error.

-include_lib("eunit/include/eunit.hrl").
