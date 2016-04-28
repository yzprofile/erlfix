-module(fix_group).
-author('Max Lapshin <max@maxidoors.ru>').


-include("business.hrl").

-export([postprocess/1]).


postprocess(#market_data_snapshot_full_refresh{fields = Fields} = Record) ->
    %% To compatible with different FIX protocol
    Record#market_data_snapshot_full_refresh{
      md_entries=[], fields=Fields};

postprocess(#execution_report{cl_ord_id = ClOrdId,
                              orig_cl_ord_id = OrigClOrdId} = Record) ->
    Record#execution_report{cl_ord_id = to_i(ClOrdId),
                            orig_cl_ord_id = to_i(OrigClOrdId)};

postprocess(#order_cancel_reject{cl_ord_id = ClOrdId,
                                 orig_cl_ord_id = OrigClOrdId} = Record) ->
    Record#order_cancel_reject{cl_ord_id = to_i(ClOrdId),
                               orig_cl_ord_id = to_i(OrigClOrdId)};

postprocess(Record) ->
    Record.


to_i(undefined) -> undefined;
to_i(Bin) when is_binary(Bin) ->
    try list_to_integer(binary_to_list(Bin)) of
        Result -> Result
    catch
        _Error:_ -> Bin
    end.
