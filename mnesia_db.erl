-module(mnesia_db).
-export([setup/0,get_data/0]).
-include("message.hrl").


setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(message,[{attributes,record_info(fields,message)}]),

    F = fun () ->
        mnesia:write(#message{sender="b43abcf3-7a90-4af6-87e2-502ea38e8142",receiver="20a5e73e-8cc0-4494-a217-53fc70f9419a",text="hi!"})
        end,
    
    mnesia:transaction(F).

get_data() ->
    mnesia:transaction(fun()->mnesia:read(message,"b43abcf3-7a90-4af6-87e2-502ea38e8142") end).
