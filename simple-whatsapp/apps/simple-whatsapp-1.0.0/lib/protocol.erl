% This module contains some helper functions to encode and decode the tcp message
-module(protocol).
-export([decode/1]).

% Decodes the message string and returns a tuple where the first item is the messageType
decode(Data) ->
    <<MessageType:8/unsigned-integer,_/binary>> = Data,
    case MessageType of
        % online event
        0 -> 
            <<_:8,UserId:8/bytes>> = Data,
            {online,binary_to_list(UserId)};
        1 -> 
            <<_:8,RecipientId:8/bytes,Message/bytes>> = Data,
            {newmessage,binary_to_list(RecipientId),binary_to_list(Message)}
    end.
    
