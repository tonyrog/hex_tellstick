Tellstick plugin to HEX
=======================

# input events 

TellStick duo and TellStick net (from rev 17) supports input events.

    [{protocol,archtech},{model,codeswitch},{data,uint32()}]

    [{protocol,everflourish},{data,uint32()}]

    [{protocol,x10},{data,uint32()}]

    [{class,sensor},{protocol,oregon},{model,uint32()},
	{data,hex(6|8)}]

    [{class,sensor},{protocol,mandolyn},{model,temperaturehimidity},
        {data,uint32()}]

    [{class,sensor},{protocol,fineoffset},{data,hex(5)}]

    [{protocol,"hasta"},{model,"selflearningv2",{data,uint32()]]


# Output events

    [{protocol,nexa},{unit,$A..$P},{channel,1..16},{dimmer,boolean()}]

    [{protocol,nexax},{unit,0..16#3ffffff},{channel,1..16}]

    [{protocol,waveman},{unit,$A..$P},{channel,1..16}]

    [{protocol,sartano},{channel,1..16#3ff}]

    [{protocol,ikea},{unit,1..16},{channel,1..10},
       {dimmer,boolean()}, {style,smooth|instant}]

    [{protocol,risingsun},{unit,1..4},{channel,1..4}]
