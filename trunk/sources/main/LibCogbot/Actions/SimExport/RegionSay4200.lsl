// Goes inside a Wearable Prim

integer region_channel = -4200;
integer repeat_channel = 4201;

default{
  state_entry(){
    llListen( region_channel, "", "", "" );
    llListen( repeat_channel, "", "", "" );
  }

  listen( integer vIntChannel, string vStrName, key vKeySpeaker, string vStrHeard ){
    //-- your filter wil probably be different
    if (vIntChannel == region_channel){
        llOwnerSay( vKeySpeaker + ": "+ vStrHeard );
    }else{
       if (vKeySpeaker==llGetOwner()) llRegionSay( -4202, vStrHeard );
    }
  }
}
