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



integer CMD_CHANNEL = -4202; // positive channel, bot says on this

//  protocol
//
//      /78687  to,x,y,z
//
//      warpPos to x,y,z and report the number of prims there on zero
//
//     /78687  up,x,y,z,delta
//
//     warpPos to x,y,z  and then move repeatedly up by delta until you find an object
//     or you reach 4096.
//     report n   where n is number found
//
//      

warpPos( vector destpos) 
{   //R&D by Keknehv Psaltery, 05/25/2006
    //with a little pokeing by Strife, and a bit more
    //some more munging by Talarus Luan
    //Final cleanup by Keknehv Psaltery
    // Compute the number of jumps necessary
    integer jumps = (integer)(llVecDist(destpos, llGetPos()) / 10.0) + 2;
    // Try and avoid stack/heap collisions
    if (jumps > 100 )
        jumps = 100;    //  1km should be plenty
    list rules = [ PRIM_POSITION, destpos ];  //The start for the rules list
    integer count = 1;
    while ( ( count = count << 1 ) < jumps)
        rules = (rules=[]) + rules + rules;   //should tighten memory use.
    llSetPrimitiveParams( (rules=[]) + rules + llList2List( rules, (count - jumps) << 1, count) ); //Changed by Eddy Ofarrel to tighten memory use some more
    if ( llVecDist( llGetPos(), destpos ) > .001 ) //Failsafe
        while ( --jumps ) 
            llSetPos( destpos );
}

float delta = 1.0;

default
{
    state_entry()
    {
        llSitTarget(ZERO_VECTOR, ZERO_ROTATION);  // can't be zero vector 
                                                        // cause that removes the target
        llListen(CMD_CHANNEL , "", NULL_KEY, "");  // change llGetOwner to NULL_KEY if you need
                                                        // to talk thru repeater
        llSetRot(llEuler2Rot(<0.0, -PI_BY_TWO, 0.0>));   // make sure scanner is scanning up
    }

    listen(integer channel , string name , key id , string message)
    {
        list args = llCSV2List(message );
        string cmd = llList2String(args , 0);
        if (llGetListLength(args) < 4)
        {
            key k = (key)message;        
            list info = llGetObjectDetails(k , [OBJECT_POS , OBJECT_NAME]);
            llSay(0, llList2String(info ,1));
            vector vv = llList2Vector(info , 0);
            llOwnerSay((string)vv);
            if (vv==ZERO_VECTOR) return;
            cmd = "to";
            args = llCSV2List(cmd+","+(string)vv.x+","+(string)vv.y+","+(string)vv.z);
            
        }
            
        vector v =   <
                          (float)llList2String(args , 1),
                          (float)llList2String(args , 2),
                          (float)llList2String(args , 3) >;
                          
        if (cmd == "to")
        {
            warpPos(v);
            llSensor("" , NULL_KEY , ACTIVE|PASSIVE , 96.0, PI_BY_TWO);
        } else {
            warpPos(v);
            delta  =  (float)llList2String(args , 4);
            state scan;
        }
         
    }
             
    no_sensor()
    {
        llOwnerSay("0");
    }
        
    sensor(integer n)
    {
        llOwnerSay((string)n);
    }
}


state scan
{
    state_entry()
    {
         llSensor("" , NULL_KEY , ACTIVE|PASSIVE , 96.0, PI_BY_TWO);
    }
         
    no_sensor()
    {
       vector v = llGetPos();
       if (v.z  > 4094.0)
       {
           llOwnerSay("0");
            state default;
        } else if (v.z + delta > 4094.0) {
            v.z = 4095.0;
        }  else {
            v.z += delta;
        }
        
        warpPos(v);
        llSensor("" , NULL_KEY , ACTIVE|PASSIVE , 96.0, PI_BY_TWO);
    }
        
    sensor(integer n)
    {
        llOwnerSay((string)n);
        state default;
    }
}

