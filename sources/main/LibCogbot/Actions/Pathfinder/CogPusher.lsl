integer PIPE_CHANNEL = 1000000;  // pipe rezzer
integer REPEATER_CHANNEL  = 100;  // bot talks on this
integer PIPE_KILL_CHANNEL = 1000001; // what pipes listen to to die
float time=0;
integer speakAt = 0;
vector last_pos = ZERO_VECTOR;

vector clr = <1,0,0>;

integer tgtid = -1;
vector tgt = ZERO_VECTOR;

default
{
    state_entry()
    {
        llListen(REPEATER_CHANNEL , "" , llGetOwner() , "");
        llSetColor(clr , ALL_SIDES);
    }
    
    on_rez(integer parm)
    {
        llResetScript(); // otherwise wrong owner
    }
    
    listen(integer channel , string name , key id , string data)
    {
    //    llOwnerSay(llGetSubString(data,0,3));
        
        if(llGetSubString(data,0,4) == "color")
        {
            list c = llCSV2List(llGetSubString(data,5,-1));
            clr.x = (float)llList2String(c,0);
            clr.y = (float)llList2String(c,1);
            clr.z = (float)llList2String(c,2); 
            clr /= 256.0;
            llSetColor(clr , ALL_SIDES);           
        } else if (llGetSubString(data,0, 1) == "hi")
        {
            llRegionSay(8887 , llGetSubString(data , 2,-1));
        }
        else if (llGetSubString(data,0,3) == "http")
        {
            llRegionSay(8888 , data);
          //  llOwnerSay("data is " + data);
            
        } else if(data == "pipe") 
        {
            if (last_pos == ZERO_VECTOR)
            {
                last_pos = llGetPos();
                return;    
            }
            vector v = llGetPos();
            if (llVecDist(last_pos,v) < 0.5)return;
            
            llRegionSay(PIPE_CHANNEL , 
            (string)(256.0 * clr.x) + "," + (string)(256.0 * clr.y) + "," + (string)(256.0 * clr.z) + "," +
            (string)last_pos.x + "," + (string)last_pos.y + "," + (string)last_pos.z + "," +
                 (string)v.x + "," + (string)v.y + "," + (string)v.z );
            last_pos = v;
        }
        else if (llGetSubString(data , 0, 2) == "die")
        {
            llRegionSay(PIPE_KILL_CHANNEL , data);
        }
        else if (llGetSubString(data, 0, 3) == "push")
        {
            list c = llCSV2List(llGetSubString(data,4,-1));
            tgt.x = (float)llList2String(c,0);
            tgt.y = (float)llList2String(c,1);
            tgt.z = (float)llList2String(c,2);
            time = (float)llList2String(c,3); 
            speakAt = (integer)llList2String(c,4);
            llSetStatus(STATUS_PHYSICS , TRUE);
            if(tgtid!=0) {
                   llStopMoveToTarget();
                   llTargetRemove(tgtid);
                   tgtid=0;
             }
             llMoveToTarget(tgt , 2.0);
            tgtid = llTarget(tgt , 1.0);
             if (time!=0)llSetTimerEvent(time);
        }
        else
        {
            llRegionSay(PIPE_CHANNEL , data);
        }
                
    }
    
    at_target(integer tnum , vector tpos , vector ourpos)
    {
        if(speakAt==1)llOwnerSay("at_target(" + (string)tpos.x + "," + (string)tpos.y + "," + (string)tpos.z + 
        "," + (string)ourpos.x + "," + (string)ourpos.y + "," + (string)ourpos.z + ")");
            if(tgtid!=0) {
                   llStopMoveToTarget();
                   llTargetRemove(tgtid);
                   tgtid=0;
             }
        llSetTimerEvent(0.0);
    }
    
    timer()
    {
        vector ourpos = llGetPos();
       // if (time!=0) return;
        if (speakAt==1)llOwnerSay("abort_target(" + (string)tgt.x + "," + (string)tgt.y + "," + (string)tgt.z + 
        "," + (string)ourpos.x + "," + (string)ourpos.y + "," + (string)ourpos.z + ")");
            if(tgtid!=0) {
                   llStopMoveToTarget();
                   llTargetRemove(tgtid);
                   tgtid=0;
             }    
    }
}