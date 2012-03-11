// C# sharp places this script on root objects of linksets that have more than one child

string buffer;
integer MAX_BUFF_SIZE  = 900;

// Abstraction of actual data transmission
d(string speak) {
    llRegionSay(-4200,speak);
}

default
{
    state_entry()
    {  
        integer n = llGetObjectPrimCount(llGetKey());
       
        integer i;
       
       
        if(n == 1)
        {
            d("Y,1," + (string)llGetKey()+",Z");
            llRemoveInventory(llGetScriptName());   //uncomment this to make script auto remove
            return;
        }
       
        buffer = "Y,"+(string)n;
       
        for(i = 1; i <= n; i++)
        {
            string new = (string)llGetLinkKey(i);
            if (llStringLength(buffer) + llStringLength(new) > MAX_BUFF_SIZE)
            {
                d(buffer);
                buffer = new;
            } else {
                buffer += "," + new;  
            }
        }
        string  new2 = ",Z";
        if (llStringLength(buffer) + llStringLength(new2) > MAX_BUFF_SIZE)
        {
            d(buffer);
            buffer = new2;
        } else {
            buffer += new2;  
        }
        if (buffer != "")d(buffer);
       
        llRemoveInventory(llGetScriptName());   //uncomment this to make script auto remove
    }
}



default
{
    on_rez(integer skey) {
        llListen(-4202,"",NULL_KEY,"");        
    }
    state_entry() {   
        llListen(-4202,"",NULL_KEY,"");        
    }    
        
    listen(integer channel, string name, key id, string message)
    {
      integer index = llSubStringIndex( message, "bto," );
      if (index==0) 
       {
           list lst = llCSV2List(message);
           if (llGetKey()!=(key)llList2String(lst,1)) return;
            message =  llList2String(lst,2);
       }
        if (llToLower(message) == "saymissing")
        {
            llRegionSay(-4200,"TaskMissing:" + llGetKey() + " " + llGetScriptName());
        }
    }
    
}
