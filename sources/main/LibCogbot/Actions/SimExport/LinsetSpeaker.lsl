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



// C# sharp places this script objects that it knows have Objects in their task inventory

integer objNum = 0;

d(string speak) { llRegionSay(-4200,speak); }
string posString(vector pos) {
    return (string)pos.x+","+(string)pos.y+","+(string)pos.z;
}
RezNext() {  
   if (objNum==llGetInventoryNumber(INVENTORY_OBJECT)) {
      llRemoveInventory(llGetScriptName());
      return;
   }
  float z = objNum+1;
  string name = llGetInventoryName(INVENTORY_OBJECT,objNum);
  integer mask = llGetInventoryPermMask(name,MASK_OWNER);
  if ((PERM_COPY & mask)==0) {
      vector pos = llGetPos()+<0,0,z>; 
      d("RTI: "+(string)NULL_KEY + "," + (string)llGetKey()+","+(string)objNum+","+
         posString(pos)+","+(string)llGetInventoryKey(name)+",nocopy," + name);
      objNum++;
      RezNext();
      return;
  }
  llRezObject(name,llGetPos()+<0,0,z>,ZERO_VECTOR,ZERO_ROTATION, 0);
}

default
{
    state_entry()
    {   
        d("RTI: objcount="+(string)llGetInventoryNumber(INVENTORY_OBJECT));
        objNum = 0;
        RezNext();        
    }
    
    object_rez(key newThing) {
         string name = llGetInventoryName(INVENTORY_OBJECT,objNum);
         vector pos = llGetPos()+<0,0,objNum+1>;         
         d("RTI: "+(string)newThing + "," + (string)llGetKey()+","+(string)objNum+","
              +posString(pos)+","+(string)llGetInventoryKey(name)+",rezzed," + name);
            
        objNum++;
        RezNext();
    }
}
    