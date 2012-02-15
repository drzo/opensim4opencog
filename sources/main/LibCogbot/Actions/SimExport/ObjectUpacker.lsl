// C# sharp places this script objects that it knows have Objects in their task inventory

key oldKey;
key newKey;
integer objNum = 0;
integer rezesNeeded = 0;
integer nn;
integer scriptAt = 9999;
integer rezesTotal = 0;

d(string speak) {
llRegionSay(-4200,speak);
}

describeItem(string item, integer i) {
integer type = llGetInventoryType(item);
integer index = llListFindList([ INVENTORY_NONE,
INVENTORY_TEXTURE, INVENTORY_SOUND, INVENTORY_LANDMARK, INVENTORY_CLOTHING,
INVENTORY_OBJECT, INVENTORY_NOTECARD, INVENTORY_SCRIPT, INVENTORY_BODYPART,
INVENTORY_ANIMATION, INVENTORY_GESTURE], [type]);
string name = llList2String(["None",
"Texture", "Sound", "Landmark", "Clothing",
"Object", "Notecard", "Script", "Bodypart",
"Animation", "Gesture"], index);


//if(type == INVENTORY_NONE)          return;

integer owner_perms = llGetInventoryPermMask(item, MASK_OWNER);
string perms;
if(owner_perms & PERM_COPY)
perms += "C";

if(owner_perms & PERM_MODIFY)
perms += "M";

if(owner_perms & PERM_TRANSFER)
perms += "T";

if(owner_perms & PERM_MOVE)
perms += "V";

d("INV-NODE: "+i+"," +llGetInventoryKey(item)+ "," + name + "," + perms + ",," + item);
}

RezNext() {
if (rezesNeeded==0) {
d("INV-OBJ:"+ rezesTotal);
llRemoveInventory(llGetScriptName());   //uncomment this to make script auto remove
return;
}
integer i;
for(i = objNum; i < nn; i++) {
                   string name = llGetInventoryName(INVENTORY_ALL,i);
                   objNum = i;
                   oldKey = llGetInventoryKey(name);
                   if (llGetInventoryType(name)==INVENTORY_OBJECT) {
                      //llOwnerSay("obj"+i + " aka "+ name + " just got served " +oldKey + " by " + llGetKey());
                      llRezObject(name, llGetPos()+<0,0,2>
  , ZERO_VECTOR, ZERO_ROTATION, 0);
  return;
  }
  }
  }

  listContents() {
  if (nn==0) {
  d("INV-ONLY: "+nn);
  return;
  }

  d("INV-START: "+nn);

  integer i;
  for(i = 0; i < nn; i++) {          
               string name = llGetInventoryName(INVENTORY_ALL,i);
               if (name=="ObjectUnpacker") scriptAt = i;
               describeItem(name,i);
            }
        
            d("INV-END: "+nn);         
    }

    default
    {
        state_entry()
        {  
    
            llListen( -4202, "", "", "" ); 
    
            nn = llGetInventoryNumber(INVENTORY_ALL);
    
            rezesNeeded = llGetInventoryNumber(INVENTORY_OBJECT);
        
            RezNext();        
        }
     
      listen( integer vIntChannel, string vStrName, key vKeySpeaker, string vStrHeard ){
               if (vIntChannel == -4202) {
                   vStrHeard = llToLower(vStrHeard);
                   if (llSubStringIndex(vStrHeard,(string)llGetKey())>-1) {
                       if (llSubStringIndex(vStrHeard,"reznext")>-1)  {
                           RezNext(); 
                           return;
                        }
                       if (llSubStringIndex(vStrHeard,"killscript")>-1)  {
                           llRemoveInventory(llGetScriptName());   //uncomment this to make script auto remove
                           return;
                        }                    
                       if (llSubStringIndex(vStrHeard,"listcontents")>-1)  {
                           listContents();   //uncomment this to make script auto remove
                           return;
                        }                    
                    }
                }
       }
   
        object_rez(key newThing) {  
            rezesNeeded--;
            rezesTotal++;      
            newKey = newThing;
            integer objNumP = objNum;
            if (objNum>scriptAt) {
                objNumP--;
            }
            d("RTI: "+newThing + "," + oldKey + "," + llGetKey()+","+objNumP);

            objNum++;
           // RezNext();
        }
    }
