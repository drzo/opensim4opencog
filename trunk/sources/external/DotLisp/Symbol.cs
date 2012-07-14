//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

namespace DotLisp
{
public class Symbol
	{
	virtual public Object getGlobalValue()
		{
		if(globalValue != UNDEFINED)
			return globalValue;
        if (name == "NIL") return null;
		throw new Exception("ERROR: undefined variable "+ name);
		}

	public Boolean isDefined()
		{
		return(globalValue!=UNDEFINED);
		}

	internal Boolean isDynamic = false;

	public virtual Object setGlobalValue(Object newval)
		{
		return( globalValue=newval);
		}

	public override String ToString()
		{
		return name;
		}

	public Symbol Setter{
		get{return setter;}
		set{setter = value;}
	}

	private static Int32 nextgen = 0;
	internal static Symbol gensym()
		{
		++nextgen;
		return new Symbol("#G" + nextgen);
		}

	internal Symbol setter = null;
	internal String name;


	protected static Symbol UNDEFINED = new Symbol("#!undefined");

	//every symbol maintains its global value
	protected Object globalValue = UNDEFINED;

	internal Symbol(String name)
		{
		this.name = name;
		} 
	}

internal class SymbolTable
	{
    private readonly Interpreter interpreter;
    private readonly List<string> Usings = new List<string>();
    public SymbolTable(Interpreter interpreter)
    {
        this.interpreter = interpreter;
        Usings.Add("System");
        Usings.Add("System.Collections");
        Usings.Add("System.Collections.Generic");
        Usings.Add("System.Relection");
        internType(typeof (string));
    }
    internal SymbolTable Parent
    {
       get
       {
           if (interpreter.Parent != null) return interpreter.Parent.symbolTable;
           return null;
       }
    }
	internal Symbol internConstant(String name,Object val)
		{
		Symbol result = null;
	    SymbolTable from = this;
        while (from != null)
	    {
            var table = from.table;
            if (table.ContainsKey(name))
            {
                // KHC : just return what the constant alreay is
                //throw new Exception("Constant: " + name + " already defined");
                return (Symbol)(table[name]);
            }
	        from = from.Parent;
	    }
		this.table[name] = result = new Constant(name,val);
		return result;
		}

	internal Symbol intern(String name)
		{
	    var table = this.table;
        Symbol result = (Symbol)table[name];
        if (result == null)
        {
            var p = Parent;
            while (p != null)
            {
                Symbol result2 = p.table[name] as Symbol;
                if (result2 != null) return result2;
                p = p.Parent;
            }
        }
	    if(result == null)
			{
			if(name.StartsWith(":"))
				{
				table[name] = result = new Keyword(name);
				}
			else
				{
				int firstdot = name.IndexOf('.');
				int lastdot = name.LastIndexOf('.'); 
				int lastcolon = name.LastIndexOf(':'); 
				int nameLength=name.Length;

				// .instance
				// .[namespace.]type:explicitinstance
				// [namespace.]type.
				// [namespace.]type:static
				// obj.member - transformed by reader
				if((firstdot != -1 || lastcolon != -1) && nameLength > 1)
					{
					if(firstdot == 0)	//instance
						{
						if(lastcolon > 0)	  //explicitly qualified
							{

                            String memberName = name.Substring(lastcolon + 1, nameLength - (lastcolon + 1));
                            CLSMember innermember = CLSMember.FindMember(
                                                             memberName, this,
                                                             name.Substring(1, lastcolon - 1), false);
                            table[name] = result = new CLSInstanceSymbol(name, innermember); 
							}
						else
							{
							String memberName = name.Substring(1,nameLength-1);
							table[name] = result = new CLSInstanceSymbol(name,memberName,null);
							}
						}
					else if(lastcolon > 0)	//static
						{
						String memberName = name.Substring(lastcolon+1,nameLength-(lastcolon+1));
                        CLSMember innermember = CLSMember.FindMember(
                                                         memberName,this,
                                                         name.Substring(0, lastcolon), true);
                        table[name] = result = new CLSStaticSymbol(name, innermember);
						}
					else if(lastdot==nameLength-1) //type
						{
						Type type = findType(name.Substring(0,lastdot));
						table[name] = result = new CLSTypeSymbol(name,type);
						}
					}
				else
					{
					table[name] = result = new Symbol(name);
					result.isDynamic = (name[0] == '*');
					}
				}
			}
		return result;
		}

	internal void internType(Type t)
		{
            try
            {
                lock (fullNamesToTypes)
                    //add the name to both the full and shortNames
                    //should be no dupes in fullNames
                    //    Console.WriteLine(" internType :" + t.ToString() + " fn=" + t.FullName + " sn=" + t.Name);
                    if (!fullNamesToTypes.Contains(t.FullName))
                    {
                        fullNamesToTypes[t.FullName] = t;
                        ArrayList arr = GetTypeCache(t.Name);
                        arr.Add(t);

                    }
            }
            catch (Exception e)
            {
                Console.WriteLine("WARNING: SKIPPING TYPE " + t + "  BECAUSE OF " + e + " (Only WARNING)");
            }
		}

    public ArrayList GetTypeCache(string s)
    {
        ArrayList arr = (ArrayList)shortNamesToTypes[s];

        if (arr == null)
        {
            arr = new ArrayList(5);
            shortNamesToTypes[s] = arr;
            foreach (string u in Usings)
            {
                Type t0 = Type.GetType(s + "." + s);
                if (t0 != null && !arr.Contains(t0)) arr.Add(t0);
            }
        }
        return arr;
    }


    internal Type[] findTypes(String name)
    {
        string noDot = name.TrimStart('.');
        if (name.TrimStart('.') == "IDataObject") return new Type[] { typeof(System.Windows.Forms.IDataObject) };
        if (name.TrimStart('.') == "Enum") return new Type[] { typeof(System.Enum) };
        if (name.TrimStart('.') == "Convert") return new Type[] { typeof(System.Convert) };
        if (name.TrimStart('.') == "Type") return new Type[] { typeof(System.Type) };
        if (name.TrimStart('.') == "String") return new Type[] { typeof(System.String) };
        if (name.TrimStart('.') == "Array") return new Type[] { typeof(System.Array) };
        if (name.IndexOf(".") > -1)	//namespace qualified
        {
            Type t = (Type)fullNamesToTypes[name];
            if (t != null)
                return NewTypes(t);
            else
            {
                throw new Exception("Can't find " + name);
            }
        }
        else	//short name
        {

            ArrayList tlist = (ArrayList)shortNamesToTypes[name];
            if (tlist == null)
            {
                if (name.Contains(":"))
                {
                    name = name.Replace(":", ".");
                    Type t = (Type)fullNamesToTypes[name];
                    return NewTypes(t);
                }
                throw new Exception("Can't find Type: " + name);
            }
            else if (tlist.Count > 0)
            {
                if (noDot == "Array") return new Type[] { typeof(System.Array) };
                foreach (string s in Usings)
                {
                    Type t = Type.GetType(s + "." + name);
                    if (t != null) return new[] {t};
                }
                return (Type[])tlist.ToArray(typeof(Type));
            }
        }
        return new Type[0];
    }

    private Type[] NewTypes(Type t)
    {
        return new Type[] { t };
    }

    internal Type findType(String name)
    {
        if (name.IndexOf(".") > -1) //namespace qualified
        {
            Type t = (Type) fullNamesToTypes[name];
            if (t != null)
                return t;
            else
            {
                throw new Exception("Can't find " + name);
            }
        }
        else //short name
        {
            foreach (string s in Usings)
            {
                Type t = Type.GetType(s + "." + name);
                if (t != null) return t;
            }
            ArrayList tlist = (ArrayList) shortNamesToTypes[name];
            if (tlist == null)
            {
                if (name.Contains(":"))
                {
                    name = name.Replace(":", ".");
                    return (Type) fullNamesToTypes[name];
                }
                throw new Exception("Can't find Type: " + name);
            }
            else if (tlist.Count > 1)
            {
                // KHC patch: to disambiguate to common choices for now ...
                foreach (Type type in tlist)
                {
                    string typeString = type.ToString();
                    int lastIndex = typeString.LastIndexOf('.');
                    if (lastIndex > 0)
                    {
                        string assemName = typeString.Substring(0, lastIndex);
                        foreach (string s in Usings)
                        {
                            if (s == assemName)
                            {
                                return type;
                            }
                        }
                    }
                    if (typeString == "System.Type") return (Type) type;
                    if (typeString == "System.Enum") return (Type) type;
                    if (typeString == "System.IO.Path") return (Type) type;
                    if (typeString == "System.Collections.IEnumerable") return (Type) type;
                    if (typeString == "System.Collections.IEnumerator") return (Type) type;
                    if (typeString == "System.Environment") return (Type) type;
                    if (typeString == "System.Windows.Forms.IDataObject") return (Type) type;
                }
                foreach (Type type in tlist)
                {
                    string typeString = type.ToString();
                    if (typeString.StartsWith("System.")) return (Type) type;
                    if (typeString.StartsWith("DotLisp.")) return (Type) type;
                    if (typeString.StartsWith("Microsoft.")) return (Type) type;
                    if (typeString.StartsWith("Mono.")) return (Type) type;
                }

                //todo tell them which names conflict
                StringBuilder sb = new StringBuilder("Ambiguous Type name: " + name + " ->");
                foreach (Type type in tlist)
                {
                    sb.Append("\n " + type.FullName);
                }
                throw new Exception(sb.ToString());
            }
            else
                return (Type) tlist[0];
        }
    }

    Hashtable table = new Hashtable(500);
	// String->Type
	private static Hashtable fullNamesToTypes = new Hashtable(500);
	// String->ArrayList<Type>
	static private Hashtable shortNamesToTypes = new Hashtable(500);

    public static List<Assembly> LoadedAssemblies = new List<Assembly>();

    public void internAssembly(Assembly a)
    {
        lock (LoadedAssemblies)
        {
            if (LoadedAssemblies.Contains(a)) return;
            LoadedAssemblies.Add(a);
        }
        Type[] types = a.GetTypes();
        foreach (Type t in types)
        {
            internType(t);
        }
    }
	}
}
