#if VISUAL_STUDIO
#define debugging
#define arg1index
#define mswindows
#define newor
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2009 John Pool -- j.pool@ision.nl
                   Contributions 2009 by Lars Iwer -- lars.iwer@inf.tu-dresden.de

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;

namespace RTParser.Prolog
{
    public enum OType { noop = -1, fx, fy, xf, yf, xfx, xfy, yfx }

    public enum OGroup { noop = -1, fz, zfz, zf }

    public class OperatorDescr
    {
        #region private fields
        int[] prec = new int[3] { -1, -1, -1 };
        OType[] type = new OType[3];
        bool[] user = new bool[3]; // user-defined vs. predefined and unmodifiable
        #endregion

        #region protected fields
        protected string name;
        protected short arity;
        #endregion

        #region public properties
        public string Name { get { return name; } }
        public int Arity { get { return arity; } }
        #endregion


        public OperatorDescr(int p, string t, string n, bool u)
        {
            name = n;
            arity = 0;
            //Console.WriteLine ("OperatorDescr: op {0} GType {1}", n, t);
            Assign(p, t, u);
        }

        public void Assign(int p, string s, bool u)
        {
            OType t = OpTypeStringToType(s);
            int g = (int)TypeToGroup(t);
            //Console.WriteLine ("Assign: op {0} GType {1}", name, TypeToGroup (t));
            // An operator can be either prefix or postfix, but not both.
            // In addition, it can be infix.
            prec[g] = p;
            if (g == (int)OGroup.fz)
                prec[(int)OGroup.zf] = -1;
            else if (g == (int)OGroup.zf)
                prec[(int)OGroup.fz] = -1;
            type[g] = t;
            user[g] = u;
        }

        public bool IsDefinedAsPrefix(out string op, out int pr, out OType ot)
        {
            op = name;
            ot = type[0];
            pr = prec[0];
            return (pr != -1);
        }

        public bool IsDefinedAsInfix(out string op, out int pr, out OType ot)
        {
            op = name;
            ot = type[1];
            pr = prec[1];
            return (pr != -1);
        }

        public bool IsDefinedAsPostfix(out string op, out int pr, out OType ot)
        {
            op = name;
            ot = type[2];
            pr = prec[2];
            return (pr != -1);
        }

        public bool IsPredefined(OType type, string name)
        {
            int g = (int)TypeToGroup(type);
            return (prec[g] != -1 && !user[g]);
        }


        public static OGroup TypeToGroup(OType t)
        {
            switch (t)
            {
                case OType.noop:
                    return OGroup.noop;
                case OType.fx:
                case OType.fy:
                    return OGroup.fz;
                case OType.xfx:
                case OType.xfy:
                case OType.yfx:
                    return OGroup.zfz;
                case OType.xf:
                case OType.yf:
                    return OGroup.zf;
            }
            throw new Exception("*** Illegal call to TypeToGroup");
        }


        private static OType OpTypeStringToType(string typeStr)
        {
            switch (typeStr)
            {
                case "fx": return OType.fx;
                case "fy": return OType.fy;
                case "xfx": return OType.xfx;
                case "xfy": return OType.xfy;
                case "yfx": return OType.yfx;
                case "xf": return OType.xf;
                case "yf": return OType.yf;
            }
            throw new Exception(String.Format("*** Illegal operator type {0}", typeStr));
        }


        public override string ToString()
        {
            StringBuilder sb = new StringBuilder("[operator: " + name + " {");

            for (int i = 0; i < 3; i++)
                if (prec[i] != -1)
                    sb.Append(String.Format("(prec: {0} type: {1})", prec[i], type[i]));

            return sb.ToString() + "}]";
        }
    }
}
