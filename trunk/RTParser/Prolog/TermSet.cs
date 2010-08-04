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
using System.Text;
using System.Collections;

namespace RTParser.Prolog
{
    public enum DupMode { dupIgnore, dupAccept, dupError };

    public class TermSet : ArrayList
    {
        private DupMode dupMode = DupMode.dupAccept;

        public TermSet(DupMode dm)
        {
            dupMode = dm;
        }


        public TermSet(Term list)
        {
            while (list.Arity == 2)
            {
                Add(list.Arg(0));
                list = list.Arg(1);
            }
        }


        public void Insert(Term termToInsert)
        {
            int i = BinarySearch(termToInsert);

            if (i >= 0) // found
            {
                if (dupMode == DupMode.dupAccept) Insert(i, termToInsert);
            }
            else
                Insert(~i, termToInsert);
        }


        public Term ToList()
        {
            Term t;

            t = Term.NULLLIST; // []
            for (int i = Count - 1; i >= 0; i--)
                t = new Term(Parser.DOT, (Term)this[i], t, FType.comp, OType.xfy, 100); // [a1, a2, ...]

            return t;
        }
    }
}
