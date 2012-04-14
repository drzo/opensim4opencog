using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    using Aima.Core.Agent;

    public class Table<TRowHeader, TColumnHeader, TValue>
    {
        private readonly IList<TRowHeader> rowHeaders;
        private readonly IList<TColumnHeader> columnHeaders;
        private Dictionary<TRowHeader, Dictionary<TColumnHeader, TValue>> rows;

        public Table(IList<TRowHeader> rowHeaders,
                IList<TColumnHeader> columnHeaders) {

            this.rowHeaders = rowHeaders;
            this.columnHeaders = columnHeaders;
            this.rows = new Dictionary<TRowHeader, Dictionary<TColumnHeader, TValue>>();
            foreach (TRowHeader rowHeader in rowHeaders) 
            {
                rows[rowHeader] = new Dictionary<TColumnHeader, TValue>();
            }
        }

        public void Set(TRowHeader r, TColumnHeader c, TValue v) {
            rows[r][c] = v;
        }

        public TValue Get(TRowHeader r, TColumnHeader c) 
        {
            Dictionary<TColumnHeader, TValue> rowValues = rows[r];
            
            return rowValues[c];

        }

        //TODO: rewrite as Linq extension method call
        public override string ToString() {
            
            //TODO: figure out if StringBuilder is ok replacement for Java's StringBuffer
            StringBuilder buf = new StringBuilder();
            foreach (TRowHeader r in rowHeaders) {
                foreach (TColumnHeader c in columnHeaders) {
                    buf.Append(this.Get(r,c).ToString());
                    buf.Append(" ");
                }
                buf.Append("\n");
            }
            return buf.ToString();
        }

        //TODO: figure out if commented part of the code below needs to be retained

        //class Row<R> {
        //    private Dictionary<TColumnHeader, TValue> cells;

        //    public Row() {

        //        this.cells = new Dictionary<TColumnHeader, TValue>();
        //    }

        //    public Dictionary<TColumnHeader, TValue> cells() {
        //        return this.cells;
        //    }

        //}

        //class Cell<TValueHeader> {
        //    private TValueHeader value;

        //    public Cell() {
        //        value = null;
        //    }

        //    public Cell(TValueHeader value) {
        //        this.value = value;
        //    }

        //    public void set(TValueHeader value) {
        //        this.value = value;
        //    }

        //    public TValueHeader value() {
        //        return value;
        //    }

        //}
    }
}
