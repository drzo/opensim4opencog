
/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007 John Pool -- j.pool@ision.nl

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/
#if USE_FirebirdSql
using FirebirdSql.Data.FirebirdClient;

//#define USE_FirebirdSql
#if persistent
using System;
using System.Text;
using System.Data;
using System.Collections;
using System.Collections.Specialized;
using System.Xml;
using System.Data.SqlClient;
//using FirebirdSql.Data.Firebird;
using IO = RTParser.Prolog.PrologIO;

namespace RTParser.Prolog
{
    public class DbLogin
    {
        private string dbc; // database connect
        private string uid;
        private string pwd;
        public string Dbc { get { return dbc; } }
        public string Uid { get { return uid; } }
        public string Pwd { get { return pwd; } }

        public DbLogin(string d, string u, string p)
        {
            dbc = d;
            uid = u;
            pwd = p;
        }
    }


    public class DbAccess
    {
        private FbConnection dbConnection;
        private FbCommand dbCommand;
        private FbDataAdapter dbDataAdapter;
        private FbDatabaseInfo dbInfo = new FbDatabaseInfo();
        private string dbSource;
        private string database;
        public string Database { get { return database; } set { database = value; } }
        private string userID;
        public string UserID { get { return userID; } set { userID = value; } }
        private string password;
        public string Password { get { return password; } set { password = value; } }
        //private bool isDbReadOnly;
        //public  bool IsDbReadOnly { get { return isDbReadOnly; } }

        public DbAccess(DbLogin dbLogin) // temp
        {
            if (dbLogin == null)
            {
                database = "C:\\PROJECTS\\DATA\\PROLOGDATA.FDB";
                userID = "SYSDBA";
                password = "masterkey";
            }
            else
            {
                database = dbLogin.Dbc;
                userID = dbLogin.Uid;
                password = dbLogin.Pwd;
            }
            OpenDbConnection(database, userID, password);
        }


        ~DbAccess()
        {
            try { CloseDbConnection(); }
            catch { }
        }


        private void OpenDbConnection(string db, string uid, string pwd)
        {
            try
            {
                FbConnectionStringBuilder cs = new FbConnectionStringBuilder();

                cs.DataSource = "localhost";
                cs.Database = database;
                cs.UserID = userID;
                cs.Password = password;
                cs.Dialect = 3;
                dbSource = cs.ToString();

                dbConnection = new FbConnection(dbSource);
                dbConnection.Open();
                dbCommand = new FbCommand(null, dbConnection);
                dbDataAdapter = new FbDataAdapter(dbCommand);
            }
            catch
            {
                dbConnection = null;
                IO.Error("Unable to open database connection:\n{0}", dbSource);
            }
        }


        private void CloseDbConnection()
        {
            dbConnection.Close();
        }


        public TableColumnCollection GetTableColumnCollection(string pred, int arity, string tableName)
        {
            DataTable dt = dbConnection.GetSchema("Tables", new string[] { null, null, tableName });

            if (dt == null || dt.Rows.Count == 0)
                IO.Error("Table or view '{0}' for persistent predicate '{1}/{2}' not found in database '{3}'",
                          tableName, pred, arity, Database);

            dt = dbConnection.GetSchema("Columns", new string[] { null, null, tableName });
            TableColumnCollection tcc = new TableColumnCollection();

            foreach (DataRow row in dt.Rows)
            {
                object o;
                string name = row["COLUMN_NAME"].ToString();
                FbDbType dbType = (FbDbType)Enum.Parse(typeof(FbDbType), row["COLUMN_DATA_TYPE"].ToString(), true);
                int size = ((o = row["COLUMN_SIZE"]) == DBNull.Value) ? -1 : (int)o;
                int precision = ((o = row["NUMERIC_PRECISION"]) == DBNull.Value) ? -1 : (int)o;
                int scale = ((o = row["NUMERIC_SCALE"]) == DBNull.Value) ? -1 : (int)o;
                bool isNullable = ((o = row["IS_NULLABLE"]) == DBNull.Value) ? false : (Convert.ToInt32(o) == 1);
                int pos = ((o = row["ORDINAL_POSITION"]) == DBNull.Value) ? -1 : Convert.ToInt32(o);

                //Console.WriteLine ("name:{0} dataType:{1} size:{2} precision:{3} scale:{4} isNullable:{5} pos:{6}",
                //                   name, dbType, size, precision, scale, isNullable, pos);

                tcc.Add(pos, new TableColumn(name, dbType, size, precision, scale, isNullable));

            }
            return tcc;
        }


        public ProcParamCollection GetProcParamCollection(string pred, int arity, string procName,
                                                           out int inputParamCount, out int outputParamCount)
        {
            DataTable dt = dbConnection.GetSchema("Procedures", new string[] { null, null, procName });

            if (dt == null || dt.Rows.Count == 0)
                IO.Error("Stored procedure '{0}' for persistent predicate '{1}/{2}' not found in database '{3}'",
                          procName, pred, arity, Database);

            inputParamCount = Convert.ToInt32(dt.Rows[0]["INPUTS"]);  // get number of in/output parameters
            outputParamCount = Convert.ToInt32(dt.Rows[0]["OUTPUTS"]); // ...

            dt = dbConnection.GetSchema("ProcedureParameters", new string[] { null, null, procName });
            ProcParamCollection ppc = new ProcParamCollection();

            foreach (DataRow row in dt.Rows)
            {
                object o;
                string name = row["PARAMETER_NAME"].ToString();
                FbDbType dbType = (FbDbType)Enum.Parse(typeof(FbDbType), row["PARAMETER_DATA_TYPE"].ToString(), true);
                int size = ((o = row["PARAMETER_SIZE"]) == DBNull.Value) ? -1 : Convert.ToInt32(o);
                int precision = ((o = row["NUMERIC_PRECISION"]) == DBNull.Value) ? -1 : Convert.ToInt32(o);
                int scale = ((o = row["NUMERIC_SCALE"]) == DBNull.Value) ? -1 : Convert.ToInt32(o);
                int pos = ((o = row["ORDINAL_POSITION"]) == DBNull.Value) ? -1 : Convert.ToInt32(o);
                ParameterDirection direction =
                  (Convert.ToInt32(row["PARAMETER_DIRECTION"]) == 1) ? ParameterDirection.Input : ParameterDirection.Output;
                if (direction == ParameterDirection.Output) pos += inputParamCount; // input parameters first

                //Console.WriteLine ("name:{0} dbType:{1} imType:{2} size:{3} precision:{4} scale:{5} direction:{6} pos:{7}",
                //                   name, dbType, size, precision, scale, direction, pos);

                ppc.Add(pos, new ProcParam(name, dbType, size, precision, scale, direction));
            }
            return ppc;
        }


        public void FillDataSet(string query, DataSet ds, string key)
        {
            dbCommand.CommandText = query;
            dbDataAdapter.Fill(ds, key);
        }


        public void ExecuteSqlCommand(string command)
        {
            dbCommand.CommandText = command;
            try
            {
                dbCommand.Transaction = dbConnection.BeginTransaction();
                dbCommand.Prepare();
                dbCommand.ExecuteNonQuery();
                dbCommand.Transaction.Commit();
            }
            catch
            {
                dbCommand.Transaction.Rollback();
                IO.Error("Failed to execute \"{0}\"", command);
            }
        }


        public void InitExecProcedure(string procName, int inputParamCount)
        {
            string qMarks = null;

            if (inputParamCount > 0)
            {
                for (int i = 0; i < inputParamCount; i++) qMarks += (i == 0) ? "(?" : ", ?";

                qMarks += ")";
            }
            dbCommand.CommandText = string.Format("EXECUTE PROCEDURE {0}{1}", procName, qMarks);
            dbCommand.Parameters.Clear();
        }


        public void AddExecInputParameter(int i, ProcParam p, string v)
        {
            dbCommand.Parameters.Add('@' + p.Name, p.DbType, p.Size, p.Name).Direction = ParameterDirection.Input;
            dbCommand.Parameters[i].Value = v;
        }


        public void AddExecOutputParameter(int i, ProcParam p)
        {
            dbCommand.Parameters.Add('@' + p.Name, p.DbType, p.Size, p.Name).Direction = ParameterDirection.Output;
        }


        public Term DoExecProcedure(string procName, Term query)
        {
            //Console.WriteLine ("DoExecProcedure -- dbCommand.CommandText = {0}", dbCommand.CommandText);
            try
            {
                dbCommand.Transaction = dbConnection.BeginTransaction();
                dbCommand.ExecuteNonQuery();
                dbCommand.Transaction.Commit();
            }
            catch
            {
                dbCommand.Transaction.Rollback();
                IO.Error("Failed to execute stored procedure call \"{0}\"", dbCommand.CommandText);
                throw;
            }

            // convert the parameter values (both input and output) to a Term

            int nCols = query.Arity;
            Term[] args = new Term[nCols];

            for (int i = 0; i < nCols; i++)
            {
                Term a = query.Args[i];

                if (a.IsVar)
                {
                    FbDbType dbType = dbCommand.Parameters[i].FbDbType;
                    args[i] = DbAccess.DbScalarToTerm(dbCommand.Parameters[i].Value, dbType, a.FType, procName);
                }
                else
                    args[i] = a;
            }
            return new Term(query.Functor, args);
        }


        public static Term DbScalarToTerm(object scalar, FbDbType dbType, FType fType, string dbEntity)
        {
            switch (dbType)
            {
                case FbDbType.SmallInt:
                case FbDbType.Decimal:
                case FbDbType.Integer:
                case FbDbType.Float:
                case FbDbType.Numeric:
                case FbDbType.Double:
                case FbDbType.BigInt:
                    return new Term(scalar.ToString(), FType.number);
                case FbDbType.Char:
                case FbDbType.VarChar:
                    return new Term(Utils.AtomFromVarChar(scalar.ToString()), FType.atom);
                case FbDbType.Date:
                case FbDbType.TimeStamp:
                    return new Term(Convert.ToDateTime(scalar));
                case FbDbType.Time:
                    return new Term((Convert.ToDateTime(scalar)).TimeOfDay);
                default:
                    IO.Error("Firebird data type {0} in table/view/stored procedure {1} not supported in Prolog", dbType, dbEntity);
                    return null;
            }
        }


        public string DataTableString(DataTable dt)
        {
            if (dt == null) return ("<empty resultset>");

            StringBuilder sb = new StringBuilder();

            //      foreach(DataColumn c in dt.Columns)
            //      {
            //        Console.WriteLine(c.ColumnName);
            //        Console.WriteLine(c.DataType);
            //      }

            foreach (DataRow row in dt.Rows)
            {
                // column[0] is always NULL and intended as a cache for the 'Termified' row
                for (int i = 1; i < dt.Columns.Count; i++) sb.Append(row[i] + " ");

                sb.Append(Environment.NewLine);
            }

            return sb.ToString();
        }
    }


    public abstract class DbDataItem // describes a table/view or a procedure parameter
    {
        protected string name;
        protected FbDbType dbType; // original database type
        protected int size;
        protected int precision;
        protected int scale;

        public string Name { get { return name; } }
        public FbDbType DbType { get { return dbType; } }
        public int Size { get { return size; } }
        public int Precision { get { return precision; } }
        public int Scale { get { return scale; } }

        public DbDataItem(string nm, FbDbType dt, int sz, int pr, int sc)
        {
            name = nm;
            dbType = dt;
            size = sz;
            precision = pr;
            scale = sc;
        }

        protected Term DatatypeTerm() // make a datatype term with a possible argument list (size, precision, scale)
        {
            Term[] a = new Term[(size == -1 ? 0 : 1) + (precision == -1 ? 0 : 1) + (scale == -1 ? 0 : 1)];

            int i = 0;
            if (size != -1) a[i++] = new Term(size.ToString(), FType.number);
            if (precision != -1) a[i++] = new Term(precision.ToString(), FType.number);
            if (scale != -1) a[i] = new Term(scale.ToString(), FType.number);

            return new ListTerm(new Term(dbType.ToString(), a));
        }

        public abstract Term ToTerm();
    }


    public class TableColumn : DbDataItem
    {
        private bool isNullable;
        public bool IsNullable { get { return isNullable; } }

        public TableColumn(string nm, FbDbType dt, int sz, int pr, int sc, bool nu)
            : base(nm, dt, sz, pr, sc)
        {
            isNullable = nu;
        }


        public override Term ToTerm()
        {
            // list is constructed in reverse order -- nullable first, name last
            Term result = new ListTerm(new Term("nullable", new Term(isNullable ? "yes" : "no"), OType.noop, 0));
            result = new ListTerm(DatatypeTerm(), result);

            return new ListTerm(new Term(Utils.MakeAtom(name)), result); // columm name
        }
    }


    public class ProcParam : DbDataItem
    {
        private ParameterDirection direction;
        public ParameterDirection Direction { get { return direction; } }

        public ProcParam(string nm, FbDbType dt, int sz, int pr, int sc, ParameterDirection pd)
            : base(nm, dt, sz, pr, sc)
        {
            direction = pd;
        }


        public override Term ToTerm()
        {
            Term result = new ListTerm(new Term("direction", new Term(direction.ToString()), OType.noop, 0));
            result = new ListTerm(DatatypeTerm(), result);

            return new ListTerm(new Term(Utils.MakeAtom(name)), result);
        }
    }


    public class DbMetaDataCollection : ListDictionary
    {
        public new virtual void Add(object Key, object Value) { base.Add(Key, Value); }

        public DbDataItem this[int i] { get { return (DbDataItem)base[i]; } }
    }


    public class TableColumnCollection : DbMetaDataCollection
    {
        public override void Add(object Key, object Value) { base.Add(Key, Value); }

        public new TableColumn this[int i] { get { return (TableColumn)base[i]; } }
    }


    public class ProcParamCollection : DbMetaDataCollection
    {
        public override void Add(object Key, object Value) { base.Add(Key, Value); }

        public new ProcParam this[int i] { get { return (ProcParam)base[i]; } }
    }

}
#endif
#endif