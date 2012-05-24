using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.Actions;
using MushDLR223.ScriptEngines;
using OpenMetaverse;

namespace cogbot
{
    public class CmdRequest : Parser
    {
        public static implicit operator string[] (CmdRequest request)
        {
            return request.tokens;
        }
        public UUID CallerAgent;
        public OutputDelegate Output;
        private IDictionary<string, object> ParamMap
        {
           get
           {
               return prepPhrases;
           }
        }
        public Command Cmd;
        private int StartArg;
        private NamedParam[] VersionSelected;

        private NamedParam[][] ParameterVersions
        {
           get
           {
               return Cmd.ParameterVersions;
           }
        }
        private NamedParam[] Parameters
        {
            get
            {
                return Cmd.Parameters;
            }
        }
        public T GetValue<T>(string Param)
        {
            Param = ToKey(Param);
            return (T) ParamMap[Param];
        }
        public void SetValue<T>(string Param, T value)
        {
            Param = ToKey(Param);
            ParamMap[Param] = value;
        }

        public CmdRequest(CmdRequest other, String[] args)
            : base(args)
        {
            CallerAgent = other.CallerAgent ?? UUID.Zero;
            Output = other.Output;
            Cmd = other.Cmd;
            //ParamMap = new Dictionary<string, object>();
            SelectVersion();
            ParseTokens();
        }


        public CmdRequest(string[] text, UUID callerIDORZero, OutputDelegate writeLine, Command command)
            : base(text)
        {
            CallerAgent = callerIDORZero ?? UUID.Zero;
            Output = writeLine;
            this.Cmd = command;
            //ParamMap = new Dictionary<string, object>();
            SelectVersion();
            ParseTokens();
        }

        private void SelectVersion()
        {
            if (VersionSelected != null) return;           
            VersionSelected = Parameters;
            if (ParameterVersions==null || ParameterVersions.Length==0) return;
            int tokenLen = tokens.Length;
            if (ParameterVersions.Length > 1)
            {
                foreach (var vchck in ParameterVersions)
                {
                    if (VersionSelected.Length == tokenLen)
                    {
                        VersionSelected = vchck;
                    }
                }
            }
            int skip = this.StartArg;
            int argCurrent = 0;
            foreach (NamedParam param in VersionSelected)
            {
                if (skip > 0)
                {
                    skip--;
                    continue;
                }
                string name = ToKey(param.Key);
                if (!ParamMap.ContainsKey(name))
                    ParamMap[name] = null;
            }
        }

        private void ParseTokens()
        {
            int tokenLen = tokens.Length;
            int skip = this.StartArg;            
            int argCurrent = 0;
            if (VersionSelected == null)
            {
                return;
            } 
            foreach (NamedParam param in VersionSelected)
            {
                if (skip > 0)
                {
                    skip--;
                    continue;
                }
                if (argCurrent >= tokenLen) return;
                string name = ToKey(param.Key);

                if (param.IsOptional)
                {
                    bool wasBool = typeof (bool) == param.Type;
                    if (!KeyMatches(tokens[argCurrent], param))
                    {
                        if (wasBool)
                        {
                            ParamMap[name] = false;
                            continue;
                        }
                        ParamMap[name] = null;
                        continue;
                    }
                    if (wasBool)
                    {
                        ParamMap[name] = true;
                        argCurrent++;
                        continue;
                    }
                }
                int argsStart = argCurrent;
                int argsUsed;
                object value = ParseArg(param, param.Type, tokens, argsStart, out argsUsed);
                argCurrent += argsUsed;
                ParamMap[name] = value;
            }
        }

        static private bool KeyMatches(string token, NamedParam param)
        {
            var k1 = ToKey(token);
            var k2 = ToKey(param.Key);
            return k1 == k2;
        }

        private object ParseArg(NamedParam param, Type parseFor, string[] text, int start, out int argsUsed)
        {
            argsUsed = 1;
            return text[start];
        }

        public bool TryGetValue<T>(string name, out T value)
        {
            object ovalue;
            if (!ParamMap.TryGetValue(name, out ovalue))
            {
                value = default(T);
                return false;
            }
            value = (T) ovalue;
            return true;
        }

        public CmdRequest AdvanceArgs(int used)
        {
            StartArg += used;
            tokens = SplitOff(tokens, used);
            ParseTokens();
            return this;
        }
    }
}