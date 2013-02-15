/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

using System;
using org.apache.qpid.transport.codec;
using System.Collections.Generic;
using org.apache.qpid.transport.util;
using org.apache.qpid.transport.network;
using System.IO;

namespace org.apache.qpid.transport
{



public sealed class ConnectionStartOk : Method {

    public const int TYPE = 258;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 0;
    }

    public override int GetPackWidth() {
        return 2;
    }

    public override bool HasPayload() {
        return false;
    }

    public override byte EncodedTrack 
    {
       get{ return Frame.L1; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private Dictionary<String,Object> _ClientProperties;
    private String _Mechanism;
    private byte[] _Response;
    private String _Locale;


    public ConnectionStartOk() {}


    public ConnectionStartOk(Dictionary<String,Object> ClientProperties, String Mechanism, byte[] Response, String Locale, params Option[] options) {
        SetClientProperties(ClientProperties);
        SetMechanism(Mechanism);
        SetResponse(Response);
        SetLocale(Locale);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.SYNC: Sync = true; break;
            case Option.BATCH: Batch = true; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public override void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.ConnectionStartOk(context, this);
    }


    public bool HasClientProperties() {
        return (packing_flags & 256) != 0;
    }

    public ConnectionStartOk ClearClientProperties() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetClientProperties() {
        return _ClientProperties;
    }

    public ConnectionStartOk SetClientProperties(Dictionary<String,Object> value) {
        _ClientProperties = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasMechanism() {
        return (packing_flags & 512) != 0;
    }

    public ConnectionStartOk ClearMechanism() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public String GetMechanism() {
        return _Mechanism;
    }

    public ConnectionStartOk SetMechanism(String value) {
        _Mechanism = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasResponse() {
        return (packing_flags & 1024) != 0;
    }

    public ConnectionStartOk ClearResponse() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public byte[] GetResponse() {
        return _Response;
    }

    public ConnectionStartOk SetResponse(byte[] value) {
        _Response = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasLocale() {
        return (packing_flags & 2048) != 0;
    }

    public ConnectionStartOk ClearLocale() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public String GetLocale() {
        return _Locale;
    }

    public ConnectionStartOk SetLocale(String value) {
        _Locale = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteMap(_ClientProperties);
        if ((packing_flags & 512) != 0)
            enc.WriteStr8(_Mechanism);
        if ((packing_flags & 1024) != 0)
            enc.WriteVbin32(_Response);
        if ((packing_flags & 2048) != 0)
            enc.WriteStr8(_Locale);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _ClientProperties = dec.ReadMap();
        if ((packing_flags & 512) != 0)
            _Mechanism = dec.ReadStr8();
        if ((packing_flags & 1024) != 0)
            _Response = dec.ReadVbin32();
        if ((packing_flags & 2048) != 0)
            _Locale = dec.ReadStr8();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_ClientProperties", GetClientProperties());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Mechanism", GetMechanism());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_Response", GetResponse());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_Locale", GetLocale());

			return result;
        }
    }

}
}
