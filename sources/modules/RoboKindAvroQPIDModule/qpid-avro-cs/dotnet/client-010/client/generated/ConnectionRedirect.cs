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



public sealed class ConnectionRedirect : Method {

    public const int TYPE = 265;

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
    private String _Host;
    private List<Object> _KnownHosts;


    public ConnectionRedirect() {}


    public ConnectionRedirect(String Host, List<Object> KnownHosts, params Option[] options) {
        SetHost(Host);
        SetKnownHosts(KnownHosts);

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
        mdelegate.ConnectionRedirect(context, this);
    }


    public bool HasHost() {
        return (packing_flags & 256) != 0;
    }

    public ConnectionRedirect ClearHost() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetHost() {
        return _Host;
    }

    public ConnectionRedirect SetHost(String value) {
        _Host = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasKnownHosts() {
        return (packing_flags & 512) != 0;
    }

    public ConnectionRedirect ClearKnownHosts() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public List<Object> GetKnownHosts() {
        return _KnownHosts;
    }

    public ConnectionRedirect SetKnownHosts(List<Object> value) {
        _KnownHosts = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr16(_Host);
        if ((packing_flags & 512) != 0)
            enc.WriteArray(_KnownHosts);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Host = dec.ReadStr16();
        if ((packing_flags & 512) != 0)
            _KnownHosts = dec.ReadArray();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Host", GetHost());
        	if ((packing_flags & 512) != 0)
            	result.Add("_KnownHosts", GetKnownHosts());

			return result;
        }
    }

}
}
