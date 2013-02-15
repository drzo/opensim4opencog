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



public sealed class SessionHeader : Struct {

    public const int TYPE = -1;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 1;
    }

    public override int GetPackWidth() {
        return 1;
    }

    public  bool HasPayload() {
        return false;
    }

    public  byte EncodedTrack 
    {
       get{ return 4; }
       set { throw new NotImplementedException(); }
    }

    private byte packing_flags = 0;


    public SessionHeader() {}


    public SessionHeader(params Option[] options) {

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.SYNC: packing_flags |= 1; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.SessionHeader(context, this);
    }


    public bool HasSync() {
        return (packing_flags & 1) != 0;
    }

    public SessionHeader ClearSync() {
        packing_flags = (byte) (packing_flags & ~1);       

        Dirty = true;
        return this;
    }

    public bool GetSync() {
        return HasSync();
    }

    public SessionHeader SetSync(bool value) {

        packing_flags |=  1;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint8(packing_flags);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (byte) dec.ReadUint8();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 1) != 0)
            	result.Add("_Sync", GetSync());

			return result;
        }
    }

}
}
