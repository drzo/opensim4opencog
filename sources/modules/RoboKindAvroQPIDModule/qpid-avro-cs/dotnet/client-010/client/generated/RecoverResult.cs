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



public sealed class RecoverResult : Struct {

    public const int TYPE = 1539;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 4;
    }

    public override int GetPackWidth() {
        return 2;
    }

    public  bool HasPayload() {
        return false;
    }

    public  byte EncodedTrack 
    {
       get{ return 4; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private List<Object> _InDoubt;


    public RecoverResult() {}


    public RecoverResult(List<Object> InDoubt) {
        SetInDoubt(InDoubt);

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.RecoverResult(context, this);
    }


    public bool HasInDoubt() {
        return (packing_flags & 256) != 0;
    }

    public RecoverResult ClearInDoubt() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public List<Object> GetInDoubt() {
        return _InDoubt;
    }

    public RecoverResult SetInDoubt(List<Object> value) {
        _InDoubt = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteArray(_InDoubt);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _InDoubt = dec.ReadArray();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_InDoubt", GetInDoubt());

			return result;
        }
    }

}
}
