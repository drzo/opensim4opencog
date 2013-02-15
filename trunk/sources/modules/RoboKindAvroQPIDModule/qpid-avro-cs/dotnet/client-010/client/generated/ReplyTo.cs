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



public sealed class ReplyTo : Struct {

    public const int TYPE = -3;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 2;
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
    private String _Exchange;
    private String _RoutingKey;


    public ReplyTo() {}


    public ReplyTo(String Exchange, String RoutingKey) {
        SetExchange(Exchange);
        SetRoutingKey(RoutingKey);

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.ReplyTo(context, this);
    }


    public bool HasExchange() {
        return (packing_flags & 256) != 0;
    }

    public ReplyTo ClearExchange() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetExchange() {
        return _Exchange;
    }

    public ReplyTo SetExchange(String value) {
        _Exchange = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasRoutingKey() {
        return (packing_flags & 512) != 0;
    }

    public ReplyTo ClearRoutingKey() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public String GetRoutingKey() {
        return _RoutingKey;
    }

    public ReplyTo SetRoutingKey(String value) {
        _RoutingKey = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr8(_Exchange);
        if ((packing_flags & 512) != 0)
            enc.WriteStr8(_RoutingKey);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Exchange = dec.ReadStr8();
        if ((packing_flags & 512) != 0)
            _RoutingKey = dec.ReadStr8();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Exchange", GetExchange());
        	if ((packing_flags & 512) != 0)
            	result.Add("_RoutingKey", GetRoutingKey());

			return result;
        }
    }

}
}
