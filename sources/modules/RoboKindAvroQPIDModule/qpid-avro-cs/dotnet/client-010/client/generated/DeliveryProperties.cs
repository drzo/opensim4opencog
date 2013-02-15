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



public sealed class DeliveryProperties : Struct {

    public const int TYPE = 1025;

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
    private MessageDeliveryPriority _Priority;
    private MessageDeliveryMode _DeliveryMode;
    private long _Ttl;
    private long _Timestamp;
    private long _Expiration;
    private String _Exchange;
    private String _RoutingKey;
    private String _ResumeId;
    private long _ResumeTtl;


    public DeliveryProperties() {}


    public DeliveryProperties(MessageDeliveryPriority Priority, MessageDeliveryMode DeliveryMode, long Ttl, long Timestamp, long Expiration, String Exchange, String RoutingKey, String ResumeId, long ResumeTtl, params Option[] options) {
        SetPriority(Priority);
        SetDeliveryMode(DeliveryMode);
        SetTtl(Ttl);
        SetTimestamp(Timestamp);
        SetExpiration(Expiration);
        SetExchange(Exchange);
        SetRoutingKey(RoutingKey);
        SetResumeId(ResumeId);
        SetResumeTtl(ResumeTtl);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.DISCARD_UNROUTABLE: packing_flags |= 256; break;
            case Option.IMMEDIATE: packing_flags |= 512; break;
            case Option.REDELIVERED: packing_flags |= 1024; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.DeliveryProperties(context, this);
    }


    public bool HasDiscardUnroutable() {
        return (packing_flags & 256) != 0;
    }

    public DeliveryProperties ClearDiscardUnroutable() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public bool GetDiscardUnroutable() {
        return HasDiscardUnroutable();
    }

    public DeliveryProperties SetDiscardUnroutable(bool value) {

        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasImmediate() {
        return (packing_flags & 512) != 0;
    }

    public DeliveryProperties ClearImmediate() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public bool GetImmediate() {
        return HasImmediate();
    }

    public DeliveryProperties SetImmediate(bool value) {

        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasRedelivered() {
        return (packing_flags & 1024) != 0;
    }

    public DeliveryProperties ClearRedelivered() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public bool GetRedelivered() {
        return HasRedelivered();
    }

    public DeliveryProperties SetRedelivered(bool value) {

        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasPriority() {
        return (packing_flags & 2048) != 0;
    }

    public DeliveryProperties ClearPriority() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public MessageDeliveryPriority GetPriority() {
        return _Priority;
    }

    public DeliveryProperties SetPriority(MessageDeliveryPriority value) {
        _Priority = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }


    public bool HasDeliveryMode() {
        return (packing_flags & 4096) != 0;
    }

    public DeliveryProperties ClearDeliveryMode() {
        packing_flags = (byte) (packing_flags & ~4096);       

        Dirty = true;
        return this;
    }

    public MessageDeliveryMode GetDeliveryMode() {
        return _DeliveryMode;
    }

    public DeliveryProperties SetDeliveryMode(MessageDeliveryMode value) {
        _DeliveryMode = value;
        packing_flags |=  4096;
        Dirty = true;
        return this;
    }


    public bool HasTtl() {
        return (packing_flags & 8192) != 0;
    }

    public DeliveryProperties ClearTtl() {
        packing_flags = (byte) (packing_flags & ~8192);       
        _Ttl =  0;
        Dirty = true;
        return this;
    }

    public long GetTtl() {
        return _Ttl;
    }

    public DeliveryProperties SetTtl(long value) {
        _Ttl = value;
        packing_flags |=  8192;
        Dirty = true;
        return this;
    }


    public bool HasTimestamp() {
        return (packing_flags & 16384) != 0;
    }

    public DeliveryProperties ClearTimestamp() {
        packing_flags = (byte) (packing_flags & ~16384);       
        _Timestamp =  0;
        Dirty = true;
        return this;
    }

    public long GetTimestamp() {
        return _Timestamp;
    }

    public DeliveryProperties SetTimestamp(long value) {
        _Timestamp = value;
        packing_flags |=  16384;
        Dirty = true;
        return this;
    }


    public bool HasExpiration() {
        return (packing_flags & 32768) != 0;
    }

    public DeliveryProperties ClearExpiration() {
        packing_flags = (byte) (packing_flags & ~32768);       
        _Expiration =  0;
        Dirty = true;
        return this;
    }

    public long GetExpiration() {
        return _Expiration;
    }

    public DeliveryProperties SetExpiration(long value) {
        _Expiration = value;
        packing_flags |=  32768;
        Dirty = true;
        return this;
    }


    public bool HasExchange() {
        return (packing_flags & 1) != 0;
    }

    public DeliveryProperties ClearExchange() {
        packing_flags = (byte) (packing_flags & ~1);       

        Dirty = true;
        return this;
    }

    public String GetExchange() {
        return _Exchange;
    }

    public DeliveryProperties SetExchange(String value) {
        _Exchange = value;
        packing_flags |=  1;
        Dirty = true;
        return this;
    }


    public bool HasRoutingKey() {
        return (packing_flags & 2) != 0;
    }

    public DeliveryProperties ClearRoutingKey() {
        packing_flags = (byte) (packing_flags & ~2);       

        Dirty = true;
        return this;
    }

    public String GetRoutingKey() {
        return _RoutingKey;
    }

    public DeliveryProperties SetRoutingKey(String value) {
        _RoutingKey = value;
        packing_flags |=  2;
        Dirty = true;
        return this;
    }


    public bool HasResumeId() {
        return (packing_flags & 4) != 0;
    }

    public DeliveryProperties ClearResumeId() {
        packing_flags = (byte) (packing_flags & ~4);       

        Dirty = true;
        return this;
    }

    public String GetResumeId() {
        return _ResumeId;
    }

    public DeliveryProperties SetResumeId(String value) {
        _ResumeId = value;
        packing_flags |=  4;
        Dirty = true;
        return this;
    }


    public bool HasResumeTtl() {
        return (packing_flags & 8) != 0;
    }

    public DeliveryProperties ClearResumeTtl() {
        packing_flags = (byte) (packing_flags & ~8);       
        _ResumeTtl =  0;
        Dirty = true;
        return this;
    }

    public long GetResumeTtl() {
        return _ResumeTtl;
    }

    public DeliveryProperties SetResumeTtl(long value) {
        _ResumeTtl = value;
        packing_flags |=  8;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 2048) != 0)
            enc.WriteUint8((short)_Priority);
        if ((packing_flags & 4096) != 0)
            enc.WriteUint8((short)_DeliveryMode);
        if ((packing_flags & 8192) != 0)
            enc.WriteUint64(_Ttl);
        if ((packing_flags & 16384) != 0)
            enc.WriteDatetime(_Timestamp);
        if ((packing_flags & 32768) != 0)
            enc.WriteDatetime(_Expiration);
        if ((packing_flags & 1) != 0)
            enc.WriteStr8(_Exchange);
        if ((packing_flags & 2) != 0)
            enc.WriteStr8(_RoutingKey);
        if ((packing_flags & 4) != 0)
            enc.WriteStr16(_ResumeId);
        if ((packing_flags & 8) != 0)
            enc.WriteUint64(_ResumeTtl);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 2048) != 0)
            _Priority = MessageDeliveryPriorityGetter.Get(dec.ReadUint8());
        if ((packing_flags & 4096) != 0)
            _DeliveryMode = MessageDeliveryModeGetter.Get(dec.ReadUint8());
        if ((packing_flags & 8192) != 0)
            _Ttl = dec.ReadUint64();
        if ((packing_flags & 16384) != 0)
            _Timestamp = dec.ReadDatetime();
        if ((packing_flags & 32768) != 0)
            _Expiration = dec.ReadDatetime();
        if ((packing_flags & 1) != 0)
            _Exchange = dec.ReadStr8();
        if ((packing_flags & 2) != 0)
            _RoutingKey = dec.ReadStr8();
        if ((packing_flags & 4) != 0)
            _ResumeId = dec.ReadStr16();
        if ((packing_flags & 8) != 0)
            _ResumeTtl = dec.ReadUint64();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_DiscardUnroutable", GetDiscardUnroutable());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Immediate", GetImmediate());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_Redelivered", GetRedelivered());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_Priority", GetPriority());
        	if ((packing_flags & 4096) != 0)
            	result.Add("_DeliveryMode", GetDeliveryMode());
        	if ((packing_flags & 8192) != 0)
            	result.Add("_Ttl", GetTtl());
        	if ((packing_flags & 16384) != 0)
            	result.Add("_Timestamp", GetTimestamp());
        	if ((packing_flags & 32768) != 0)
            	result.Add("_Expiration", GetExpiration());
        	if ((packing_flags & 1) != 0)
            	result.Add("_Exchange", GetExchange());
        	if ((packing_flags & 2) != 0)
            	result.Add("_RoutingKey", GetRoutingKey());
        	if ((packing_flags & 4) != 0)
            	result.Add("_ResumeId", GetResumeId());
        	if ((packing_flags & 8) != 0)
            	result.Add("_ResumeTtl", GetResumeTtl());

			return result;
        }
    }

}
}
