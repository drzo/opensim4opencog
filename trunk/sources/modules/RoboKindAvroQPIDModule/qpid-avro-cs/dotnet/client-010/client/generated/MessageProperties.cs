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



public sealed class MessageProperties : Struct {

    public const int TYPE = 1027;

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
    private long _ContentLength;
    private UUID _MessageId;
    private byte[] _CorrelationId;
    private ReplyTo _ReplyTo;
    private String _ContentType;
    private String _ContentEncoding;
    private byte[] _UserId;
    private byte[] _AppId;
    private Dictionary<String,Object> _ApplicationHeaders;


    public MessageProperties() {}


    public MessageProperties(long ContentLength, UUID MessageId, byte[] CorrelationId, ReplyTo ReplyTo, String ContentType, String ContentEncoding, byte[] UserId, byte[] AppId, Dictionary<String,Object> ApplicationHeaders) {
        SetContentLength(ContentLength);
        SetMessageId(MessageId);
        SetCorrelationId(CorrelationId);
        SetReplyTo(ReplyTo);
        SetContentType(ContentType);
        SetContentEncoding(ContentEncoding);
        SetUserId(UserId);
        SetAppId(AppId);
        SetApplicationHeaders(ApplicationHeaders);

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.MessageProperties(context, this);
    }


    public bool HasContentLength() {
        return (packing_flags & 256) != 0;
    }

    public MessageProperties ClearContentLength() {
        packing_flags = (byte) (packing_flags & ~256);       
        _ContentLength =  0;
        Dirty = true;
        return this;
    }

    public long GetContentLength() {
        return _ContentLength;
    }

    public MessageProperties SetContentLength(long value) {
        _ContentLength = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasMessageId() {
        return (packing_flags & 512) != 0;
    }

    public MessageProperties ClearMessageId() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public UUID GetMessageId() {
        return _MessageId;
    }

    public MessageProperties SetMessageId(UUID value) {
        _MessageId = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasCorrelationId() {
        return (packing_flags & 1024) != 0;
    }

    public MessageProperties ClearCorrelationId() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public byte[] GetCorrelationId() {
        return _CorrelationId;
    }

    public MessageProperties SetCorrelationId(byte[] value) {
        _CorrelationId = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasReplyTo() {
        return (packing_flags & 2048) != 0;
    }

    public MessageProperties ClearReplyTo() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public ReplyTo GetReplyTo() {
        return _ReplyTo;
    }

    public MessageProperties SetReplyTo(ReplyTo value) {
        _ReplyTo = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }


    public bool HasContentType() {
        return (packing_flags & 4096) != 0;
    }

    public MessageProperties ClearContentType() {
        packing_flags = (byte) (packing_flags & ~4096);       

        Dirty = true;
        return this;
    }

    public String GetContentType() {
        return _ContentType;
    }

    public MessageProperties SetContentType(String value) {
        _ContentType = value;
        packing_flags |=  4096;
        Dirty = true;
        return this;
    }


    public bool HasContentEncoding() {
        return (packing_flags & 8192) != 0;
    }

    public MessageProperties ClearContentEncoding() {
        packing_flags = (byte) (packing_flags & ~8192);       

        Dirty = true;
        return this;
    }

    public String GetContentEncoding() {
        return _ContentEncoding;
    }

    public MessageProperties SetContentEncoding(String value) {
        _ContentEncoding = value;
        packing_flags |=  8192;
        Dirty = true;
        return this;
    }


    public bool HasUserId() {
        return (packing_flags & 16384) != 0;
    }

    public MessageProperties ClearUserId() {
        packing_flags = (byte) (packing_flags & ~16384);       

        Dirty = true;
        return this;
    }

    public byte[] GetUserId() {
        return _UserId;
    }

    public MessageProperties SetUserId(byte[] value) {
        _UserId = value;
        packing_flags |=  16384;
        Dirty = true;
        return this;
    }


    public bool HasAppId() {
        return (packing_flags & 32768) != 0;
    }

    public MessageProperties ClearAppId() {
        packing_flags = (byte) (packing_flags & ~32768);       

        Dirty = true;
        return this;
    }

    public byte[] GetAppId() {
        return _AppId;
    }

    public MessageProperties SetAppId(byte[] value) {
        _AppId = value;
        packing_flags |=  32768;
        Dirty = true;
        return this;
    }


    public bool HasApplicationHeaders() {
        return (packing_flags & 1) != 0;
    }

    public MessageProperties ClearApplicationHeaders() {
        packing_flags = (byte) (packing_flags & ~1);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetApplicationHeaders() {
        return _ApplicationHeaders;
    }

    public MessageProperties SetApplicationHeaders(Dictionary<String,Object> value) {
        _ApplicationHeaders = value;
        packing_flags |=  1;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteUint64(_ContentLength);
        if ((packing_flags & 512) != 0)
            enc.WriteUuid(_MessageId);
        if ((packing_flags & 1024) != 0)
            enc.WriteVbin16(_CorrelationId);
        if ((packing_flags & 2048) != 0)
            enc.WriteStruct(ReplyTo.TYPE, _ReplyTo);
        if ((packing_flags & 4096) != 0)
            enc.WriteStr8(_ContentType);
        if ((packing_flags & 8192) != 0)
            enc.WriteStr8(_ContentEncoding);
        if ((packing_flags & 16384) != 0)
            enc.WriteVbin16(_UserId);
        if ((packing_flags & 32768) != 0)
            enc.WriteVbin16(_AppId);
        if ((packing_flags & 1) != 0)
            enc.WriteMap(_ApplicationHeaders);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _ContentLength = dec.ReadUint64();
        if ((packing_flags & 512) != 0)
            _MessageId = dec.ReadUuid();
        if ((packing_flags & 1024) != 0)
            _CorrelationId = dec.ReadVbin16();
        if ((packing_flags & 2048) != 0)
            _ReplyTo = (ReplyTo)dec.ReadStruct(ReplyTo.TYPE);
        if ((packing_flags & 4096) != 0)
            _ContentType = dec.ReadStr8();
        if ((packing_flags & 8192) != 0)
            _ContentEncoding = dec.ReadStr8();
        if ((packing_flags & 16384) != 0)
            _UserId = dec.ReadVbin16();
        if ((packing_flags & 32768) != 0)
            _AppId = dec.ReadVbin16();
        if ((packing_flags & 1) != 0)
            _ApplicationHeaders = dec.ReadMap();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_ContentLength", GetContentLength());
        	if ((packing_flags & 512) != 0)
            	result.Add("_MessageId", GetMessageId());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_CorrelationId", GetCorrelationId());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_ReplyTo", GetReplyTo());
        	if ((packing_flags & 4096) != 0)
            	result.Add("_ContentType", GetContentType());
        	if ((packing_flags & 8192) != 0)
            	result.Add("_ContentEncoding", GetContentEncoding());
        	if ((packing_flags & 16384) != 0)
            	result.Add("_UserId", GetUserId());
        	if ((packing_flags & 32768) != 0)
            	result.Add("_AppId", GetAppId());
        	if ((packing_flags & 1) != 0)
            	result.Add("_ApplicationHeaders", GetApplicationHeaders());

			return result;
        }
    }

}
}
