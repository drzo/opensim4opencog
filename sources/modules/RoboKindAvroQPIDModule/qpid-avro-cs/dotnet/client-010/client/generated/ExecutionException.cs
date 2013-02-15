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



public sealed class ExecutionException : Method {

    public const int TYPE = 771;

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
       get{ return Frame.L4; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private ExecutionErrorCode _ErrorCode;
    private int _CommandId;
    private short _ClassCode;
    private short _CommandCode;
    private short _FieldIndex;
    private String _Description;
    private Dictionary<String,Object> _ErrorInfo;


    public ExecutionException() {}


    public ExecutionException(ExecutionErrorCode ErrorCode, int CommandId, short ClassCode, short CommandCode, short FieldIndex, String Description, Dictionary<String,Object> ErrorInfo, params Option[] options) {
        SetErrorCode(ErrorCode);
        SetCommandId(CommandId);
        SetClassCode(ClassCode);
        SetCommandCode(CommandCode);
        SetFieldIndex(FieldIndex);
        SetDescription(Description);
        SetErrorInfo(ErrorInfo);

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
        mdelegate.ExecutionException(context, this);
    }


    public bool HasErrorCode() {
        return (packing_flags & 256) != 0;
    }

    public ExecutionException ClearErrorCode() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public ExecutionErrorCode GetErrorCode() {
        return _ErrorCode;
    }

    public ExecutionException SetErrorCode(ExecutionErrorCode value) {
        _ErrorCode = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasCommandId() {
        return (packing_flags & 512) != 0;
    }

    public ExecutionException ClearCommandId() {
        packing_flags = (byte) (packing_flags & ~512);       
        _CommandId =  0;
        Dirty = true;
        return this;
    }

    public int GetCommandId() {
        return _CommandId;
    }

    public ExecutionException SetCommandId(int value) {
        _CommandId = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasClassCode() {
        return (packing_flags & 1024) != 0;
    }

    public ExecutionException ClearClassCode() {
        packing_flags = (byte) (packing_flags & ~1024);       
        _ClassCode =  0;
        Dirty = true;
        return this;
    }

    public short GetClassCode() {
        return _ClassCode;
    }

    public ExecutionException SetClassCode(short value) {
        _ClassCode = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasCommandCode() {
        return (packing_flags & 2048) != 0;
    }

    public ExecutionException ClearCommandCode() {
        packing_flags = (byte) (packing_flags & ~2048);       
        _CommandCode =  0;
        Dirty = true;
        return this;
    }

    public short GetCommandCode() {
        return _CommandCode;
    }

    public ExecutionException SetCommandCode(short value) {
        _CommandCode = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }


    public bool HasFieldIndex() {
        return (packing_flags & 4096) != 0;
    }

    public ExecutionException ClearFieldIndex() {
        packing_flags = (byte) (packing_flags & ~4096);       
        _FieldIndex =  0;
        Dirty = true;
        return this;
    }

    public short GetFieldIndex() {
        return _FieldIndex;
    }

    public ExecutionException SetFieldIndex(short value) {
        _FieldIndex = value;
        packing_flags |=  4096;
        Dirty = true;
        return this;
    }


    public bool HasDescription() {
        return (packing_flags & 8192) != 0;
    }

    public ExecutionException ClearDescription() {
        packing_flags = (byte) (packing_flags & ~8192);       

        Dirty = true;
        return this;
    }

    public String GetDescription() {
        return _Description;
    }

    public ExecutionException SetDescription(String value) {
        _Description = value;
        packing_flags |=  8192;
        Dirty = true;
        return this;
    }


    public bool HasErrorInfo() {
        return (packing_flags & 16384) != 0;
    }

    public ExecutionException ClearErrorInfo() {
        packing_flags = (byte) (packing_flags & ~16384);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetErrorInfo() {
        return _ErrorInfo;
    }

    public ExecutionException SetErrorInfo(Dictionary<String,Object> value) {
        _ErrorInfo = value;
        packing_flags |=  16384;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteUint16((short)_ErrorCode);
        if ((packing_flags & 512) != 0)
            enc.WriteSequenceNo(_CommandId);
        if ((packing_flags & 1024) != 0)
            enc.WriteUint8(_ClassCode);
        if ((packing_flags & 2048) != 0)
            enc.WriteUint8(_CommandCode);
        if ((packing_flags & 4096) != 0)
            enc.WriteUint8(_FieldIndex);
        if ((packing_flags & 8192) != 0)
            enc.WriteStr16(_Description);
        if ((packing_flags & 16384) != 0)
            enc.WriteMap(_ErrorInfo);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _ErrorCode = ExecutionErrorCodeGetter.Get(dec.ReadUint16());
        if ((packing_flags & 512) != 0)
            _CommandId = dec.ReadSequenceNo();
        if ((packing_flags & 1024) != 0)
            _ClassCode = dec.ReadUint8();
        if ((packing_flags & 2048) != 0)
            _CommandCode = dec.ReadUint8();
        if ((packing_flags & 4096) != 0)
            _FieldIndex = dec.ReadUint8();
        if ((packing_flags & 8192) != 0)
            _Description = dec.ReadStr16();
        if ((packing_flags & 16384) != 0)
            _ErrorInfo = dec.ReadMap();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_ErrorCode", GetErrorCode());
        	if ((packing_flags & 512) != 0)
            	result.Add("_CommandId", GetCommandId());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_ClassCode", GetClassCode());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_CommandCode", GetCommandCode());
        	if ((packing_flags & 4096) != 0)
            	result.Add("_FieldIndex", GetFieldIndex());
        	if ((packing_flags & 8192) != 0)
            	result.Add("_Description", GetDescription());
        	if ((packing_flags & 16384) != 0)
            	result.Add("_ErrorInfo", GetErrorInfo());

			return result;
        }
    }

}
}
