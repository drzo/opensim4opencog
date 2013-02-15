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

namespace org.apache.qpid.transport
{



public struct QpidType
{
    public Code code;
    public int width;
    public bool isfixed;

    public Code Code
    {
        get { return code; }
        set { code = value; }
    }
    
    public int Width
    {
        get { return width; }
        set { width = value; }
    }
    
    public bool Fixed
    {
        get { return isfixed; }
        set { isfixed = value; }
    }
    
    QpidType(Code code, int width, bool isfixed)
    {
        this.code = code;
        this.width = width;
        this.isfixed = isfixed;
    }
    
    public static QpidType get(byte code)
    {
        switch (code)
        {      	
          case 0x00 : return new QpidType(Code.BIN8, 1, true);
          case 0x01 : return new QpidType(Code.INT8, 1, true);
          case 0x02 : return new QpidType(Code.UINT8, 1, true);
          case 0x04 : return new QpidType(Code.CHAR, 1, true);
          case 0x08 : return new QpidType(Code.BOOLEAN, 1, true);
          case 0x10 : return new QpidType(Code.BIN16, 2, true);
          case 0x11 : return new QpidType(Code.INT16, 2, true);
          case 0x12 : return new QpidType(Code.UINT16, 2, true);
          case 0x20 : return new QpidType(Code.BIN32, 4, true);
          case 0x21 : return new QpidType(Code.INT32, 4, true);
          case 0x22 : return new QpidType(Code.UINT32, 4, true);
          case 0x23 : return new QpidType(Code.FLOAT, 4, true);
          case 0x27 : return new QpidType(Code.CHAR_UTF32, 4, true);
          case 0x30 : return new QpidType(Code.BIN64, 8, true);
          case 0x31 : return new QpidType(Code.INT64, 8, true);
          case 0x32 : return new QpidType(Code.UINT64, 8, true);
          case 0x33 : return new QpidType(Code.DOUBLE, 8, true);
          case 0x38 : return new QpidType(Code.DATETIME, 8, true);
          case 0x40 : return new QpidType(Code.BIN128, 16, true);
          case 0x48 : return new QpidType(Code.UUID, 16, true);
          case 0x50 : return new QpidType(Code.BIN256, 32, true);
          case 0x60 : return new QpidType(Code.BIN512, 64, true);
          case 0x70 : return new QpidType(Code.BIN1024, 128, true);
          case 0x80 : return new QpidType(Code.VBIN8, 1, false);
          case 0x84 : return new QpidType(Code.STR8_LATIN, 1, false);
          case 0x85 : return new QpidType(Code.STR8, 1, false);
          case 0x86 : return new QpidType(Code.STR8_UTF16, 1, false);
          case 0x90 : return new QpidType(Code.VBIN16, 2, false);
          case 0x94 : return new QpidType(Code.STR16_LATIN, 2, false);
          case 0x95 : return new QpidType(Code.STR16, 2, false);
          case 0x96 : return new QpidType(Code.STR16_UTF16, 2, false);
          case 0xa0 : return new QpidType(Code.VBIN32, 4, false);
          case 0xa8 : return new QpidType(Code.MAP, 4, false);
          case 0xa9 : return new QpidType(Code.LIST, 4, false);
          case 0xaa : return new QpidType(Code.ARRAY, 4, false);
          case 0xab : return new QpidType(Code.STRUCT32, 4, false);
          case 0xc0 : return new QpidType(Code.BIN40, 5, true);
          case 0xc8 : return new QpidType(Code.DEC32, 5, true);
          case 0xd0 : return new QpidType(Code.BIN72, 9, true);
          case 0xd8 : return new QpidType(Code.DEC64, 9, true);
          case 0xf0 : return new QpidType(Code.VOID, 0, true);
          case 0xf1 : return new QpidType(Code.BIT, 0, true);

          default: throw new Exception("unknown code: " + code);
        }
    }
}

public enum Code : byte
   {    
   BIN8 = 0x00,
   INT8 = 0x01,
   UINT8 = 0x02,
   CHAR = 0x04,
   BOOLEAN = 0x08,
   BIN16 = 0x10,
   INT16 = 0x11,
   UINT16 = 0x12,
   BIN32 = 0x20,
   INT32 = 0x21,
   UINT32 = 0x22,
   FLOAT = 0x23,
   CHAR_UTF32 = 0x27,
   BIN64 = 0x30,
   INT64 = 0x31,
   UINT64 = 0x32,
   DOUBLE = 0x33,
   DATETIME = 0x38,
   BIN128 = 0x40,
   UUID = 0x48,
   BIN256 = 0x50,
   BIN512 = 0x60,
   BIN1024 = 0x70,
   VBIN8 = 0x80,
   STR8_LATIN = 0x84,
   STR8 = 0x85,
   STR8_UTF16 = 0x86,
   VBIN16 = 0x90,
   STR16_LATIN = 0x94,
   STR16 = 0x95,
   STR16_UTF16 = 0x96,
   VBIN32 = 0xa0,
   MAP = 0xa8,
   LIST = 0xa9,
   ARRAY = 0xaa,
   STRUCT32 = 0xab,
   BIN40 = 0xc0,
   DEC32 = 0xc8,
   BIN72 = 0xd0,
   DEC64 = 0xd8,
   VOID = 0xf0,
   BIT = 0xf1,

   }
}
