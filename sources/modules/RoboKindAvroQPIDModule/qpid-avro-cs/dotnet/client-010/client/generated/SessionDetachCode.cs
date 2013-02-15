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
   public enum SessionDetachCode : short
   {
    NORMAL= 0,
    SESSION_BUSY= 1,
    TRANSPORT_BUSY= 2,
    NOT_ATTACHED= 3,
    UNKNOWN_IDS= 4
    }

   public struct SessionDetachCodeGetter
   {
    public static SessionDetachCode Get(short value)
    {
        switch (value)
        {
          case 0: return SessionDetachCode.NORMAL;
          case 1: return SessionDetachCode.SESSION_BUSY;
          case 2: return SessionDetachCode.TRANSPORT_BUSY;
          case 3: return SessionDetachCode.NOT_ATTACHED;
          case 4: return SessionDetachCode.UNKNOWN_IDS;
        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
