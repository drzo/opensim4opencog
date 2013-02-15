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
   public enum DtxXaStatus : int
   {
    XA_OK= 0,
    XA_RBROLLBACK= 1,
    XA_RBTIMEOUT= 2,
    XA_HEURHAZ= 3,
    XA_HEURCOM= 4,
    XA_HEURRB= 5,
    XA_HEURMIX= 6,
    XA_RDONLY= 7
    }

   public struct DtxXaStatusGetter
   {
    public static DtxXaStatus Get(int value)
    {
        switch (value)
        {
          case 0: return DtxXaStatus.XA_OK;
          case 1: return DtxXaStatus.XA_RBROLLBACK;
          case 2: return DtxXaStatus.XA_RBTIMEOUT;
          case 3: return DtxXaStatus.XA_HEURHAZ;
          case 4: return DtxXaStatus.XA_HEURCOM;
          case 5: return DtxXaStatus.XA_HEURRB;
          case 6: return DtxXaStatus.XA_HEURMIX;
          case 7: return DtxXaStatus.XA_RDONLY;
        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
