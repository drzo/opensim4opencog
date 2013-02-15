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
   public enum SegmentType : short
   {
    CONTROL= 0,
    COMMAND= 1,
    HEADER= 2,
    BODY= 3
    }

   public struct SegmentTypeGetter
   {
    public static SegmentType Get(short value)
    {
        switch (value)
        {
          case 0: return SegmentType.CONTROL;
          case 1: return SegmentType.COMMAND;
          case 2: return SegmentType.HEADER;
          case 3: return SegmentType.BODY;
        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
