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
   public enum MessageDeliveryPriority : short
   {
    LOWEST= 0,
    LOWER= 1,
    LOW= 2,
    BELOW_AVERAGE= 3,
    MEDIUM= 4,
    ABOVE_AVERAGE= 5,
    HIGH= 6,
    HIGHER= 7,
    VERY_HIGH= 8,
    HIGHEST= 9
    }

   public struct MessageDeliveryPriorityGetter
   {
    public static MessageDeliveryPriority Get(short value)
    {
        switch (value)
        {
          case 0: return MessageDeliveryPriority.LOWEST;
          case 1: return MessageDeliveryPriority.LOWER;
          case 2: return MessageDeliveryPriority.LOW;
          case 3: return MessageDeliveryPriority.BELOW_AVERAGE;
          case 4: return MessageDeliveryPriority.MEDIUM;
          case 5: return MessageDeliveryPriority.ABOVE_AVERAGE;
          case 6: return MessageDeliveryPriority.HIGH;
          case 7: return MessageDeliveryPriority.HIGHER;
          case 8: return MessageDeliveryPriority.VERY_HIGH;
          case 9: return MessageDeliveryPriority.HIGHEST;
        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
