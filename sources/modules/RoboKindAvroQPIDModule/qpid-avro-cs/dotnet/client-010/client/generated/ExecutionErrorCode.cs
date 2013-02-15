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
   public enum ExecutionErrorCode : int
   {
    UNAUTHORIZED_ACCESS= 403,
    NOT_FOUND= 404,
    RESOURCE_LOCKED= 405,
    PRECONDITION_FAILED= 406,
    RESOURCE_DELETED= 408,
    ILLEGAL_STATE= 409,
    COMMAND_INVALID= 503,
    RESOURCE_LIMIT_EXCEEDED= 506,
    NOT_ALLOWED= 530,
    ILLEGAL_ARGUMENT= 531,
    NOT_IMPLEMENTED= 540,
    INTERNAL_ERROR= 541,
    INVALID_ARGUMENT= 542
    }

   public struct ExecutionErrorCodeGetter
   {
    public static ExecutionErrorCode Get(int value)
    {
        switch (value)
        {
          case 403: return ExecutionErrorCode.UNAUTHORIZED_ACCESS;
          case 404: return ExecutionErrorCode.NOT_FOUND;
          case 405: return ExecutionErrorCode.RESOURCE_LOCKED;
          case 406: return ExecutionErrorCode.PRECONDITION_FAILED;
          case 408: return ExecutionErrorCode.RESOURCE_DELETED;
          case 409: return ExecutionErrorCode.ILLEGAL_STATE;
          case 503: return ExecutionErrorCode.COMMAND_INVALID;
          case 506: return ExecutionErrorCode.RESOURCE_LIMIT_EXCEEDED;
          case 530: return ExecutionErrorCode.NOT_ALLOWED;
          case 531: return ExecutionErrorCode.ILLEGAL_ARGUMENT;
          case 540: return ExecutionErrorCode.NOT_IMPLEMENTED;
          case 541: return ExecutionErrorCode.INTERNAL_ERROR;
          case 542: return ExecutionErrorCode.INVALID_ARGUMENT;
        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
