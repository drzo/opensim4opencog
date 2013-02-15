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
using System.Collections.Generic;
using System.IO;

namespace org.apache.qpid.transport
{

public interface IInvoker {

    IFuture Invoke(Method method, IFuture resultClass);


    void ConnectionStart(Dictionary<String,Object> ServerProperties, List<Object> Mechanisms, List<Object> Locales, params Option[] options);

    void ConnectionStartOk(Dictionary<String,Object> ClientProperties, String Mechanism, byte[] Response, String Locale, params Option[] options);

    void ConnectionSecure(byte[] Challenge, params Option[] options);

    void ConnectionSecureOk(byte[] Response, params Option[] options);

    void ConnectionTune(int ChannelMax, int MaxFrameSize, int HeartbeatMin, int HeartbeatMax, params Option[] options);

    void ConnectionTuneOk(int ChannelMax, int MaxFrameSize, int Heartbeat, params Option[] options);

    void ConnectionOpen(String VirtualHost, List<Object> Capabilities, params Option[] options);

    void ConnectionOpenOk(List<Object> KnownHosts, params Option[] options);

    void ConnectionRedirect(String Host, List<Object> KnownHosts, params Option[] options);

    void ConnectionHeartbeat(params Option[] options);

    void ConnectionClose(ConnectionCloseCode ReplyCode, String ReplyText, params Option[] options);

    void ConnectionCloseOk(params Option[] options);

    void SessionAttach(byte[] Name, params Option[] options);

    void SessionAttached(byte[] Name, params Option[] options);

    void SessionDetach(byte[] Name, params Option[] options);

    void SessionDetached(byte[] Name, SessionDetachCode Code, params Option[] options);

    void SessionRequestTimeout(long Timeout, params Option[] options);

    void SessionTimeout(long Timeout, params Option[] options);

    void SessionCommandPoint(int CommandId, long CommandOffset, params Option[] options);

    void SessionExpected(RangeSet Commands, List<Object> Fragments, params Option[] options);

    void SessionConfirmed(RangeSet Commands, List<Object> Fragments, params Option[] options);

    void SessionCompleted(RangeSet Commands, params Option[] options);

    void SessionKnownCompleted(RangeSet Commands, params Option[] options);

    void SessionFlush(params Option[] options);

    void SessionGap(RangeSet Commands, params Option[] options);

    void ExecutionSync(params Option[] options);

    void ExecutionResult(int CommandId, Struct Value, params Option[] options);

    void ExecutionException(ExecutionErrorCode ErrorCode, int CommandId, short ClassCode, short CommandCode, short FieldIndex, String Description, Dictionary<String,Object> ErrorInfo, params Option[] options);

    void MessageTransfer(String Destination, MessageAcceptMode AcceptMode, MessageAcquireMode AcquireMode, Header header, MemoryStream body, params Option[] options);

    void MessageAccept(RangeSet Transfers, params Option[] options);

    void MessageReject(RangeSet Transfers, MessageRejectCode Code, String Text, params Option[] options);

    void MessageRelease(RangeSet Transfers, params Option[] options);

    IFuture MessageAcquire(RangeSet Transfers, params Option[] options);

    IFuture MessageResume(String Destination, String ResumeId, params Option[] options);

    void MessageSubscribe(String Queue, String Destination, MessageAcceptMode AcceptMode, MessageAcquireMode AcquireMode, String ResumeId, long ResumeTtl, Dictionary<String,Object> Arguments, params Option[] options);

    void MessageCancel(String Destination, params Option[] options);

    void MessageSetFlowMode(String Destination, MessageFlowMode FlowMode, params Option[] options);

    void MessageFlow(String Destination, MessageCreditUnit Unit, long Value, params Option[] options);

    void MessageFlush(String Destination, params Option[] options);

    void MessageStop(String Destination, params Option[] options);

    void TxSelect(params Option[] options);

    void TxCommit(params Option[] options);

    void TxRollback(params Option[] options);

    void DtxSelect(params Option[] options);

    IFuture DtxStart(Xid Xid, params Option[] options);

    IFuture DtxEnd(Xid Xid, params Option[] options);

    IFuture DtxCommit(Xid Xid, params Option[] options);

    void DtxForget(Xid Xid, params Option[] options);

    IFuture DtxGetTimeout(Xid Xid, params Option[] options);

    IFuture DtxPrepare(Xid Xid, params Option[] options);

    IFuture DtxRecover(params Option[] options);

    IFuture DtxRollback(Xid Xid, params Option[] options);

    void DtxSetTimeout(Xid Xid, long Timeout, params Option[] options);

    void ExchangeDeclare(String Exchange, String Type, String AlternateExchange, Dictionary<String,Object> Arguments, params Option[] options);

    void ExchangeDelete(String Exchange, params Option[] options);

    IFuture ExchangeQuery(String Name, params Option[] options);

    void ExchangeBind(String Queue, String Exchange, String BindingKey, Dictionary<String,Object> Arguments, params Option[] options);

    void ExchangeUnbind(String Queue, String Exchange, String BindingKey, params Option[] options);

    IFuture ExchangeBound(String Exchange, String Queue, String BindingKey, Dictionary<String,Object> Arguments, params Option[] options);

    void QueueDeclare(String Queue, String AlternateExchange, Dictionary<String,Object> Arguments, params Option[] options);

    void QueueDelete(String Queue, params Option[] options);

    void QueuePurge(String Queue, params Option[] options);

    IFuture QueueQuery(String Queue, params Option[] options);


}
}
