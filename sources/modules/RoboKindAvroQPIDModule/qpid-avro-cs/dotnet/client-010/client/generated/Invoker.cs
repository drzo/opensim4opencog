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
using common.org.apache.qpid.transport.util;

namespace org.apache.qpid.transport
{

public abstract class Invoker : IInvoker {

    protected abstract void Invoke(Method method);
    public abstract IFuture Invoke(Method method, IFuture resultClass);


    public void ConnectionStart(Dictionary<String,Object> ServerProperties, List<Object> Mechanisms, List<Object> Locales, params Option[] options) {
        Invoke(new ConnectionStart(ServerProperties, Mechanisms, Locales, options));
    }

    public void ConnectionStartOk(Dictionary<String,Object> ClientProperties, String Mechanism, byte[] Response, String Locale, params Option[] options) {
        Invoke(new ConnectionStartOk(ClientProperties, Mechanism, Response, Locale, options));
    }

    public void ConnectionSecure(byte[] Challenge, params Option[] options) {
        Invoke(new ConnectionSecure(Challenge, options));
    }

    public void ConnectionSecureOk(byte[] Response, params Option[] options) {
        Invoke(new ConnectionSecureOk(Response, options));
    }

    public void ConnectionTune(int ChannelMax, int MaxFrameSize, int HeartbeatMin, int HeartbeatMax, params Option[] options) {
        Invoke(new ConnectionTune(ChannelMax, MaxFrameSize, HeartbeatMin, HeartbeatMax, options));
    }

    public void ConnectionTuneOk(int ChannelMax, int MaxFrameSize, int Heartbeat, params Option[] options) {
        Invoke(new ConnectionTuneOk(ChannelMax, MaxFrameSize, Heartbeat, options));
    }

    public void ConnectionOpen(String VirtualHost, List<Object> Capabilities, params Option[] options) {
        Invoke(new ConnectionOpen(VirtualHost, Capabilities, options));
    }

    public void ConnectionOpenOk(List<Object> KnownHosts, params Option[] options) {
        Invoke(new ConnectionOpenOk(KnownHosts, options));
    }

    public void ConnectionRedirect(String Host, List<Object> KnownHosts, params Option[] options) {
        Invoke(new ConnectionRedirect(Host, KnownHosts, options));
    }

    public void ConnectionHeartbeat(params Option[] options) {
        Invoke(new ConnectionHeartbeat(options));
    }

    public void ConnectionClose(ConnectionCloseCode ReplyCode, String ReplyText, params Option[] options) {
        Invoke(new ConnectionClose(ReplyCode, ReplyText, options));
    }

    public void ConnectionCloseOk(params Option[] options) {
        Invoke(new ConnectionCloseOk(options));
    }

    public void SessionAttach(byte[] Name, params Option[] options) {
        Invoke(new SessionAttach(Name, options));
    }

    public void SessionAttached(byte[] Name, params Option[] options) {
        Invoke(new SessionAttached(Name, options));
    }

    public void SessionDetach(byte[] Name, params Option[] options) {
        Invoke(new SessionDetach(Name, options));
    }

    public void SessionDetached(byte[] Name, SessionDetachCode Code, params Option[] options) {
        Invoke(new SessionDetached(Name, Code, options));
    }

    public void SessionRequestTimeout(long Timeout, params Option[] options) {
        Invoke(new SessionRequestTimeout(Timeout, options));
    }

    public void SessionTimeout(long Timeout, params Option[] options) {
        Invoke(new SessionTimeout(Timeout, options));
    }

    public void SessionCommandPoint(int CommandId, long CommandOffset, params Option[] options) {
        Invoke(new SessionCommandPoint(CommandId, CommandOffset, options));
    }

    public void SessionExpected(RangeSet Commands, List<Object> Fragments, params Option[] options) {
        Invoke(new SessionExpected(Commands, Fragments, options));
    }

    public void SessionConfirmed(RangeSet Commands, List<Object> Fragments, params Option[] options) {
        Invoke(new SessionConfirmed(Commands, Fragments, options));
    }

    public void SessionCompleted(RangeSet Commands, params Option[] options) {
        Invoke(new SessionCompleted(Commands, options));
    }

    public void SessionKnownCompleted(RangeSet Commands, params Option[] options) {
        Invoke(new SessionKnownCompleted(Commands, options));
    }

    public void SessionFlush(params Option[] options) {
        Invoke(new SessionFlush(options));
    }

    public void SessionGap(RangeSet Commands, params Option[] options) {
        Invoke(new SessionGap(Commands, options));
    }

    public void ExecutionSync(params Option[] options) {
        Invoke(new ExecutionSync(options));
    }

    public void ExecutionResult(int CommandId, Struct Value, params Option[] options) {
        Invoke(new ExecutionResult(CommandId, Value, options));
    }

    public void ExecutionException(ExecutionErrorCode ErrorCode, int CommandId, short ClassCode, short CommandCode, short FieldIndex, String Description, Dictionary<String,Object> ErrorInfo, params Option[] options) {
        Invoke(new ExecutionException(ErrorCode, CommandId, ClassCode, CommandCode, FieldIndex, Description, ErrorInfo, options));
    }

    public void MessageTransfer(String Destination, MessageAcceptMode AcceptMode, MessageAcquireMode AcquireMode, Header header, MemoryStream body, params Option[] options) {
        Invoke(new MessageTransfer(Destination, AcceptMode, AcquireMode, header, body, options));
    }

    public void MessageAccept(RangeSet Transfers, params Option[] options) {
        Invoke(new MessageAccept(Transfers, options));
    }

    public void MessageReject(RangeSet Transfers, MessageRejectCode Code, String Text, params Option[] options) {
        Invoke(new MessageReject(Transfers, Code, Text, options));
    }

    public void MessageRelease(RangeSet Transfers, params Option[] options) {
        Invoke(new MessageRelease(Transfers, options));
    }

    public IFuture MessageAcquire(RangeSet Transfers, params Option[] options) {
        return Invoke(new MessageAcquire(Transfers, options), new ResultFuture());
    }

    public IFuture MessageResume(String Destination, String ResumeId, params Option[] options) {
        return Invoke(new MessageResume(Destination, ResumeId, options), new ResultFuture());
    }

    public void MessageSubscribe(String Queue, String Destination, MessageAcceptMode AcceptMode, MessageAcquireMode AcquireMode, String ResumeId, long ResumeTtl, Dictionary<String,Object> Arguments, params Option[] options) {
        Invoke(new MessageSubscribe(Queue, Destination, AcceptMode, AcquireMode, ResumeId, ResumeTtl, Arguments, options));
    }

    public void MessageCancel(String Destination, params Option[] options) {
        Invoke(new MessageCancel(Destination, options));
    }

    public void MessageSetFlowMode(String Destination, MessageFlowMode FlowMode, params Option[] options) {
        Invoke(new MessageSetFlowMode(Destination, FlowMode, options));
    }

    public void MessageFlow(String Destination, MessageCreditUnit Unit, long Value, params Option[] options) {
        Invoke(new MessageFlow(Destination, Unit, Value, options));
    }

    public void MessageFlush(String Destination, params Option[] options) {
        Invoke(new MessageFlush(Destination, options));
    }

    public void MessageStop(String Destination, params Option[] options) {
        Invoke(new MessageStop(Destination, options));
    }

    public void TxSelect(params Option[] options) {
        Invoke(new TxSelect(options));
    }

    public void TxCommit(params Option[] options) {
        Invoke(new TxCommit(options));
    }

    public void TxRollback(params Option[] options) {
        Invoke(new TxRollback(options));
    }

    public void DtxSelect(params Option[] options) {
        Invoke(new DtxSelect(options));
    }

    public IFuture DtxStart(Xid Xid, params Option[] options) {
        return Invoke(new DtxStart(Xid, options), new ResultFuture());
    }

    public IFuture DtxEnd(Xid Xid, params Option[] options) {
        return Invoke(new DtxEnd(Xid, options), new ResultFuture());
    }

    public IFuture DtxCommit(Xid Xid, params Option[] options) {
        return Invoke(new DtxCommit(Xid, options), new ResultFuture());
    }

    public void DtxForget(Xid Xid, params Option[] options) {
        Invoke(new DtxForget(Xid, options));
    }

    public IFuture DtxGetTimeout(Xid Xid, params Option[] options) {
        return Invoke(new DtxGetTimeout(Xid, options), new ResultFuture());
    }

    public IFuture DtxPrepare(Xid Xid, params Option[] options) {
        return Invoke(new DtxPrepare(Xid, options), new ResultFuture());
    }

    public IFuture DtxRecover(params Option[] options) {
        return Invoke(new DtxRecover(options), new ResultFuture());
    }

    public IFuture DtxRollback(Xid Xid, params Option[] options) {
        return Invoke(new DtxRollback(Xid, options), new ResultFuture());
    }

    public void DtxSetTimeout(Xid Xid, long Timeout, params Option[] options) {
        Invoke(new DtxSetTimeout(Xid, Timeout, options));
    }

    public void ExchangeDeclare(String Exchange, String Type, String AlternateExchange, Dictionary<String,Object> Arguments, params Option[] options) {
        Invoke(new ExchangeDeclare(Exchange, Type, AlternateExchange, Arguments, options));
    }

    public void ExchangeDelete(String Exchange, params Option[] options) {
        Invoke(new ExchangeDelete(Exchange, options));
    }

    public IFuture ExchangeQuery(String Name, params Option[] options) {
        return Invoke(new ExchangeQuery(Name, options), new ResultFuture());
    }

    public void ExchangeBind(String Queue, String Exchange, String BindingKey, Dictionary<String,Object> Arguments, params Option[] options) {
        Invoke(new ExchangeBind(Queue, Exchange, BindingKey, Arguments, options));
    }

    public void ExchangeUnbind(String Queue, String Exchange, String BindingKey, params Option[] options) {
        Invoke(new ExchangeUnbind(Queue, Exchange, BindingKey, options));
    }

    public IFuture ExchangeBound(String Exchange, String Queue, String BindingKey, Dictionary<String,Object> Arguments, params Option[] options) {
        return Invoke(new ExchangeBound(Exchange, Queue, BindingKey, Arguments, options), new ResultFuture());
    }

    public void QueueDeclare(String Queue, String AlternateExchange, Dictionary<String,Object> Arguments, params Option[] options) {
        Invoke(new QueueDeclare(Queue, AlternateExchange, Arguments, options));
    }

    public void QueueDelete(String Queue, params Option[] options) {
        Invoke(new QueueDelete(Queue, options));
    }

    public void QueuePurge(String Queue, params Option[] options) {
        Invoke(new QueuePurge(Queue, options));
    }

    public IFuture QueueQuery(String Queue, params Option[] options) {
        return Invoke(new QueueQuery(Queue, options), new ResultFuture());
    }


}
}
