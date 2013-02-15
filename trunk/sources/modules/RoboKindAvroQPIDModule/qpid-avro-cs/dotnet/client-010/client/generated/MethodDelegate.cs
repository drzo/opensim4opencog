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

namespace org.apache.qpid.transport
{

public abstract class MethodDelegate<C> {

    public virtual void SessionHeader(C context, SessionHeader mystruct) {}
    public virtual void SessionCommandFragment(C context, SessionCommandFragment mystruct) {}
    public virtual void DeliveryProperties(C context, DeliveryProperties mystruct) {}
    public virtual void FragmentProperties(C context, FragmentProperties mystruct) {}
    public virtual void ReplyTo(C context, ReplyTo mystruct) {}
    public virtual void MessageProperties(C context, MessageProperties mystruct) {}
    public virtual void XaResult(C context, XaResult mystruct) {}
    public virtual void Xid(C context, Xid mystruct) {}
    public virtual void Acquired(C context, Acquired mystruct) {}
    public virtual void MessageResumeResult(C context, MessageResumeResult mystruct) {}
    public virtual void GetTimeoutResult(C context, GetTimeoutResult mystruct) {}
    public virtual void RecoverResult(C context, RecoverResult mystruct) {}
    public virtual void ExchangeQueryResult(C context, ExchangeQueryResult mystruct) {}
    public virtual void ExchangeBoundResult(C context, ExchangeBoundResult mystruct) {}
    public virtual void QueueQueryResult(C context, QueueQueryResult mystruct) {}
    public virtual void ConnectionStart(C context, ConnectionStart mystruct) {}
    public virtual void ConnectionStartOk(C context, ConnectionStartOk mystruct) {}
    public virtual void ConnectionSecure(C context, ConnectionSecure mystruct) {}
    public virtual void ConnectionSecureOk(C context, ConnectionSecureOk mystruct) {}
    public virtual void ConnectionTune(C context, ConnectionTune mystruct) {}
    public virtual void ConnectionTuneOk(C context, ConnectionTuneOk mystruct) {}
    public virtual void ConnectionOpen(C context, ConnectionOpen mystruct) {}
    public virtual void ConnectionOpenOk(C context, ConnectionOpenOk mystruct) {}
    public virtual void ConnectionRedirect(C context, ConnectionRedirect mystruct) {}
    public virtual void ConnectionHeartbeat(C context, ConnectionHeartbeat mystruct) {}
    public virtual void ConnectionClose(C context, ConnectionClose mystruct) {}
    public virtual void ConnectionCloseOk(C context, ConnectionCloseOk mystruct) {}
    public virtual void SessionAttach(C context, SessionAttach mystruct) {}
    public virtual void SessionAttached(C context, SessionAttached mystruct) {}
    public virtual void SessionDetach(C context, SessionDetach mystruct) {}
    public virtual void SessionDetached(C context, SessionDetached mystruct) {}
    public virtual void SessionRequestTimeout(C context, SessionRequestTimeout mystruct) {}
    public virtual void SessionTimeout(C context, SessionTimeout mystruct) {}
    public virtual void SessionCommandPoint(C context, SessionCommandPoint mystruct) {}
    public virtual void SessionExpected(C context, SessionExpected mystruct) {}
    public virtual void SessionConfirmed(C context, SessionConfirmed mystruct) {}
    public virtual void SessionCompleted(C context, SessionCompleted mystruct) {}
    public virtual void SessionKnownCompleted(C context, SessionKnownCompleted mystruct) {}
    public virtual void SessionFlush(C context, SessionFlush mystruct) {}
    public virtual void SessionGap(C context, SessionGap mystruct) {}
    public virtual void ExecutionSync(C context, ExecutionSync mystruct) {}
    public virtual void ExecutionResult(C context, ExecutionResult mystruct) {}
    public virtual void ExecutionException(C context, ExecutionException mystruct) {}
    public virtual void MessageTransfer(C context, MessageTransfer mystruct) {}
    public virtual void MessageAccept(C context, MessageAccept mystruct) {}
    public virtual void MessageReject(C context, MessageReject mystruct) {}
    public virtual void MessageRelease(C context, MessageRelease mystruct) {}
    public virtual void MessageAcquire(C context, MessageAcquire mystruct) {}
    public virtual void MessageResume(C context, MessageResume mystruct) {}
    public virtual void MessageSubscribe(C context, MessageSubscribe mystruct) {}
    public virtual void MessageCancel(C context, MessageCancel mystruct) {}
    public virtual void MessageSetFlowMode(C context, MessageSetFlowMode mystruct) {}
    public virtual void MessageFlow(C context, MessageFlow mystruct) {}
    public virtual void MessageFlush(C context, MessageFlush mystruct) {}
    public virtual void MessageStop(C context, MessageStop mystruct) {}
    public virtual void TxSelect(C context, TxSelect mystruct) {}
    public virtual void TxCommit(C context, TxCommit mystruct) {}
    public virtual void TxRollback(C context, TxRollback mystruct) {}
    public virtual void DtxSelect(C context, DtxSelect mystruct) {}
    public virtual void DtxStart(C context, DtxStart mystruct) {}
    public virtual void DtxEnd(C context, DtxEnd mystruct) {}
    public virtual void DtxCommit(C context, DtxCommit mystruct) {}
    public virtual void DtxForget(C context, DtxForget mystruct) {}
    public virtual void DtxGetTimeout(C context, DtxGetTimeout mystruct) {}
    public virtual void DtxPrepare(C context, DtxPrepare mystruct) {}
    public virtual void DtxRecover(C context, DtxRecover mystruct) {}
    public virtual void DtxRollback(C context, DtxRollback mystruct) {}
    public virtual void DtxSetTimeout(C context, DtxSetTimeout mystruct) {}
    public virtual void ExchangeDeclare(C context, ExchangeDeclare mystruct) {}
    public virtual void ExchangeDelete(C context, ExchangeDelete mystruct) {}
    public virtual void ExchangeQuery(C context, ExchangeQuery mystruct) {}
    public virtual void ExchangeBind(C context, ExchangeBind mystruct) {}
    public virtual void ExchangeUnbind(C context, ExchangeUnbind mystruct) {}
    public virtual void ExchangeBound(C context, ExchangeBound mystruct) {}
    public virtual void QueueDeclare(C context, QueueDeclare mystruct) {}
    public virtual void QueueDelete(C context, QueueDelete mystruct) {}
    public virtual void QueuePurge(C context, QueuePurge mystruct) {}
    public virtual void QueueQuery(C context, QueueQuery mystruct) {}

}
}
