/*
 * GLdb thread & IOCP implementy file
 *
 * GLdb is a Multi-thread customed Key-Value No-SQL memory database.
 * GLdb atomic insert voucher & update balance, provide interface for ERP.
 * GLdb have its own Async IO system, support Windows & Linux by IOCP & epoll.
 * GLdb request large memory, so only support 64bit system.
 *
 * Copyright (c) 2015 Raymon SHan <quickhorse77 at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modifica-
 * tion, are permitted provided that the following conditions are met:
 *
 *   1.  Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *   2.  Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MER-
 * CHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTH-
 * ERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include    "GIOCP.hpp"
#include    "GEncapsulate.hpp"

/*
 * for use GLdbIOCP instead of GLdbDatabase, should define this two as GLOBAL var.
 *
 * for GLdbDatabase, already define it in GCommon.hpp
 */
#ifndef   __GLdb_SELF_USE
MEMORY      GlobalMemory;
IOCP        GlobalIOCP;
#endif  //__GLdb_SELF_USE

/*
 * return -1 for NOT socket, return >0 is listening, return 0 is not.
 */
int         isListeningSocket(HANDLE handle)
{
  int       val;
  int       result;
  socklen_t len = sizeof(val);
  result = getsockopt(handle, SOL_SOCKET, SO_ACCEPTCONN, &val, &len);
  if (result == -1) return result;
  else return val;
};

/*
 * For the following compatible function for Windows, 
 *   should remember some type convert. All have written in GCommon.hpp, but again.
 *
 * The most important is SOCKET and HANDLE.
 *
 * SOCKET : as a pointer of CContextItem. 64bits
 * HANDLE : as unsigned long long int. 64bits
 */

/*
 * In Windows, a better way to get SOCKET for IOCP is WSASocket. in GLdbIOCP, 
 *   it is necessarily.
 *
 * dwFlags must with OVERLAP flags, although i ignore it.
 */
SOCKET      WSASocket(
            int             af, 
	    int             type, 
	    int             protocol,
	    LPWSAPROTOCOL_INFO  lpProtocolInfo, 
	    GROUP           g, 
	    DWORD           dwFlags)
{
  (void)     lpProtocolInfo;
  (void)     g;
  PCONT      pcont;
  RESULT     result;

  result = GetContext(pcont);
  if (result) return 0;

/*
 * for the different for AcceptEx and accept, do NOT create socket here
 */
#ifdef    __GLdb_SELF_USE
  if (dwFlags & WSA_FLAG_ISACCEPT) pcont->bHandle = 0;
  else pcont->bHandle = socket(af, type, protocol);
#else  // __GLdb_SELF_USE
  pcont->bHandle = socket(af, type, protocol);
#endif // __GLdb_SELF_USE

  pcont->dwFlags = dwFlags;
  return pcont;
};

/*
 * Same as Windows version. three parameter is 0 for create new handle, or
 *   join the given FileHandle join ExistingCompletionPOrt with COmpetionKey.
 *
 * CreateIoCompletionPort() use GetIOCPItem() to get handle, 64 at most.
 * All FileHandle add EPOLLIN into same epollHandle. with each IOCP handle.
 */
HANDLE      CreateIoCompletionPort(
            SOCKET          FileHandle,
	    HANDLE          ExistingCompletionPort,
	    ULONG_PTR       CompletionKey,
	    DWORD           NumberOfConcurrentThreads)
{
  (void)    NumberOfConcurrentThreads;
__TRY__
  ADDR      addr;
  int       state;
  struct    epoll_event ev;
  if (!FileHandle && !ExistingCompletionPort && !CompletionKey) {
    __DO_(GlobalIOCP.GetIOCPItem(addr), "Errorr in Create IOCP handle\n");
    return addr.aLong;
  } else {
    __DO (!ExistingCompletionPort);
    __DO (!FileHandle);
    FileHandle->iocpHandle = (PEVENT)ExistingCompletionPort;
    FileHandle->completionKey = CompletionKey;
    ev.events = EPOLLET | EPOLLIN;
    ev.data.ptr = CompletionKey;
    __DO1 (state,
	   epoll_ctl(GlobalIOCP.epollHandle,
		     EPOLL_CTL_ADD, 
		     FileHandle->bHandle, 
		     &ev));
    return ExistingCompletionPort;
  }
__CATCH_1
};

/*
 * Wait for eventfd do NOT timeout option. Maybe I should change to another way.
 * So periodicial trigger is given by another thread.
 *
 * HAVE NOT IMPLEMENT YET. IGNORE THE OPTION NOW, ALL ARE INFINITE.
 *
 * Other things are simple, it wait for eventfd as IOCP hanle, and translate
 *   struct member as Windows define.
 */
BOOL        GetQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    LPDWORD         lpNumberOfBytes,
	    PULONG_PTR      lpCompletionKey,
	    LPOVERLAPPED   *lpOverlapped,
	    DWORD           dwMilliseconds)
{
  (void)dwMilliseconds;
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;

  iocpHandle = (PEVENT)CompletionPort;
  DD("GetQueryCompletionStatus wait : %p\n", iocpHandle);
  __DO(*iocpHandle -= addr);
  D(AfterGetQueryCompletionStatus);
  *lpOverlapped = (POLAP)addr.pVoid;
  if (addr != ZERO) {
    (*lpCompletionKey) = (*lpOverlapped)->Internal->completionKey;
    (*lpNumberOfBytes) = (*lpOverlapped)->doneSize;
  }
__CATCH_1
};

/*
 * in normal IOCP, lpOverlapped can be NULL.
 * but in GLdbIOCP, must use valid OVERLAPPED struct for lpOverlapped
 */
BOOL        PostQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    DWORD           dwNumberOfBytesTransferred,
	    ULONG_PTR       dwCompletionKey,
	    LPOVERLAPPED    lpOverlapped)
{
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;
  iocpHandle = (PEVENT)CompletionPort;
  addr.pVoid = lpOverlapped;
  lpOverlapped->Internal->completionKey = dwCompletionKey;
  lpOverlapped->doneSize = dwNumberOfBytesTransferred;
  __DO (*iocpHandle += addr);
__CATCH_1
};

/*
 * WSASend & WSARecv do almost same thing, the difference is events value,
 *   one for EPOLLWRITE and other for EPOLLREAD.
 * The only thing should attention is WSASend add writeBuffer, while WSARecv not add
 *   readBuffer.
 */
int         WSASend(
	    SOCKET          s, 
	    LPWSABUF        lpBuffers, 
	    DWORD           dwBufferCount, 
	    PUINT           lpNumberOfBytesSent, 
	    DWORD           dwFlags, 
	    LPWSAOVERLAPPED lpOverlapped, 
	    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine)
{
  (void)dwFlags;
  (void)lpCompletionRoutine;
__TRY
  ADDR      overlap;
  __DO_(dwBufferCount != 1, "GLdbIOCP only support one WSABUF now\n");
  __DO_(lpOverlapped == NULL, "GLdbIOCP not support NULL OVERLAPPED\n");
  overlap.pVoid = lpOverlapped;
  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
  lpOverlapped->events = EPOLLWRITE;
  lpOverlapped->doneSize = 0;
  *lpNumberOfBytesSent = lpBuffers->len;
  __DO (s->writeBuffer += overlap);
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH
};

int         WSARecv(
	    SOCKET          s,
	    LPWSABUF        lpBuffers,
	    DWORD           dwBufferCount,
	    PUINT           lpNumberOfBytesRecvd,
	    LPDWORD         lpFlags,
	    LPWSAOVERLAPPED lpOverlapped,
	    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine)
{
  (void)lpFlags;
  (void)lpCompletionRoutine;
__TRY
  ADDR      overlap;
  __DO_(dwBufferCount != 1, "GLdbIOCP only support one WSABUF now\n");
  __DO_(lpOverlapped == NULL, "GLdbIOCP not support NULL OVERLAPPED\n");
  overlap.pVoid = lpOverlapped;
  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
  lpOverlapped->events = EPOLLREAD;
  lpOverlapped->doneSize = 0;
  *lpNumberOfBytesRecvd = lpBuffers->len;
/*__DO (s->readBuffer += overlap);*/
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH
};

/*
 * AccetpEx of GLdbIOCP version will not received any data from socket. It behavior
 *   as dwReceiveDataLength = 0 in Windows.
 */
BOOL        AcceptEx(
            SOCKET          sListenSocket,
	    SOCKET          sAcceptSocket,
	    PVOID           lpOutputBuffer,
	    DWORD           dwReceiveDataLength,
	    DWORD           dwLocalAddressLength,
	    DWORD           dwRemoteAddressLength,
	    LPDWORD         lpdwBytesReceived,
	    LPOVERLAPPED    lpOverlapped)
{
  (void)lpOutputBuffer;
  (void)dwReceiveDataLength;
  (void)dwLocalAddressLength;
  (void)dwRemoteAddressLength;
  (void)lpdwBytesReceived;
__TRY__
  ADDR      overlap;
 D(inAcceptEx);
  lpOverlapped->Internal = sListenSocket;
  lpOverlapped->accSocket = sAcceptSocket;
  lpOverlapped->events = EPOLLACCEPT;
  overlap.pVoid = lpOverlapped;
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH_1
}

RESULT      RThreadEpoll::ThreadInit(void)
{
__TRY
  ADDR      addr;
    
  GlobalMemory.InitThreadMemory(1);
  addr = (PVOID)overlapBuffer;
  __DO1_(epollHandle, epoll_create(1), "Error in create epoll\n");
  overlapStack.FullArrayStack(addr, sizeof(OVERLAPPED), LIST_SMALL);
__CATCH
};

RESULT      RThreadEpoll::ThreadDoing(void)
{
__TRY
  int       evNumber, i;
  ADDR      overlapaddr;
  POLAP     &overlap = (POLAP &)overlapaddr;

  __DO1 (evNumber,
	 epoll_wait(epollHandle, waitEv, NUMBER_MAX_EV, TimeoutEpollWait));
  if (waitEv == 0) {
    __DO (overlapStack -= overlapaddr);
    overlap->events = EPOLLTIMEOUT;
    __DO (*peventHandle += overlapaddr);
  } else {
    for (i = 0; i < evNumber; i++) {
/*
 * the buffer for overlap is small, send sign one by one, not group.
 * continuous add sign do not switch thread.
 * InternalHigh set to 0, means this sign from RThreadEpoll
 */
      __DO (overlapStack -= overlapaddr);
      overlap->Internal = (PCONT)waitEv[i].data.u64;
      overlap->events = waitEv[i].events;
      overlap->InternalHigh = 0;
      __DO (*peventHandle += overlapaddr);
    }
  }
__CATCH
};

RESULT      RThreadEvent::ThreadInit(void)
{
__TRY
  GlobalMemory.InitThreadMemory(1);
  __DO_(eventHandle.InitArrayEvent(), "Error in create eventEv\n");
__CATCH
};

RESULT      RThreadEvent::ThreadDoing(void)
{
__TRY
  int     readed, writed, state;
  ADDR    contextaddr, overlapaddr, bufferaddr, listenaddr;
  PCONT   &context = (PCONT &)contextaddr;
  PCONT   clicont;
  POLAP   &overlap = (POLAP &)overlapaddr;
  PWSABUF &buffer = (PWSABUF &)bufferaddr; 
  struct  epoll_event ev;
  UINT    tempevent = EPOLLREAD;
  socklen_t tempsize = sizeof(SOCKADDR);

/*
 * Wait eventfd, then get CContextItem and WSABuffer address
 */
  __DO (eventHandle -= overlapaddr);
  contextaddr = overlap->Internal;

  if (overlap->events == EPOLLIN) {
/*
 * Free the sign OVERLAPPED from RThreadEpoll and get really OVERLAPPED with 
 *   buffer from readBuffer. If no OVERLAPPED in readBuffer, finished.
 */
    *pOverlapStack += overlapaddr;
    //    overlapaddr = ZERO;
    context->readBuffer -= overlapaddr;

#ifdef    __GLdb_SELF_USE
/*
 * For listening SOCKET will NOT use writeBuffer, it is different than other
 *   ReadBuffer for EPOLLACCEPT is accSocket query wait for accept.
 *   WriteBuffer for EPOLLACCEPT is incoming accept query
 */
    if (IS_LISTEN(context)) {
      D(ISLISTEN);
      tempevent = EPOLLACCEPT;
      __DO(context->writeBuffer += contextaddr);
    }
#endif // __GLdb_SELF_USE

    if (overlapaddr == ZERO) __BREAK_OK;
    overlap->events = tempevent;
  }

  if (overlap->events == EPOLLOUT) {
    *pOverlapStack += overlapaddr;
    overlap->events = EPOLLWRITE;             // maybe not necessary
  }

#ifdef    __GLdb_SELF_USE
  if (overlap->events == EPOLLACCEPT) {
/*
 * do half work of AcceptEx, only return accept SOCKET, but not receive first packet
 *
 * Here overlap->accSocket is SOCKET for accept, if it is not 0, means this SOCKET
 *   is creted without WSA_FLAG_ISACCEPT, it should be closed, and replaced by 
 *   really accept SOCKET.
 * Here context is listening SOCKET.
 */

    if (!(context->writeBuffer -= listenaddr)) {
      //    if (listenaddr != ZERO) {
      clicont = (PCONT)overlap->accSocket;
      D(a1);
      if (clicont->bHandle) close(clicont->bHandle);
      D(a2);
      clicont->bHandle = accept(context->bHandle, 
			      &(context->remoteSocket.saddr), &tempsize);
      D(a3);
      DD("add iocp to %p\n", context->iocpHandle);
      __DO (*context->iocpHandle += overlapaddr);

      ADDR pp;
     __DO (*context->iocpHandle -= pp);



      D(a4);
    } else {
      context->readBuffer += overlapaddr;
    }
  }
  else
#endif // __GLdb_SELF_USE

  if (overlap->events == EPOLLREAD) {
    bufferaddr = overlap->InternalHigh;
    overlap->events = EPOLLIN;
    readed = read(context->bHandle, buffer->buf, buffer->len);
    if (readed == NEGONE) {
      if (errno == EAGAIN) {
	context->readBuffer += overlapaddr;
      } else {
	// close socket
      }
    }
    else {
      overlap->doneSize = readed;
      __DO (*context->iocpHandle += overlapaddr);
    } 
  } 
  else if (overlap->events == EPOLLWRITE) {
/*
 * this loop will be break in three condition.
 * 1: writeBuffer is empty, then remove EPOLLOUT from epoll if necessary
 * 2: errno == EAGAIN, then add EPOLLOUT to epoll if necessary.
 * 3: writed + doneSize != len,
 *    This means partly send, do same thing as EAGAIN happen, but is this exist?
 */
    while (true) {
      writed = 0;
      context->writeBuffer.TryGet(overlapaddr);
      if (overlapaddr == ZERO) break;
      if (buffer->len - overlap->doneSize) {
	writed = write(context->bHandle,
		       buffer->buf + overlap->doneSize,
		       buffer->len - overlap->doneSize);
	if (writed == NEGONE) {
	  if (errno == EAGAIN) break;
	  else {
	    // write error close socket
	  } } }
      overlap->doneSize += writed;
      overlap->events = EPOLLOUT;
      if (writed + overlap->doneSize == buffer->len) {
	context->writeBuffer -= overlapaddr;  // must have ??
	if (*context->iocpHandle += overlapaddr) {
	  // IOCP error close socket
	}
      } else break;
    }
    if (overlapaddr == ZERO) {
      if (!context->inEpollOut) __BREAK_OK;
      __DO1 (state,
	     epoll_ctl(epollHandle, 
		       EPOLL_CTL_DEL, 
		       context->bHandle, 
		       &ev));
      context->inEpollOut = 0;
    } else {
      if (context->inEpollOut) __BREAK_OK;
      ev.events = EPOLLET | EPOLLOUT;
      ev.data.u64 = contextaddr.aLong;
      __DO1 (state,
	     epoll_ctl(epollHandle, 
		       EPOLL_CTL_ADD, 
		       context->bHandle, 
		       &ev));
      context->inEpollOut = 1;
    }
  }
  else if (overlap->events == EPOLLTIMEOUT) {
    // set some
    __BREAK_OK;
  }
__CATCH
};


#ifdef    __GLdb_SELF_USE

RESULT      RThreadWork::ThreadInit(void)
{
  GlobalMemory.InitThreadMemory(1);

  return 0;
};

RESULT      RThreadWork::ThreadDoing(void)
{
__TRY
  int       size;
  PCONT     pcont;
  PBUFF     pbuff;
  UINT      noper;

  D(BeginWrokThread);
  GetQueuedCompletionStatus(
            (HANDLE)handleIOCP, (DWORD*)&size, 
            (PULONG_PTR)&pcont, (LPOVERLAPPED*)&pbuff, 
            WSA_INFINITE);

  D(WorkWorkThread);
  if (size == -1) __BREAK_OK;
  noper = pbuff->nOper;

  if (size) NoneAppFunc(&NoneApp, noper - OP_BASE)(pcont, pbuff, size);
  else NoneAppFunc(&NoneApp, fOnClose)(pcont, pbuff, size);
__CATCH
};

#endif  //__GLdb_SELF_USE

RESULT      GLdbIOCP::InitGLdbIOCP()
{
__TRY__
  int       i;
  ADDR      addr;

  for (i=0; i<NUMBER_MAX_IOCP; i++) {
    addr = (PVOID)&(iocpHandle[i]);
    iocpHandleFree += addr;
  }
  nowWorkThread = 0;
  threadEpoll.ThreadClone(true);
  sleep(1);
  threadEvent.ThreadClone(true);
  sleep(1);
  RThread::ThreadStart();

/*
 * YES, this code may be execute before threadEvent initialized.
 * but threadEpoll is initialized surely.
 */
  epollHandle = threadEvent.epollHandle = threadEpoll.epollHandle;
  eventHandle = threadEpoll.peventHandle = &threadEvent.eventHandle;
  pOverlapStack = threadEvent.pOverlapStack = &threadEpoll.overlapStack;
DD("Global epoll:%x, event:%p\n", epollHandle, eventHandle);
  // move this line to encapsulate, for lazy, may change control clone later
  //  RThread::ThreadStart();

__CATCH__
};

RESULT      GLdbIOCP::FreeGLdbIOCP()
{
  ADDR      addr;
  PEVENT    pevent;
  int       status;
  UINT      i;
  while (!(iocpHandleUsed -= addr)) {
    pevent = (PEVENT)(addr.pVoid);
    pevent->FreeArrayEvent();
  }
/*
 * for all worThread and Epoll & Event
 */
  for (i=0; i<nowWorkThread + 2; i++)  
    waitpid(-1, &status, __WCLONE);
  return 0;
};

RESULT      GLdbIOCP::GetIOCPItem(ADDR &addr)
{
__TRY
  PEVENT    pevent;
  __DO (iocpHandleFree -= addr);
  __DO (iocpHandleUsed += addr);
  pevent = (PEVENT)(addr.pVoid);

  __DO (pevent->InitArrayEvent());
__CATCH
};

RESULT      GLdbIOCP::StartWork(HANDLE handle, UINT num)
{
  UINT      nowwork = LockAdd(nowWorkThread, num);
  UINT      i;
__TRY__
  for (i = nowwork; i < nowwork + num; i++) {
    threadWork[i].SetupHandle(handle);
    threadWork[i].ThreadClone(false);
  }
__CATCH__
};

/*
 * for demo of GLdbIOCP initialize
 * NOT USED for my program
 */
#ifndef   __GLdb_SELF_USE

#define     NUMBER_CONTEXT                      5
#define     NUMBER_BUFFER_SMALL                 10
#define     NUMBER_BUFFER_MIDDLE                2

int         main(int, char**)
{
  TIME      rtime(CLOCK_MONOTONIC_RAW);
  struct timespec timestruct;

  SetupSIG(SIGSEGV, SIGSEGV_Handle);                            // sign 11
  SetupSIG(SIGILL, SIGSEGV_Handle);                             // sign 4
  SetupSIG(SIGTERM, SIGSEGV_Handle);                            // sign 15

__TRY__
  GlobalMemory.InitMemoryBlock(NUMBER_CONTEXT,
			       NUMBER_BUFFER_SMALL,
			       NUMBER_BUFFER_MIDDLE);
  GlobalIOCP.InitGLdbIOCP();
  rtime += &timestruct;


  GlobalShouldQuit = 1;
  rtime.OutputTime();
  GlobalIOCP.FreeGLdbIOCP();
  GlobalMemory.FrreeMemoryBlock();
  
__CATCH__
};

#endif  //__GLdb_SELF_USE
