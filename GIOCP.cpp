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
  InitContextItem(pcont);

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
  __DO(*iocpHandle -= addr);
  *lpOverlapped = (LPOVERLAPPED)addr.pVoid;
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
	    LPDWORD         lpNumberOfBytesSent, 
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
	    LPDWORD         lpNumberOfBytesRecvd,
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
  overlap.pVoid = lpOverlapped;
  lpOverlapped->Internal = sListenSocket;
  lpOverlapped->doneSize = (UINT)sAcceptSocket;
  lpOverlapped->events = EPOLLACCEPT;
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH_1
}


/*
 * for demo of GLdbIOCP initialize
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
