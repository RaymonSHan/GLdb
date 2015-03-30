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
 * return -1 for NOT socket, return > 0 is listening, return 0 is not.
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
  (void)    lpProtocolInfo;
  (void)    g;
  PCONT     pcont;
  int       handle;
  RESULT    result;

/*
 * for the different for AcceptEx and accept, do NOT create socket here
 */
#ifdef    __GLdb_SELF_USE
  if (dwFlags & (WSA_FLAG_ISACCEPT | WSA_FLAG_ISCONNECT)) pcont->bHandle = 0;
  else handle = socket(af, type, protocol);
#else  // __GLdb_SELF_USE
  handle = socket(af, type, protocol);
#endif // __GLdb_SELF_USE

  if (handle == NEGONE) {
    WSAERROR
    return INVALID_SOCKET;
  }
  result = GetContext(pcont);
  if (result) return INVALID_SOCKET;

  pcont->bHandle = handle;
  pcont->dwFlags = dwFlags;
  return pcont;
};

/*
 * Same as Windows version. three parameter is 0 for create new handle, or
 *   join the given FileHandle join ExistingCompletionPOrt with COmpetionKey.
 *
 * CreateIoCompletionPort() use GetIOCPItem() to get handle, 64 at most.
 * All FileHandle add EPOLLIN into same epollHandle. with each IOCP handle.
 *
 * for Connect socket, wait for writeable first, then wait EPOLLIN
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
    if (GlobalIOCP.GetIOCPItem(addr)) {
    __RETURN_(0);
    }
  __RETURN_(addr.aLong);
  }
  __DOe(FileHandle == 0,  GL_IOCP_INPUT_ZERO);
  __DOe(ExistingCompletionPort == 0, GL_IOCP_INPUT_ZERO);
  __DOe(CompletionKey == 0, GL_IOCP_INPUT_ZERO);
/*
 * the first thing CreateIoCompletionPort do is associte FileHandle with 
 *   CompletionPort & CompetionKey.
 * the second thing is get event when it happen
 */
  FileHandle->iocpHandle = (PEVENT)ExistingCompletionPort;
  FileHandle->completionKey = CompletionKey;
  if (IS_CONNECT(FileHandle)) {
/*
 * NO EPOLLIN at first for PostConnect
 * epoll return Context value, which store CompletionKey.
 */
    ev.events = EPOLLET | EPOLLOUT | EPOLLRDHUP;
    FileHandle->waitEpollOut = 1;
  } else {
    ev.events = EPOLLET | EPOLLIN | EPOLLRDHUP;
    FileHandle->waitEpollOut = 0;
  }
  ev.data.ptr = FileHandle;
  state = epoll_ctl(GlobalIOCP.epollHandle,
	    EPOLL_CTL_ADD, 
	    FileHandle->bHandle, 
	    &ev);
  if (state) {
    WSAERROR
  __RETURN_(0);
  }
__CATCH_(ExistingCompletionPort)
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
	    POLAP           *lpOverlapped,
	    DWORD           dwMilliseconds)
{
  (void)    dwMilliseconds;
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;
  PSIGN     psign;

  __DOe(CompletionPort == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpNumberOfBytes == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpCompletionKey == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  iocpHandle = (PEVENT)CompletionPort;
  __DO (*iocpHandle -= addr);

  psign = addr.pSign;
  addr = psign->sContext;
  if (addr.aLong & MAX_64BIT) {
    addr &= ~MAX_64BIT;
    psign->sContext = 0;
    *lpCompletionKey = addr.pLong;
  } else {
    if (psign->sContext) *lpCompletionKey = psign->sContext->completionKey;
    else *lpCompletionKey = 0;
  }
  *lpOverlapped = psign->sOverlap;
  *lpNumberOfBytes = psign->sSize;
  __DO (FreeSign(psign));
__CATCH_(1)
};

/*
 * NULL lpOverlapped is valid,
 * lpOverlapped with valid Internal is valid.
 * lpOverlapped with 0 Internal is valid.
 */
BOOL        PostQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    DWORD           dwNumberOfBytesTransferred,
	    ULONG_PTR       dwCompletionKey,
	    POLAP           lpOverlapped)
{
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;
  PSIGN     psign;

  __DOe(CompletionPort == 0, GL_IOCP_INPUT_ZERO);
  __DOe(dwCompletionKey == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  iocpHandle = (PEVENT)CompletionPort;
  __DO (GetSign(psign));
  psign->sOverlap = lpOverlapped;
  psign->sSize = dwNumberOfBytesTransferred;
  if (lpOverlapped) {
    psign->sContext = lpOverlapped->Internal;
  } else {
    psign->sContext = 0;
  }
  if (psign->sContext) {
    psign->sContext->completionKey = dwCompletionKey;
  } else {
/*
 * translate CompletionKey without Context, set Highest bit to 1
 */
    addr.pVoid = dwCompletionKey;
    addr |= MAX_64BIT;
    psign->sContext = addr.pCont;
  }

  addr = psign;
  __DO (*iocpHandle += addr);
__CATCH_(1)
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
	    POLAP           lpOverlapped, 
	    POLAPCR         lpCompletionRoutine)
{
  (void)    dwFlags;
  (void)    lpCompletionRoutine;
__TRY__
  ADDR      addr, overlap;
  PSIGN     psign;

  __DOe(s == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpBuffers == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpNumberOfBytesSent == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);
  __DOe(dwBufferCount != 1, GL_IOCP_INPUT_NOSUP);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = s;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLWRITE;
  psign->sSize = 0;

  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
/*
 * now i set lpNumberOfBytesSent now, maybe should set after send
 */
  *lpNumberOfBytesSent = lpBuffers->len;

  overlap.pVoid = lpOverlapped;
  __DO (s->writeBuffer += overlap);
#ifdef    __DEBUG_EVENT
  D(WSASend);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (*(GlobalIOCP.eventHandle) += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(SOCKET_ERROR)
};

int         WSARecv(
	    SOCKET          s,
	    LPWSABUF        lpBuffers,
	    DWORD           dwBufferCount,
	    PUINT           lpNumberOfBytesRecvd,
	    LPDWORD         lpFlags,
	    POLAP           lpOverlapped,
	    POLAPCR         lpCompletionRoutine)
{
  (void)    lpFlags;
  (void)    lpCompletionRoutine;
__TRY__
  ADDR      addr;
  PSIGN     psign;

  __DOe(s == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpBuffers == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpNumberOfBytesRecvd == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);
  __DOe(dwBufferCount != 1, GL_IOCP_INPUT_NOSUP);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = s;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLREAD;
  psign->sSize = 0;

  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
  *lpNumberOfBytesRecvd = lpBuffers->len;
#ifdef    __DEBUG_EVENT
  D(WSARecv);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (*(GlobalIOCP.eventHandle) += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(SOCKET_ERROR)
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
	    POLAP           lpOverlapped)
{
  (void)    dwReceiveDataLength;
  (void)    dwLocalAddressLength;
  (void)    dwRemoteAddressLength;
  (void)    lpdwBytesReceived;
__TRY__
  ADDR      addr;
  PSIGN     psign;

  __DOe(sListenSocket == 0, GL_IOCP_INPUT_ZERO);
  __DOe(sAcceptSocket == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOutputBuffer == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = sListenSocket;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLACCEPT;
  psign->sSize = 0;

  lpOverlapped->Internal = sListenSocket;
  lpOverlapped->InternalHigh = (PWSABUF)lpOutputBuffer;
  lpOverlapped->accSocket = sAcceptSocket;
#ifdef    __DEBUG_EVENT
  D(AcceptEx);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (*(GlobalIOCP.eventHandle) += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(1)
};

/*
 * ConnectEx of GLdbIOCP version will not send any data fromto socket. It behavior
 *   as dwSendDataLength = 0 in Windows.
 */
BOOL        ConnectEx(
            SOCKET          s,
	    PSOCK           name,
	    int             namelen,
	    PVOID           lpSendBuffer,
	    DWORD           dwSendDataLength,
	    LPDWORD         lpdwBytesSent,
	    POLAP           lpOverlapped)
{
  (void)    dwSendDataLength;
  (void)    lpdwBytesSent;
__TRY__
  ADDR      addr;
  PSIGN     psign;

  __DOe(s == 0, GL_IOCP_INPUT_ZERO);
  __DOe(name == 0, GL_IOCP_INPUT_ZERO);
  __DOe(namelen == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpSendBuffer == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = s;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLCONNECT;
  psign->sSize = 0;

  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = (PWSABUF)lpSendBuffer;
  /*
   * should change following line in future
   */
  memcpy(&(s->remoteSocket), name, namelen);

#ifdef    __DEBUG_EVENT
  D(ConnectEx);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (*GlobalIOCP.eventHandle += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(1)
};

FILEHANDLE  CreateFile(
            LPCTSTR         lpFileName,
            DWORD           dwDesiredAccess,
            DWORD           dwShareMode,
            LPSECURITY_ATTRIBUTES lpSecurityAttributes,
            DWORD           dwCreationDisposition,
            DWORD           dwFlagsAndAttributes,
            HANDLE          hTemplateFile)
{
  (void)    dwShareMode;                        // always FILE_SHARE_READ
  (void)    lpSecurityAttributes;
  (void)    hTemplateFile;
__TRY__
  ADDR      addr;
  PSIGN     psign = 0;
  PCONT     pcont = 0;
  PBUFF     pbuff = 0;
  POLAP     polap = 0;
  PWSABUF   pwsa = 0;

  __DOe(lpFileName == 0, GL_IOCP_INPUT_ZERO);
  __DOe(dwFlagsAndAttributes != FILE_FLAG_OVERLAPPED, GL_IOCP_INPUT_NOSUP);

  __DO (GetContext(pcont));
  __DO (GetBufferSmall(pbuff));
  __DO (GetSign(psign));

  polap = &(pbuff->oLapped);
  pwsa = &(pbuff->wsaBuf);

  polap->Internal = pcont;
  polap->InternalHigh = pwsa;

  psign->sContext = pcont;
  psign->sOverlap = polap;
  psign->sEvent = EPOLLFILEOPEN;
  psign->sSize = 0;
  psign->dwAccess = dwDesiredAccess;
  psign->dwCreation = dwCreationDisposition;
  //  pcont->localFilename = lpFileName;

  pwsa->len = strlen((char*)lpFileName);
  memcpy (pwsa->buf, lpFileName, pwsa->len);
#ifdef    __DEBUG_EVENT
  D(CreateFile);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  addr = psign;
  __DO (EventFile += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(FILE_ERROR)
};

BOOL        CloseHandle(
            FILEHANDLE      hObject)
{
__TRY__
  ADDR      addr;
  PSIGN     psign = 0;

  __DOe(hObject == 0, GL_IOCP_INPUT_ZERO);

  __DO (GetSign(psign));

  psign->sContext = hObject;
  psign->sOverlap = 0;
  psign->sEvent = EPOLLFILECLOSE;
  psign->sSize = 0;

#ifdef    __DEBUG_EVENT
  D(CloseHandle);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  addr = psign;
  __DO (EventFile += addr);
  WSASetLastError(WSA_IO_PENDING);
  //MARK_ERROR_CLOSE
__CATCH_(0)
};

BOOL        ReadFile(
            FILEHANDLE      hFile,
            LPVOID          lpBuffer,
            DWORD           nNumberOfBytesToRead,
            LPDWORD         lpNumberOfBytesRead,
            POLAP           lpOverlapped)
{
__TRY__
  ADDR      addr;
  PSIGN     psign = 0;
  PBUFF     pbuff = (PBUFF)lpOverlapped;
  PWSABUF   wsabuff;

  __DOe(hFile == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpBuffer == 0, GL_IOCP_INPUT_ZERO);
  //  __DOe(nNumberOfBytesToRead == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpNumberOfBytesRead == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  wsabuff = &(pbuff->wsaBuf);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = hFile;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLFILEREAD;
  psign->sSize = 0;

  lpOverlapped->Internal = hFile;
  lpOverlapped->InternalHigh = wsabuff;

#ifndef   __GLdb_SELF_USE
  wsabuff->buf = lpBuffer;
  wsabuff->len = nNumberOfBytesToRead;
#endif  //__GLdb_SELF_USE
  *lpNumberOfBytesRead = nNumberOfBytesToRead;

#ifdef    __DEBUG_EVENT
  D(ReadFile);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (EventFile += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(0)
};

BOOL        WriteFile(
            FILEHANDLE      hFile,
            LPVOID          lpBuffer,
            DWORD           nNumberOfBytesToWrite,
            LPDWORD         lpNumberOfBytesWritten,
            POLAP           lpOverlapped)
{
__TRY__
  ADDR      addr, overlap;
  PSIGN     psign = 0;
  PBUFF     pbuff = (PBUFF)lpOverlapped;
  PWSABUF   wsabuff;

  __DOe(hFile == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpBuffer == 0, GL_IOCP_INPUT_ZERO);
  //  __DOe(nNumberOfBytesToWrite == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpNumberOfBytesWritten == 0, GL_IOCP_INPUT_ZERO);
  __DOe(lpOverlapped == 0, GL_IOCP_INPUT_ZERO);

  wsabuff = &(pbuff->wsaBuf);

  __DO (GetSign(psign));
  addr = psign;
  psign->sContext = hFile;
  psign->sOverlap = lpOverlapped;
  psign->sEvent = EPOLLFILEWRITE;
  psign->sSize = 0;

  lpOverlapped->Internal = hFile;
  lpOverlapped->InternalHigh = wsabuff;

#ifndef   __GLdb_SELF_USE
  wsabuff->buf = lpBuffer;
  wsabuff->len = nNumberOfBytesToWrite;
#endif  //__GLdb_SELF_USE
  *lpNumberOfBytesWritten = nNumberOfBytesToWrite;

/*
 * all write buffer is cached in writeBuffer
 */
  overlap.pVoid = lpOverlapped;
  __DO (hFile->writeBuffer += overlap);
#ifdef    __DEBUG_EVENT
  D(WriteFile);DSIGN(psign);Dn;
#endif // __DEBUG_EVENT
  __DO (EventFile += addr);
  WSASetLastError(WSA_IO_PENDING);
__CATCH_(0)
};

UINT        WSAGetLastError(void)
{
  PTINFO    ptinfo;
  getTraceInfo(ptinfo);
  return (ptinfo->GLError);
};

void        WSASetLastError(UINT err)
{
  PTINFO    ptinfo;
  PUINT     pint;
  getTraceInfo(ptinfo);
  pint = (PUINT)&(ptinfo->GLError);
  *pint = err;
};

RESULT      RThreadEpoll::ThreadInit(void)
{
__TRY
  __DO (GlobalMemory.InitThreadMemory(1));
  __DO1(epollHandle, epoll_create(1));
__CATCH
};

RESULT      RThreadEpoll::ThreadDoing(void)
{
__TRY
  int       evNumber, i;
  ADDR      addr;;
  PSIGN     psign;

  __DO1(evNumber,
	    epoll_wait(epollHandle, waitEv, NUMBER_MAX_EV, TimeoutEpollWait));
  if (evNumber == 0) {
    __DO (GetSign(psign));
    psign->sContext = 0;
    psign->sOverlap = 0;
    psign->sEvent = EPOLLTIMEOUT;
    psign->sSize = 0;
    addr.pSign = psign;
    __DO (*peventHandle += addr);
  } else {
    for (i = 0; i < evNumber; i++) {
/*
 * the buffer for overlap is small, send sign one by one, not group.
 * continuous add sign do not switch thread.
 * InternalHigh set to 0, means this sign from RThreadEpoll
 */
      __DO (GetSign(psign));
      psign->sContext = (PCONT)waitEv[i].data.u64;
      psign->sOverlap = 0;
      psign->sEvent = waitEv[i].events;
      psign->sSize = 0;
      addr = psign;
      __DO (*peventHandle += addr);
#ifdef    __DEBUG_EPOLL
      D(RThreadEpoll);DSIGN(psign);
#endif // __DEBUG_EPOLL
    }
  }
__CATCH
};

RESULT      RThreadEvent::ThreadInit(void)
{
__TRY
  __DO (GlobalMemory.InitThreadMemory(1));
  __DO (eventHandle.InitArrayEvent());
__CATCH
};

RESULT      RThreadEvent::ThreadDoing(void)
{
__TRY
  int       readed, writed, state;
  ADDR      signaddr, newaddr, olapaddr, contaddr, buffaddr;
  PSIGN     &psign = (PSIGN &)signaddr;
  PSIGN     &newsign = (PSIGN &)newaddr;
  POLAP     &polap = (POLAP &)olapaddr;
  PCONT     &pcont = (PCONT &)contaddr;
  PWSABUF   &pbuff = (PWSABUF &)buffaddr;
  PCONT     clicont;

  struct    epoll_event ev;
  UINT      tempevent;
  socklen_t tempsize = sizeof(SOCKADDR);
  RESULT    result;

/*
 * Wait eventfd, then get CContextItem and WSABuffer address
 */
  __DO (eventHandle -= signaddr);
  pcont = psign->sContext;
  polap = psign->sOverlap;
  if (psign->sEvent & EPOLLTIMEOUT) {
    // set some
    __DO (FreeSign(psign));
    __BREAK_OK;
  }

  __DO (pcont == 0);
  if (psign->sEvent & (EPOLLERR | EPOLLHUP | EPOLLRDHUP)) {
    psign->sEvent &= ((~EPOLLERR) & (~EPOLLHUP) & (~EPOLLRDHUP));
    if (psign->sEvent == 0) {
      psign->sSize = MARK_ERROR_CLOSE;
      __DO (*pcont->iocpHandle += signaddr);
      __BREAK_OK;
    }
  }

  if (psign->sEvent & EPOLLIN) {
/*
 * Get OVERLAPPED from readBuffer. If no OVERLAPPED in readBuffer, finished.
 */
    result = pcont->readBuffer.TryAndGet(olapaddr);
    if (result) {
      __DO (FreeSign(psign));
      __BREAK_OK;
    }
    if (IS_LISTEN(pcont)) {
      tempevent = EPOLLACCEPT;
    } else {
      tempevent = EPOLLREAD;
    }
    psign->sOverlap = polap;
    psign->sEvent |= tempevent;
  }

  if (psign->sEvent & EPOLLOUT) {
    psign->sEvent |= EPOLLWRITE;
  }

  if (psign->sEvent & EPOLLACCEPT) {
/*
 * do half work of AcceptEx, only return accept SOCKET, but not receive first packet
 *
 * Here overlap->accSocket is SOCKET for accept, if it is not 0, means this SOCKET
 *   is creted without WSA_FLAG_ISACCEPT, it should be closed, and replaced by 
 *   really accept SOCKET.
 * Here context is listening SOCKET.
 */
    __DOe(psign->sOverlap == 0, GL_IOCP_INPUT_ZERO);
    psign->sSize = 0;
    clicont = (PCONT)polap->accSocket;
    if (clicont->bHandle && clicont->bHandle != NEGONE) {
      close(clicont->bHandle);
    }
    clicont->bHandle = accept4(
	    pcont->bHandle, &(pcont->localSocket.saddr), 
	    &tempsize, SOCK_NONBLOCK);
    if (clicont->bHandle && clicont->bHandle != NEGONE) {       // accept OK
      __DO (*pcont->iocpHandle += signaddr);
      __BREAK_OK;                                               //(1)
    } else if (clicont->bHandle == NEGONE && errno == EAGAIN) { // no accept OK
      __DO (pcont->readBuffer += olapaddr);
      __DO (FreeSign(psign));
      __BREAK_OK;                                               //(2)
    } else {                                                    // error
      psign->sSize = MARK_ERROR_CLOSE;
      __DO (*pcont->iocpHandle += signaddr);
      WSAERROR
      __BREAK_OK;                                               //(3)
    }
  }

  if (psign->sEvent & EPOLLREAD) {
    __DOe(psign->sOverlap == 0, GL_IOCP_INPUT_ZERO);

    buffaddr = polap->InternalHigh;
    __DOe(buffaddr == ZERO, GL_IOCP_INPUT_ZERO);

    readed = read(pcont->bHandle, pbuff->buf, pbuff->len);
    if (readed != NEGONE) {                                     // read ok
      psign->sSize = readed;
      __DO (*pcont->iocpHandle += signaddr);
      __BREAK_OK;                                               //(4)
    } else if (errno == EAGAIN) {                               // no read ok
      __DO (pcont->readBuffer += olapaddr);
      __DO (FreeSign(psign));
      __BREAK_OK;                                               //(5)
    } else {                                                    // error
      psign->sSize = MARK_ERROR_CLOSE;
      __DO (*pcont->iocpHandle += signaddr);
      WSAERROR
      __BREAK_OK;                                               //(6)
    }
  }

  if (psign->sEvent & EPOLLWRITE) {
/*
 * this loop will be break in two condition.
 * 1: writeBuffer is empty, then remove EPOLLOUT from epoll if necessary
 * 2: errno == EAGAIN, then add EPOLLOUT to epoll if necessary.
 */
    while (true) {
      setLine();
      writed = 0;
      pcont->writeBuffer.TryGet(olapaddr);
      psign->sOverlap = polap;

      if (IS_CONNECT(pcont)) {
      	pcont->dwFlags &= ~WSA_FLAG_ISCONNECT;
      }

      if (polap == ZERO) {
	if (!pcont->waitEpollOut) {
	  FreeSign(psign);
	  __BREAK_OK;
	}
	ev.events = EPOLLET | EPOLLIN | EPOLLRDHUP;
	ev.data.u64 = contaddr.aLong;
	__DO1(state,
	    epoll_ctl(epollHandle, EPOLL_CTL_MOD, 
	    pcont->bHandle, &ev));
	pcont->waitEpollOut = 0;
	__DO (FreeSign(psign));
	__BREAK_OK;                                             //(7)zero ok
      }
      buffaddr = polap->InternalHigh;
      __DOe(pbuff == 0, GL_IOCP_INPUT_ZERO);
      if (pbuff->len == 0) {
	writed = 0;
      } else {
	writed = write(pcont->bHandle, pbuff->buf, pbuff->len);
      }
      if (writed != NEGONE) {                                   // write OK
	__DO (pcont->writeBuffer -= olapaddr);
	__DO (GetSign(newsign));
	newsign->sContext = pcont;
	newsign->sOverlap = polap;
	newsign->sEvent = psign->sEvent;
	newsign->sSize = writed;
	if (*pcont->iocpHandle += newaddr) {
	  __DO (FreeSign(newsign));
	  __BREAK;
	}
	else {
	  continue;
	}
      } else if (errno == EAGAIN) {                             // write EAGAIN
	if (pcont->waitEpollOut) {
	  __BREAK_OK;
	}
	ev.events = EPOLLET | EPOLLOUT | EPOLLRDHUP;
	ev.data.u64 = contaddr.aLong;
	__DO1(state,
	    epoll_ctl(epollHandle, EPOLL_CTL_MOD, 
	    pcont->bHandle, &ev));
	pcont->waitEpollOut = 1;
	__DO (FreeSign(psign));
	__BREAK;
	break;                                                  //(8)EAGAIN ok
      } else {                                                  // error
	psign->sSize = MARK_ERROR_CLOSE;
	__DO (*pcont->iocpHandle += signaddr);
	WSAERROR
	__BREAK_OK;                                             //(9)write error
      } 
    }
  }

  if (psign->sEvent & EPOLLCONNECT) {
    state = connect(pcont->bHandle,
	    &(pcont->remoteSocket.saddr), sizeof(SOCK));
    // for linux asyn connect never return immediately, always for EINPROCESS, 
    // while FreeBSD maybe for loopback connect.
    if (state == NEGONE && errno == EINPROGRESS) {
      __DO (pcont->writeBuffer += olapaddr);
      __DO (FreeSign(psign));                   //()
      __BREAK_OK;
    } else {
      psign->sSize = MARK_ERROR_CLOSE;
      __DO (*pcont->iocpHandle += signaddr);
      __BREAK_OK;                                               //(3)
    }
  }
__CATCH_BEGIN
  FreeSign(psign);
__CATCH_END
};


#ifdef    __GLdb_SELF_USE

RESULT      RThreadWork::ThreadInit(void)
{
__TRY
  __DO (GlobalMemory.InitThreadMemory(1));
__CATCH
};

RESULT      RThreadWork::ThreadDoing(void)
{
__TRY
  int       size;
  PCONT     pcont;
  PBUFF     pbuff;
  UINT      noper;
  BOOL      state;

  __DO1(state, GetQueuedCompletionStatus(
            (HANDLE)handleIOCP, (DWORD*)&size, 
            (PULONG_PTR)&pcont, (LPOVERLAPPED*)&pbuff, 
            WSA_INFINITE));

  Dp(pcont); Dp(pbuff); if (pbuff) Dlld(pbuff->nOper); Dd(size); Dn;

  if (size == NEGONE) __BREAK_OK;
  if (pbuff == 0) noper = 0;
  else noper = pbuff->nOper;

  if ((size == MARK_ERROR_CLOSE) || (noper == 0) ||
            ((!size) && (noper != OP_ACCEPT) && (noper != OP_CONNECT))) {
    __DO (NoneAppFunc(fOnClose)(pcont, pbuff, size));
  } else {
    __DO (NoneAppFunc(noper - OP_BASE)(pcont, pbuff, size));
  }
__CATCH
};

#endif  //__GLdb_SELF_USE

RESULT      RThreadFile::ThreadInit(void)
{
__TRY
  __DO (GlobalMemory.InitThreadMemory(0));
__CATCH
};

RESULT      RThreadFile::ThreadDoing(void)
{
__TRY
  int       readed, writed, state = NEGONE;
  ADDR      signaddr, olapaddr, contaddr;
  PSIGN     &psign = (PSIGN &)signaddr;
  POLAP     &polap = (POLAP &)olapaddr;
  PCONT     &pcont = (PCONT &)contaddr;
  PWSABUF   pwsa;

  __DO (EventFile -= signaddr);
  pcont = psign->sContext;
  polap = psign->sOverlap;

  __DO (pcont == 0);
  if (psign->sEvent & EPOLLFILEOPEN) {
    __DO (polap == 0);
    pwsa = polap->InternalHigh;
    __DO (pwsa == 0);
    if (psign->dwAccess == GENERIC_READ) {
      state = open((char*)pwsa->buf, O_RDONLY);
    }
    if (psign->dwAccess == GENERIC_WRITE) {
      state = open((char*)pwsa->buf, O_WRONLY, O_TRUNC | O_CREAT);
    }
    pcont->bHandle = state;
    if (state == NEGONE) {                                      // error open
      WSAERROR
      __BREAK_OK;
    }
    psign->sSize = 0;
    __DO (*pcont->iocpHandle += signaddr);                      // open OK
    __BREAK_OK; 
  }

  if (psign->sEvent & EPOLLFILECLOSE) {
    state = close(pcont->bHandle);
    pcont->bHandle = 0;
    if (state == NEGONE) {                                      // error close
      WSAERROR
      __BREAK_OK;
    }
    __DO (*pcont->iocpHandle += signaddr);                      // close OK
    __BREAK_OK; 
  }

  if (psign->sEvent & EPOLLFILEREAD) {
    __DO (polap == 0);
  }

  if (psign->sEvent & EPOLLFILECLOSE) {
    __DO (pcont->writeBuffer -= olapaddr);
    __DO (olapaddr == ZERO);
    pwsa = polap->InternalHigh;
    __DO (pwsa == 0);
    writed = write(pcont->bHandle, (char*)pwsa->buf, pwsa->len);
    if (state == NEGONE) {                                      // error write
      WSAERROR
      __BREAK_OK;
    }
    psign->sSize = writed;
    __DO (*pcont->iocpHandle += signaddr);                      // write OK
    __DO (polap == 0);
  }
__CATCH
};

GLdbIOCP::GLdbIOCP()
{
  nowWorkThread = nowFileThread = 0;
};

RESULT      GLdbIOCP::InitGLdbIOCP()
{
__TRY
  int       i;
  ADDR      addr;

  for (i = 0; i < NUMBER_MAX_IOCP; i++) {
    addr = (PVOID)&(iocpHandle[i]);
    iocpHandleFree += addr;
  }
  nowWorkThread = 0;
  __DO (threadEpoll.ThreadClone(true));
  __DO (threadEvent.ThreadClone(true));
  __DO (RThread::ThreadStart());

/*
 * YES, this code may be execute before threadEvent initialized.
 * but threadEpoll is initialized surely.
 */
  epollHandle = threadEvent.epollHandle = threadEpoll.epollHandle;
  eventHandle = threadEpoll.peventHandle = &threadEvent.eventHandle;
  //  Dp(&EventFile);

__CATCH
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
  for (i = 0; i < GlobalThreadNumber; i++)  
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

RESULT      GLdbIOCP::StartWork(PEVENT handle, UINT num)
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

RESULT      GLdbIOCP::StartFile(UINT num)
{
  UINT      nowfile = LockAdd(nowFileThread, num);
  UINT      i;
__TRY__
  for (i = nowfile; i < nowfile + num; i++) {
    //    threadFile[i].SetupHandle(&eventFile);
    threadFile[i].ThreadClone(false);
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
