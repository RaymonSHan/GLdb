/*
 * GLdb common header file
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

#ifndef     GLdb_COMMON_HPP
#define     GLdb_COMMON_HPP

#include    <errno.h>
#include    <fcntl.h> 
#include    <sched.h>
#include    <signal.h>
#include    <stdio.h>
#include    <stdlib.h>
#include    <stdarg.h>
#include    <string.h>
#include    <time.h>
#include    <unistd.h>

#include    <arpa/inet.h>
#include    <netinet/in.h>

#include    <sys/epoll.h>
#include    <sys/eventfd.h>
#include    <sys/mman.h>
#include    <sys/param.h>
#include    <sys/socket.h>
#include    <sys/stat.h>
#include    <sys/types.h>
#include    <sys/wait.h>

#include    "GError.hpp"

/*
 * Compiler condition
 * 
 * It add some field into CContextItem struct
 *   and declare CEncapsulate class, with global IOCP & MEMORY
 */
#define   __GLdb_SELF_USE

/*
 * Debug option
 *
 * #define   __DEBUG_CONTEXT
 * #define   __DEBUG_BUFFER
 * #define   __DEBUG_SIGN
 * #define   __DEBUG_EPOLL
 * #define   __DEBUG_EVENT
 * #define   __DEBUG_IOCP
 */


/*
 * In GLdb, money is signed int64, 1 million means 1 dollar, 
 * range is about +/- 9 thousand billion, enough for enterprise use.
 */
#define     ONE_DOLLAR_VALUE                    (1000*1000)
#define     MAX_64BIT                           0x8000000000000000
/*
 * only for short
 */
#define     SIZEADDR                            sizeof(ADDR)
/*
 * compiler ambiguous 0 for (UINT)0 & (ADDR)0
 */
#define     ZERO                                ((UINT)0)
#define     ZEROADDR                            ((ADDR)0)

#define     NEGONE                              (-1)
#define     INVALID_SOCKET                      ((SOCKET)-1)
#define     FILE_ERROR                          ((FILEHANDLE)-1)

/*
 * for STRING use, constant len string
 */
#define     CHAR_SMALL                          ((1<<6)-2*SIZEADDR-1)
#define     CHAR_MIDDLE                         ((1<<9)-2*SIZEADDR-1)
#define     CHAR_LARGE                          ((1<<12)-2*SIZEADDR-1)
/*
 * for STACK & QUERY use, constant len stack & query
 */
#define     LIST_SMALL                          ((1<<4)-1)
#define     LIST_MIDDLE                         ((1<<6)-1)
#define     LIST_LARGE                          ((1<<8)-1)
/*
 * if stack & query is single thread, it need NOT lock, a little faster.
 */
#define     SINGLE_THREAD                       1

#define     MAX_PATH                            512
/*
 * used for macro
 */
#define    _TOSTRING(x)                         #x
#define     TOSTRING(x)                        _TOSTRING(x)

#define    _JOIN(x,y)                           x ## y
#define     JOIN(x,y)                          _JOIN(x,y)

/*
 * BASIC typedef
 */
typedef     signed char                         CHAR;
typedef     unsigned char                       UCHAR;
typedef     signed long long int                INT;
typedef     unsigned long long int              UINT;
typedef     signed long long int                MONEY;
typedef     volatile long long int              LOCK;
typedef     bool                                BOOL;
typedef     signed long long int                RESULT;

typedef     signed char                        *PCHAR;
typedef     unsigned char                      *PUCHAR;
typedef     signed long long int               *PINT;
typedef     unsigned long long int             *PUINT;
typedef     signed long long int               *PMONEY;
typedef     volatile long long int             *PLOCK;
typedef     void                               *PVOID;

typedef     union  ADDR                        *PADDR;
typedef     struct threadTraceInfo             *PTINFO;
typedef     struct threadMemoryInfo            *PMINFO;
typedef     class  CListItem                   *PLIST;
typedef     class  CContextItem                *PCONT;
typedef     class  CBufferItem                 *PBUFF;
typedef     class  CMemoryBlock                *PBLOCK;
typedef     class  RArrayStack                 *PSTACK;
typedef     class  RArrayQuery                 *PQUERY;
typedef     class  RMultiEvent                 *PEVENT;
typedef     class  RSign                       *PSIGN;
typedef     class  RThreadInfo                 *PRINFO;

/*
 * GLdbDatabase interface used 
 */
#ifdef    __GLdb_SELF_USE

typedef     class GProtocol*                    PPROT;
typedef     class GApplication*                 PAPP;

#endif  //__GLdb_SELF_USE

/*
 * for DEBUG use
 */
#define     D(a)                                printf("%s  ", #a);
#define     Dd(a)                               printf("%s:%d  ", #a, a);
#define     Dx(a)                               printf("%s:%x  ", #a, a);
#define     Dld(a)                              printf("%s:%ld  ", #a, a);
#define     Dlx(a)                              printf("%s:%lx  ", #a, a);
#define     Dlld(a)                             printf("%s:%lld  ", #a, a);
#define     Dllx(a)                             printf("%s:%llx  ", #a, a);
#define     Dp(a)                               printf("%s:%p  ", #a, a);
#define     Ds(a)                               printf("%s:%s  ", #a, a);
#define     Dn                                  printf("\n");
#define     DD                                  printf


#define     DSIGN(sign)						\
  printf("%p Cont:%p, Buf:%p, event:%llx, size:%lld\n",		\
	 sign, sign->sContext, sign->sOverlap,			\
	 sign->sEvent, sign->sSize);

#define     DCONT(cont)						\
  printf("%p handle:%d, Peer:%p\n",				\
	 cont, cont->bHandle, cont->pPeer);

#define     DSOCK(sock)						\
  printf("sock addr:%s port:%d\n",				\
	 inet_ntoa(sock.saddrin.sin_addr),			\
	 ntohs(sock.saddrin.sin_port));


/*
 * typedef for IOCP, compatible for Windows
 * 
 * ONLY used in linux
 */
#ifdef    __linux

#define     WSADESCRIPTION_LEN                  256             // NOT know
#define     WSASYS_STATUS_LEN                   16              // NOT know
#define     INFINITE                            (DWORD)NEGONE

/*
 * find value in http://fossies.org/linux/rdesktop/disk.h
 */
#define     FILE_FLAG_OVERLAPPED                0x40000000

#define     FILE_SHARE_READ                     0x01
#define     FILE_SHARE_WRITE                    0x02
#define     FILE_SHARE_DELETE                   0x04

#define     OPEN_EXISTING                       1
#define     CREATE_NEW                          2
#define     OPEN_ALWAYS                         3
#define     TRUNCATE_EXISTING                   4
#define     CREATE_ALWAYS                       5

#define     GENERIC_READ                        0x80000000
#define     GENERIC_WRITE                       0x40000000
#define     GENERIC_EXECUTE                     0x20000000
#define     GENERIC_ALL                         0x10000000

#define     SOCKET_ERROR                        NEGONE

typedef     unsigned short                      WORD;
typedef     UINT                                HANDLE;
typedef     class CContextItem                 *SOCKET;
typedef     class CContextItem                 *FILEHANDLE;
typedef     unsigned long long int             *ULONG_PTR;      // 64bit in 64bit
typedef     unsigned long long int            **PULONG_PTR;
typedef     unsigned int                        DWORD;
typedef     unsigned int                       *LPDWORD;
typedef     void                               *LPWSAPROTOCOL_INFO;
typedef     void                               *GROUP;          // NOT konw
typedef     void                               *LPWSAOVERLAPPED_COMPLETION_ROUTINE;
typedef     void                               *POLAPCR;

typedef     unsigned char                      *LPCTSTR;
typedef     void                               *LPSECURITY_ATTRIBUTES;
typedef     void                               *LPVOID;

typedef     struct WSAData {
  WORD      wVersion;
  WORD      wHighVersion;
  char      szDescription[WSADESCRIPTION_LEN+1];
  char      szSystemStatus[WSASYS_STATUS_LEN+1];
  WORD      iMaxSockets;
  WORD      iMaxUdpDg;
  PCHAR     lpVendorInfo;
}WSADATA, *LPWSADATA;

/*
 * The struct used by Windows, 
 *
 * I changed its type for less type conversion.
 *   len : original type is u_long
 *   buf : original type is char*
 *
 * I change the order of two field, for &(WSABUF) == &(WSABUF.buf)
 */
typedef     struct __WSABUF {
  PUCHAR    buf;
  UINT      len;
}WSABUF, *LPWSABUF, *PWSABUF;

/*
 * The most important struct for IOCP in Windows
 *
 * In windows, Internal & InterrnalHigh is used by windows kernal, so I use it for 
 *   GLdbIOCP itself.
 * Internal    : original type is ULONG_PTR, I change it to PCONT, for pointer the
 *               struct of ContextItem, which store all information for I/O handle
 * InternalHigh: original type is ULONG_PTR, I use it for point to buffer
 * events      : NEW member, store EPOLLIN or EPOLLOUT or other EPOLLxx I defined.
 * doneSize    : NEW member, as its name. I store it in OVERLAPPED, so lpOverlapped
 *               in PostQueuedCompletionStatus() will NOT be NULL. it is different 
 *               than Windows.
 * accSocket   : for AcceptEx, store accept SOCKET. OLAP with accSocket is push
 *               to readBuffer of listening socket.
 */
typedef     struct _WSAOVERLAPPED {
  PCONT     Internal;
  LPWSABUF  InternalHigh;
  union {
    struct {
      DWORD Offset;
      DWORD OffsetHigh;
    };
    PVOID   Pointer;
  };
  HANDLE    hEvent;
  SOCKET    accSocket;
}WSAOVERLAPPED, *LPWSAOVERLAPPED, OVERLAPPED, *LPOVERLAPPED, OLAP, *POLAP;

#endif // __linux


/*
 * most common struct
 * I create this, for do C-style in C++. 
 */
#define     ADDR_SELF_OPERATION(op)				\
  void operator op (const UINT &one) {				\
    this->aLong op (one); };

#define     ADDR_POINT_OPERATION(op)				\
  void operator op (const PUCHAR &one) {			\
    this->pChar op (one); };					\
  void operator op (const PVOID &one) {				\
    this->pChar op (PUCHAR)(one); };				\
  void operator op (const PLIST &one) {				\
    this->pList op (one); };

typedef     union ADDR{
  UINT      aLong;
  PUCHAR    pChar;
  PUINT     pLong;
  PVOID     pVoid;
  PADDR     pAddr;
  PLIST     pList;
  PCONT     pCont;
  PBUFF     pBuff;
  PSIGN     pSign;

  ADDR_SELF_OPERATION(=)
  ADDR_SELF_OPERATION(+=)
  ADDR_SELF_OPERATION(-=)
  ADDR_SELF_OPERATION(&=)
  ADDR_SELF_OPERATION(|=)
  ADDR_SELF_OPERATION(^=)

  ADDR_POINT_OPERATION(=)
}ADDR;

#define     ADDR_COMPARE(op)					\
  BOOL inline operator op (ADDR &one, const ADDR &two) {	\
    return (one.aLong op two.aLong); };				\
  BOOL inline operator op (ADDR &one, const UINT &two) {	\
    return (one.aLong op two); };				\
  BOOL inline operator op (ADDR &one, const PUCHAR &two) {	\
    return (one.pChar op two); };				\
  BOOL inline operator op (ADDR &one, const PUINT &two) {	\
    return (one.pLong op two); };				\
  BOOL inline operator op (ADDR &one, const PVOID &two) {	\
    return (one.pVoid op two); };				\
  BOOL inline operator op (ADDR &one, const PADDR &two) {	\
    return (one.pAddr op two); };				\
  BOOL inline operator op (ADDR &one, const PLIST &two) {	\
    return (one.pList op two); };				\
  BOOL inline operator op (ADDR &one, const PCONT &two) {	\
    return (one.pCont op two); };				\
  BOOL inline operator op (ADDR &one, const PBUFF &two) {	\
    return (one.pBuff op two); };				\
  BOOL inline operator op (ADDR &one, const PSIGN &two) {	\
    return (one.pSign op two); };

ADDR_COMPARE(==)
ADDR_COMPARE(!=)
ADDR_COMPARE(>)
ADDR_COMPARE(>=)
ADDR_COMPARE(<)
ADDR_COMPARE(<=)

#define ADDR_OPERATION(op)					\
  UINT inline operator op (ADDR &one, ADDR &two) {		\
    return (one.aLong op two.aLong); };				\
  ADDR inline operator op (ADDR &one, const UINT &two) {	\
    ADDR ret;							\
    ret.aLong = one.aLong op two;  return ret; };

ADDR_OPERATION(+)
ADDR_OPERATION(-)
ADDR_OPERATION(&)
ADDR_OPERATION(|)
ADDR_OPERATION(^)
ADDR_OPERATION(&&)
ADDR_OPERATION(||)

/*
 * a binary-safe string type
 */
typedef     class STRING
{
public:
  PUCHAR    strStart;
  PUCHAR    strEnd;
public:
  void virtual operator = (STRING &one) {
    strStart = one.strStart;
    strEnd = one.strEnd;
  };
  void virtual operator = (const PUCHAR pchar) {
    strEnd = strStart = pchar;
    while (*strEnd++);
    strEnd --;
  };
  void virtual operator = (const PCHAR pchar) {
    return operator = ((PUCHAR)pchar);
  };
  STRING() {
    strStart = strEnd = NULL;
  };
  UINT      strLen(void) {
    return strEnd - strStart;
  };
}STRING, *PSTRING;

#define     STRING_FUNCTION(classname, size)			\
  typedef   class classname : public STRING			\
  {								\
  public:							\
    UCHAR   string[size + 1];					\
  public:							\
    void virtual operator = (STRING &one) {			\
      STRING::operator = (one);					\
      UINT  slen = strEnd - strStart + 1;			\
      if (slen > size) slen = size;				\
      memcpy(string, strStart, slen);				\
      strStart = strEnd = string;				\
      strEnd += (slen - 1);					\
      *(strEnd + 1) = 0;					\
      return;							\
    };								\
    void virtual operator = (const PUCHAR pchar) {		\
      memcpy(string, pchar, size);				\
      string[size + 1] = 0;					\
      STRING::operator = (string);				\
      return;							\
    };								\
    void virtual operator = (const PCHAR pchar) {		\
      return operator = ((PUCHAR)pchar);			\
    };								\
    void virtual operator += (STRING &one) {			\
      UINT  slen = size - (strEnd - strStart) - 1;		\
      UINT  dlen = one.strEnd - one.strStart + 1;		\
      if (dlen > slen) dlen = slen;				\
      memcpy(strEnd + 1, one.strStart, slen);			\
      strEnd += slen;						\
      *(strEnd + 1) = 0;					\
      return;							\
    };								\
    void virtual operator += (const PUCHAR pchar) {		\
      UINT  slen = size - (strEnd - strStart) - 1;		\
      memcpy(strEnd + 1, pchar, slen);				\
      string[size + 1] = 0;					\
      while (*strEnd++);					\
      strEnd --;						\
      return;							\
    };								\
    void virtual operator += (const PCHAR pchar) {		\
      return operator += ((PUCHAR)pchar);			\
    };								\
  }classname, *JOIN(P,classname);

STRING_FUNCTION(STR_S, CHAR_SMALL)
STRING_FUNCTION(STR_M, CHAR_MIDDLE)
STRING_FUNCTION(STR_L, CHAR_LARGE)


INT         StrCmp(STRING &one, STRING &two);

#define     STR_STR_COMPARE(op)				        \
  BOOL inline operator op (STRING one, STRING two) {		\
    return (StrCmp(one, two) op 0);				\
  };

STR_STR_COMPARE(==)
STR_STR_COMPARE(!=)
STR_STR_COMPARE(>)
STR_STR_COMPARE(>=)
STR_STR_COMPARE(<)
STR_STR_COMPARE(<=)


/*
 * only little lazy
 */
typedef     union SOCKADDR
{
  sockaddr_in saddrin;
  sockaddr  saddr;
}SOCK, *PSOCK;


/*
 * Synchronous, Atom operation
 * All GLdb lock will be free immediately
 */
#define     NOT_IN_PROCESS                      (UINT)0xa0
#define     IN_PROCESS                          (UINT)0xa1

#define     CmpExg                              __sync_bool_compare_and_swap
#define     LockAdd(p,s)                        __sync_fetch_and_add(&p, s)
#define     LockInc(p)                          __sync_fetch_and_add(&p, 1)
#define     LockDec(p)                          __sync_fetch_and_add(&p, -1)

#define   __LOCKp(lock)				                \
  while(!CmpExg(lock, NOT_IN_PROCESS, IN_PROCESS));
#define   __FREEp(lock)                                         \
  *lock = NOT_IN_PROCESS;
#define   __LOCK(lock)				                \
  while(!CmpExg(&lock, NOT_IN_PROCESS, IN_PROCESS));
#define   __FREE(lock)	                			\
  lock = NOT_IN_PROCESS;
#define   __LOCK__TRY(lock)		        	        \
  !CmpExg(&lock, NOT_IN_PROCESS, IN_PROCESS)


/*
 * For fit memory border
 */
#define     PAD_SIZE(p, pad, bord)				\
  ((sizeof(p) + pad - 1) & (-1 * bord)) + bord
#define     PAD_INT(p, pad, bord)				\
  ((p + pad - 1) & (-1 * bord)) + bord
#define     PAD_ADDR(p, pad, bord)				\
  ((p.aLong + pad - 1) & (-1 * bord)) + bord


/*
 * Thread Local Storage
 *
 * TLS for windows is not very will till VS2013.
 * I boild my own way, same way for Windows & Linux.
 *   the way is similar Linux kernal, save it at the bottom of STACK.
 * in Linux I mmap() stack with MAP_FIXED for border at 2^N, while 2^N is stack size.
 * in Windows, _beginthread could NOT set stack border, 
 *   so set stack size to 2^(N+1), but only use 2^N in border by change RSP
 *
 * TLS only save info for thread, NOT save info for application. 
 * GLdb is statusless inherently, All application info is store in CContextItem & 
 *   CBufferItem. NO information in thread necessary.
 * Any thread process these struct when trigger event.
 * Now TLS save TraceInfo and per-Thread Buffer which base class RThreadResource.
 * and now, there are only one kind of TLS struct for all threads.
 *
 * Typical, thread in GLdb use stack with 16M border.
 * for every 16M, first low 2M is empty and unmmaped. because of 2M is huge_page size.
 *   in GLdb should use huge_page if possible.
 * Next 16k is TraceInfo, with max 1023 call nest.
 * Every class inherit from RThreadResource set its offset above TraceInfo accumulated.
 * for initialization:
 *   volatile UINT RThreadResource::globalResourceOffset =
 *     PAD_TRACE_INFO + SIZE_TRACE_INFO;
 */
#define     SIZE_CACHE                          64LL
#define     SIZE_NORMAL_PAGE                    (0x01LL << 12)  // 4K
#define     SIZE_HUGE_PAGE                      (0x01LL << 21)  // 2M
#define     PAD_THREAD_STACK                    SIZE_HUGE_PAGE
#define     SIZE_THREAD_STACK                   (0x01LL << 24)  // 16M
#define     NEG_SIZE_THREAD_STACK               (-1*SIZE_THREAD_STACK)
#define     REAL_SIZE_THREAD_STACK              (SIZE_THREAD_STACK - PAD_THREAD_STACK)
#define     SIZE_TRACE_INFO                     sizeof(threadTraceInfo)


/*
 * TraceInfo is common function in Linux, but not in Windows.
 * Following the same principle in GLdb, I do it myself.
 * It saved in TLS.
 *
 * now set to MAX_NEST_LOOP, but I do not check, sizeof(TINFO) is 32K
 *   for normal program, it is enough.
 *
 * GLError is another TLS, just like errno in linux.
 *   error define and message store in GError.hpp
 */
#define     MAX_NEST_LOOP                       1023

typedef     struct perTraceInfo {
  PUCHAR    fileInfo;
  PUCHAR    funcInfo;
  UINT      lineInfo;
  UINT      pad;
}OTINFO;

typedef     struct threadTraceInfo {
  UINT      nowLevel;
  PUCHAR    threadName;
  GERROR    GLError;
  UINT      pad;
  OTINFO    calledInfo[MAX_NEST_LOOP];
}TINFO, *PTINFO;

#define     beginCall()						\
  asm volatile ("movq %%rsp, %%rcx;"				\
		"andq %3, %%rcx;"				\
		"addq %4, %%rcx;"				\
		"addq %5, (%%rcx);"				\
		"addq (%%rcx), %%rcx;"				\
		"movq %0, (%%rcx);"				\
		"movq %1, 8(%%rcx);"				\
		"movq %2, 16(%%rcx);"				\
		:						\
		: "i" (__FILE__), "i"(__PRETTY_FUNCTION__),	\
		  "i" (__LINE__),				\
		  "i" (NEG_SIZE_THREAD_STACK),			\
		  "i" (PAD_THREAD_STACK),			\
		  "i" (sizeof(perTraceInfo))			\
		: "%rcx");

#define     endCall()						\
  asm volatile ("movq %%rsp, %%rcx;"				\
		"andq %0, %%rcx;"				\
		"addq %1, %%rcx;"				\
		"movq %%rcx, %%rdx;"				\
		"addq (%%rcx), %%rdx;"				\
		"movq $0, (%%rdx);"				\
		"movq $0, 8(%%rdx);"				\
		"movq $0, 16(%%rdx);"				\
		"subq %2, (%%rcx);"				\
		:						\
		: "i" (NEG_SIZE_THREAD_STACK),			\
		  "i" (PAD_THREAD_STACK),			\
		  "i" (sizeof(perTraceInfo))			\
		: "%rcx", "%rdx");

#define     setLine()						\
  asm volatile ("movq %%rsp, %%rcx;"				\
		"andq %1, %%rcx;"				\
		"addq %2, %%rcx;"				\
		"addq (%%rcx), %%rcx;"				\
		"movq %0, 16(%%rcx);"				\
		:						\
		: "i" (__LINE__),				\
		  "i" (NEG_SIZE_THREAD_STACK),			\
	 	  "i" (PAD_THREAD_STACK)			\
		: "%rcx");

#define     getTraceInfo(info)					\
  asm volatile ("movq %%rsp, %0;"				\
		"andq %1, %0;"					\
		"addq %2, %0;"					\
		: "=r" (info)					\
		: "i" (NEG_SIZE_THREAD_STACK),			\
	          "i" (PAD_THREAD_STACK));


#define     displayTraceInfo(info)			        \
  getTraceInfo(info);						\
  if (info->threadName)						\
    printf("In %p, threa: %s\n", info, info->threadName);	\
  else								\
    printf("In %p, thread:MainThread\n", info);			\
  for (int i=info->nowLevel/sizeof(perTraceInfo)-1; i>=0; i--)	\
    printf("  %d, in file:%14s, line:%4lld, func: %s\n",	\
	    i,							\
	    info->calledInfo[i].fileInfo,			\
	    info->calledInfo[i].lineInfo,			\
	    info->calledInfo[i].funcInfo);


/*
 * class for working thread use following declare,
 * which will be clone or _beginthread
 *
 * getThreadName is virtual, so MUST declare in every class
 */
#define   __class(name)						\
  class name {							\
  protected:							\
    virtual const char* getThreadName(void) {			\
      return #name; };						\
    PTINFO    threadInfo;					\
  private:

#define   __class_(name, base)		                	\
  class name : public base {					\
  protected:							\
    virtual const char* getThreadName(void) {			\
      return #name; };                                      	\
  private:

#define     setThreadName()			               	\
  getTraceInfo(threadInfo);					\
  threadInfo->threadName = (PUCHAR)getThreadName();


/*
 * Implement TraceInfo with ErrorControl
 * make TRY & CATCH and TraceInfo together
 */
#define    _TO_MARK(x)                         _rm_ ## x
#define    _rm_MarkMax                          0xffffffff

#define	  __TRY                                                 \
  RESULT ret_err = __LINE__;					\
  beginCall();
#define   __MARK(x)                                             \
  static RESULT _TO_MARK(x) = ret_err = __LINE__;		\
  setLine();
#define   DEF_MARK(x)						\
  static RESULT _TO_MARK(x);
#define   __MARK_(x)						\
  _TO_MARK(x) = ret_err = __LINE__;				\
  setLine();
#define   __CATCH_BEGIN                                         \
  endCall();							\
  return 0;							\
error_stop:							\
  endCall();							\
  if (ret_err == 0) return 0;
#define   __BETWEEN(x,y)                                        \
  if (ret_err >= _TO_MARK(x) && ret_err <= _TO_MARK(y))
#define   __BEFORE(x)                                           \
  if (ret_err < _TO_MARK(x))
#define   __AFTER(x)                                            \
  if (ret_err >= _TO_MARK(x))
#define   __CATCH_END                                           \
  return ret_err;
#define   __CATCH                                               \
  endCall();							\
  return 0;							\
error_stop:							\
  endCall();							\
  return ret_err;
#define   __CATCH_(ret)						\
  endCall();							\
  return ret;							\
error_stop:							\
  endCall();							\
  return 0;
#define   __TRY__                                               \
  beginCall();
#define   __CATCH__                                             \
  endCall();							\
  return 0;
#define   __RETURN_(ret)					\
  endCall();							\
  return ret;
#define   __BREAK                                               \
  { goto error_stop; }
#define   __BREAK_OK                                            \
  { ret_err = 0; goto error_stop; }

#define     MESSAGE_INFO                        (1 << 0)
#define     MESSAGE_DEBUG                       (1 << 1)
#define     MESSAGE_ERROR                       (1 << 2)
#define     MESSAGE_HALT                        (1 << 3)

void      __MESSAGE_(INT level, const char * _Format, ...);
void      __MESSAGE (INT level);
void      __TRACE();

#define     TRACE                             __TRACE();

#define   __INFO(level, _Format,...) {				\
    __MESSAGE_(level, _Format,##__VA_ARGS__);			\
  };

#define   __DO1(val, func) {					\
    setLine();							\
    val = func;							\
    if (val == NEGONE) {					\
      WSASetLastError(errno);					\
      __BREAK;				                        \
    }								\
  };

#define   __DOc_(func, _Format,...) {				\
    setLine();							\
    if (func)							\
      __INFO(MESSAGE_INFO, _Format,##__VA_ARGS__);		\
  };
#define   __DOc(func) {						\
    setLine();							\
    func;							\
  };

#define   __DO(func) {						\
    setLine();							\
    if (func) {							\
      __MESSAGE(MESSAGE_DEBUG);					\
      __BREAK;							\
    }								\
  };

#define   __DOb(func) {						\
    setLine();							\
    func;							\
    __BREAK;							\
  };

#define   __DOe(func, err) {					\
    setLine();							\
    if (func) {							\
      ERROR(err);						\
      __MESSAGE(MESSAGE_DEBUG);					\
      __BREAK;							\
    }								\
  };
/*
 * classic data structure
 *
 * After init, ArrayStack is empty, can full it by FullArrayStack for block memory
 * if parentArray is not NULL, child stack can get more, or free to it. when child is
 *   empty, it get getSize from parent, when child is full, it free freeSize to parent.
 *   typical child stack is per-thread, while parent stack is global.
 * 
 * arrayStart & arrayEnd will NOT change again after Initialize,
 * see InitArrayStack for other initialize value.
 */
typedef     class RArrayStack {
private:
  LOCK      inProcess;
  ADDR      arrayStart;
  ADDR      arrayEnd;
  ADDR      arrayFree;
  PSTACK    parentArray;
  BOOL      singleThread;
  UINT      getSize;
  UINT      freeSize;
public:
  void      InitArrayStack(
            ADDR start, UINT number, BOOL singlethread = 0,
	    PSTACK parent = 0, UINT getsize = 0, UINT freesize = 0)
  {
    inProcess = NOT_IN_PROCESS;
    arrayStart = start;
    arrayEnd = start + (number-1) * SIZEADDR;
    arrayFree = start + number * SIZEADDR;
    parentArray = parent;
    singleThread = singlethread;
    getSize = getsize;
    freeSize = freesize;
  };
  RESULT      FullArrayStack(
              ADDR begin, UINT size, UINT number)
  {
  __TRY__
    __LOCK(inProcess);
    do {
      arrayFree -= SIZEADDR;
      *(arrayFree.pAddr) = begin;
      begin += size;
      number--;
    } while ((arrayFree > arrayStart) && number);
    __FREE(inProcess);
  __CATCH__
  };

/*
 * in GetMulti & FreeMulti, addr is child's freeStart.
 * it copy memory and change freeStart in child thread if possible, NO return val.
 * parent thread always multi thread, so LOCK.
 */
  void      GetMulti(ADDR &addr, UINT number)
  {
    UINT    getsize, getnumber;
    __LOCK(inProcess);
    getsize = arrayEnd - arrayFree;
    getnumber = getsize / SIZEADDR;
    if (getnumber < number) number = getnumber;
    if (number) {
      getsize = number * SIZEADDR;
      addr -= getsize;
      memcpy(addr.pVoid, arrayFree.pVoid, getsize);
      arrayFree += getsize;
    }
    __FREE(inProcess);
  };
  void      FreeMulti(ADDR &addr, UINT number)
  {
    UINT    freesize;
    __LOCK(inProcess);
    freesize = number * SIZEADDR;
    if (number) {
      arrayFree -= freesize;
      memcpy(arrayFree.pVoid, addr.pVoid, freesize);
      addr += freesize;
    }
    __FREE(inProcess);
  };
  RESULT    operator += (ADDR addr) 
  {
  __TRY
    if (!singleThread) __LOCK(inProcess);
    if ((arrayFree <= arrayStart) && parentArray) {
      parentArray->FreeMulti(arrayFree, freeSize);
    }
    __DOe(arrayFree <= arrayStart, 
            GL_STACK_FULL);
    arrayFree -= SIZEADDR;
    *(arrayFree.pAddr) = addr;
    if (!singleThread) __FREE(inProcess);
  __CATCH_BEGIN
    if (!singleThread) __FREE(inProcess);
  __CATCH_END
  };
  RESULT    operator -= (ADDR &addr) 
  {
  __TRY
    if (!singleThread) __LOCK(inProcess);
    if ((arrayFree > arrayEnd) && parentArray) {
      parentArray->GetMulti(arrayFree, getSize);
    }
    __DOe(arrayFree > arrayEnd, 
            GL_STACK_EMPTY);
    addr = *(arrayFree.pAddr);
    arrayFree += SIZEADDR;
    if (!singleThread) __FREE(inProcess);
  __CATCH_BEGIN
    addr = ZERO;
    if (!singleThread) __FREE(inProcess);
  __CATCH_END
  };
  UINT      GetNumber(void)
  {
    if (arrayEnd < arrayFree) return 0;
    return ((arrayEnd - arrayFree) / SIZEADDR + 1);
  };
}STACK, *PSTACK;

/*
 * in my habit, constant len class initialize in constructor.
 *       while, class with variable part initialize in Initxx function.
 */
#define     STACK_CLASS(classname, size, singlethread)		\
  typedef   class classname : public RArrayStack  {		\
  private:							\
    ADDR    stackData[size + 1];				\
  public:							\
    classname()							\
    {								\
      ADDR  start;						\
      start = &(stackData[0]);					\
      InitArrayStack(start, size, singlethread);		\
    };								\
  }classname, *JOIN(P,classname);

STACK_CLASS(STACK_S, LIST_SMALL, 0)
STACK_CLASS(STACK_M, LIST_MIDDLE, 0)
STACK_CLASS(STACK_L, LIST_LARGE, 0)

STACK_CLASS(STACK_s, LIST_SMALL, SINGLE_THREAD)
STACK_CLASS(STACK_m, LIST_MIDDLE, SINGLE_THREAD)
STACK_CLASS(STACK_l, LIST_LARGE, SINGLE_THREAD)


/*
 * in my arithmetic, if query length is 1, could not distinguish full or empty.
 */
#define     MIN_ARRAY_QUERY                     2

typedef     class RArrayQuery 
{
private:
  LOCK      inProcess;
  ADDR      arrayStart;
  ADDR      arrayEnd;
  ADDR      freeStart;
  ADDR      freeEnd;
  BOOL      singleThread;
public:
  void      InitArrayQuery(ADDR start, UINT number, BOOL singlethread = 0) 
  {
    inProcess = NOT_IN_PROCESS;
    if (number < MIN_ARRAY_QUERY) number = MIN_ARRAY_QUERY;

    arrayStart = freeStart = start;
    freeEnd = start + (number-1) * SIZEADDR;
    arrayEnd = start + number * SIZEADDR;
    singleThread = singlethread;
  };
  RESULT    operator += (ADDR addr)
  {
  __TRY
    if (!singleThread) __LOCK(inProcess);
    __DOe(freeStart == freeEnd, 
            GL_QUERY_FULL);
    *(freeStart.pAddr) = addr;
    freeStart += SIZEADDR;
    if (freeStart == arrayEnd) freeStart = arrayStart;
    if (!singleThread) __FREE(inProcess);
  __CATCH_BEGIN
    if (!singleThread) __FREE(inProcess);
  __CATCH_END
  };
  RESULT    operator -= (ADDR &addr)
  {
  __TRY
    ADDR    freeend;
    if (!singleThread) __LOCK(inProcess);
    freeend = freeEnd + SIZEADDR;
    if (freeend == arrayEnd) freeend = arrayStart;
    __DOe(freeend == freeStart, 
            GL_QUERY_EMPTY);
    addr = *(freeend.pAddr);
    freeEnd = freeend;
    if (!singleThread) __FREE(inProcess);
  __CATCH_BEGIN
    addr = ZERO;
    if (!singleThread) __FREE(inProcess);
  __CATCH_END
  };
  RESULT    TryGet(ADDR &addr)
  {
  __TRY
    ADDR    freeend;
    freeend = freeEnd + SIZEADDR;
    if (freeend == arrayEnd) freeend = arrayStart;
/*
 * for TryGet, empty is ok, not error. do not __DOe()
 */
    if (freeend == freeStart) __BREAK;
    addr = *(freeend.pAddr);
  __CATCH_BEGIN
    addr = ZERO;
  __CATCH_END
  };
  RESULT    TryAndGet(ADDR &addr)
  {
  __TRY
    ADDR    freeend;
    if (!singleThread) __LOCK(inProcess);
    freeend = freeEnd + SIZEADDR;
    if (freeend == arrayEnd) freeend = arrayStart;
    if (freeend == freeStart) __BREAK;
    addr = *(freeend.pAddr);
    freeEnd = freeend;
    if (!singleThread) __FREE(inProcess);
  __CATCH_BEGIN
    addr = ZERO;
    if (!singleThread) __FREE(inProcess);
  __CATCH_END
  };

  UINT      GetNumber(void)
  {
    if (freeStart > freeEnd) 
      return ((freeStart-freeEnd) / SIZEADDR -1);
    else 
      return (((arrayEnd-freeEnd) + (freeStart-arrayStart)) / SIZEADDR - 1);
  };
}QUERY, *PQUERY;

#define     QUERY_CLASS(classname, size, singlethread)		\
  typedef   class classname : public RArrayQuery {		\
  private:							\
    ADDR    queryData[size + 1];				\
  public:							\
    void JOIN(Init,classname)()					\
    {								\
      ADDR  start;						\
      start = &(queryData[0]);					\
      InitArrayQuery(start, size, singlethread);		\
    };								\
    classname()							\
    {								\
      JOIN(Init,classname)();					\
    };								\
  }classname, *JOIN(P,classname);

QUERY_CLASS(QUERY_S, LIST_SMALL, 0)
QUERY_CLASS(QUERY_M, LIST_MIDDLE, 0)
QUERY_CLASS(QUERY_L, LIST_LARGE, 0)

QUERY_CLASS(QUERY_s, LIST_SMALL, SINGLE_THREAD)
QUERY_CLASS(QUERY_m, LIST_MIDDLE, SINGLE_THREAD)
QUERY_CLASS(QUERY_l, LIST_LARGE, SINGLE_THREAD)


/*
 * continuous += CLOCK_THREAD_CPUTIME_ID 55,  90,  300ns
 * continuous += CLOCK_MONOTONIC_RAW     33,  60,  180ns
 * CLOCK_MONOTONIC_COARSE interval is 4ms, too long
 */
#define     MAX_TIME_QUERY                      LIST_MIDDLE
#define     NANO_SECOND                         (1000 * 1000 * 1000)

#define     DIFF_TIME(now, last)				\
  ((now).tv_sec - (last).tv_sec) * NANO_SECOND +		\
  ((now).tv_nsec - (last).tv_nsec);

typedef     class RArrayTime {
private:
  clockid_t timeType;
  QUERY_m   timeQuery;
  struct    timespec timeStart;

public:
  RArrayTime(clockid_t timetype)
  {
    timeType = timetype;
    clock_gettime(timeType, &timeStart);
  };
  RESULT    operator += (struct timespec *timenow)
  {
    ADDR    diff;
    clock_gettime(timeType, timenow);
    diff = DIFF_TIME(*timenow, timeStart);
    return timeQuery += diff;
  };
  RESULT    operator -= (ADDR &addr)
  {
    return timeQuery -= addr;
  };
  void      OutputTime(void)
  {
    ADDR    timelast = {0}, timenext;
    UINT    i = 1;
    while (!(timeQuery -= timenext)) {
      printf("No. %2lld, time: %8lld, diff: %8f\n", 
	     i++, timenext.aLong, 
	     ((double)(timenext - timelast))/(1000*1000));
      timelast = timenext;
    };
  };
}TIME, *PTIME;

#endif   // GLdb_COMMON_HPP
