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

#include   <errno.h>
#include   <fcntl.h> 
#include   <sched.h>
#include   <signal.h>
#include   <stdio.h>
#include   <stdlib.h>
#include   <stdarg.h>
#include   <string.h>
#include   <time.h>
#include   <unistd.h>

#include   <arpa/inet.h>
#include   <netinet/in.h>

#include   <sys/epoll.h>
#include   <sys/eventfd.h>
#include   <sys/mman.h>
#include   <sys/socket.h> 
#include   <sys/types.h>
#include   <sys/wait.h>

/*
 * BASIC typedef
 *
 * In GLdb, money is signed int64, 1 million means 1 dollar, 
 * range is about +/- 9 thousand billion, enough for enterprise use.
 */
#define     ONE_DOLLAR_VALUE                    (100*100*100)

typedef     signed char                         CHAR;
typedef     unsigned char                       UCHAR;
typedef     signed long long int                INT;
typedef     unsigned long long int              UINT;
typedef     signed long long int                MONEY;
typedef     volatile long long int              LOCK;
typedef     bool                                BOOL;

typedef     signed char*                        PCHAR;
typedef     unsigned char*                      PUCHAR;
typedef     signed long long int*               PINT;
typedef     unsigned long long int*             PUINT;
typedef     void*                               PVOID;
typedef     signed long long int*               PMONEY;
typedef     long long int*                      PLOCK;

typedef     union ADDR*                         PADDR;
typedef     class CListItem*                    PLIST;
typedef     class CContextItem*                 PCONT;
typedef     class CBufferItem*                  PBUFF;


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
    return (one.pBuff op two); };

ADDR_COMPARE(==)
ADDR_COMPARE(!=)
ADDR_COMPARE(>)
ADDR_COMPARE(>=)
ADDR_COMPARE(<)
ADDR_COMPARE(<=)

#define ADDR_OPERATION(op)					\
  ADDR inline operator op (ADDR &one, ADDR &two) {		\
      ADDR ret;							\
      ret.aLong = one.aLong op two.aLong;  return ret; };	\
  ADDR inline operator op (ADDR &one, const UINT &two) {	\
      ADDR ret;							\
      ret.aLong = one.aLong op two;  return ret; };

ADDR_OPERATION(+)
ADDR_OPERATION(-)
ADDR_OPERATION(&)
ADDR_OPERATION(|)
ADDR_OPERATION(^)

/*
 * a binary-safe string type
 */
typedef     struct STRING
{
  PUCHAR    strStart;
  PUCHAR    strEnd;

  void operator = (STRING &one) {
    this->strStart = one.strStart;
    this->strEnd = one.strEnd;
  };
  void operator = (const PUCHAR pchar) {
    this->strEnd = this->strStart = pchar;
    while (*this->strEnd++);
    this->strEnd --;
  };
  void operator = (const char *pchar) {
    return operator = ((PUCHAR)pchar);
  };
}STRING;

INT         StrCmp(STRING &one, STRING &two);

#define     STR_STR_COMPARE(op)				         \
  BOOL inline operator op (STRING one, STRING two) {		 \
    return (StrCmp(one, two) op 0);				 \
  }
STR_STR_COMPARE(==)
STR_STR_COMPARE(!=)
STR_STR_COMPARE(>)
STR_STR_COMPARE(>=)
STR_STR_COMPARE(<)
STR_STR_COMPARE(<=)


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
#define   __LOCK__TRY(lock)		        	        \
  !CmpExg(&lock, NOT_IN_PROCESS, IN_PROCESS)
#define   __FREE(lock)                                          \
  lock = NOT_IN_PROCESS;


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
 * GLdb is statusless inherently, All application info is store in CContextItem & CBufferItem.
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
#define     SIZE_THREAD_STACK                   (0x01LL << 24)  // 16M
#define     NEG_SIZE_THREAD_STACK               (-1*SIZE_THREAD_STACK)
        
#define     PAD_TRACE_INFO                      SIZE_HUGE_PAGE
#define     SIZE_TRACE_INFO                     sizeof(threadTraceInfo)


/*
 * TraceInfo is common function in Linux, but not in Windows.
 * Following the same principle in GLdb, I do it myself.
 * It saved in TLS.
 */
#define     MAX_NEST_LOOP                       1023            // number of call nest

typedef     struct perTraceInfo {
  PUCHAR    fileInfo;
  PUCHAR    funcInfo;
  UINT      lineInfo;
  UINT      pad;
}perTraceInfo;

typedef     struct threadTraceInfo {
  UINT      nowLevel;                                           // in asm act as offset
  PUCHAR    threadName;
  UINT      pad[2];
  perTraceInfo calledInfo[MAX_NEST_LOOP];
}threadTraceInfo;

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
		  "i" (PAD_TRACE_INFO),				\
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
		  "i" (PAD_TRACE_INFO),				\
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
		  "i" (PAD_TRACE_INFO)				\
		: "%rcx");

#define     getTraceInfo(info)					\
  asm volatile ("movq %%rsp, %0;"				\
		"andq %1, %0;"					\
		"addq %2, %0;"					\
		: "=r" (info)					\
		: "i" (NEG_SIZE_THREAD_STACK),			\
	          "i" (PAD_TRACE_INFO));

#define     displayTraceInfo(info)			        \
  getTraceInfo(info);						\
  if (info->threadName)						\
    printf("In %p, threa: %s\n", info, info->threadName);	\
  else								\
    printf("In %p, thread:Unkonwn\n", info);			\
  for (int i=info->nowLevel/sizeof(perTraceInfo)-1; i>=0; i--)	\
    printf("  %d, in file:%14s, line:%4lld, func: %s\n",	\
	   i,							\
	   info->calledInfo[i].fileInfo,			\
	   info->calledInfo[i].lineInfo,			\
	   info->calledInfo[i].funcInfo);

/*
 * class for working thread use following declare,
 * which will be clone or _beginthread
 */
#define   __class(name)						\
  class name {							\
  protected:							\
  virtual const char* getThreadName(void) {			\
    return #name; };						\
  private:

#define   __class_(name, base)		                	\
  class name : public base {					\
  protected:							\
  virtual const char* getThreadName(void) {			\
    return #name; };                                      	\
  private:

#define     setClassName()			               	\
  getTraceInfo(threadInfo);					\
  threadInfo->threadName = (PUCHAR)getThreadName();

/*
 * get TLS for RThreadResource
 */
#define     getThreadInfo(info, off)				\
  asm volatile ("movq %%rsp, %0;"				\
		"andq %2, %0;"					\
		"addq %1, %0;"					\
		: "=r" (info)					\
		: "m" (off), "i"(NEG_SIZE_THREAD_STACK));


/*
 * Implement TraceInfo with ErrorControl
 * make TRY & CATCH and TraceInfo together
 */
#define    _TOSTRING(x)                         #x
#define     TOSTRING(x)                        _TOSTRING(x)

#define    _JOIN(x,y)                           x ## y
#define     JOIN(x,y)                          _JOIN(x,y)

#define    _TO_MARK(x)                         _rm_ ## x
#define    _rm_MarkMax                          0xffffffff      // for mark en

#define	  __TRY                                                 \
  UINT ret_err = __LINE__;					\
  beginCall();
#define   __MARK(x)                                             \
  static int _TO_MARK(x) = ret_err = __LINE__;			\
  setLine();
#define   __CATCH_BEGIN                                         \
  endCall();							\
  return 0;							\
error_stop:

#define   __BETWEEN(x,y)                                        \
  if (ret_err >= _TO_MARK(x) && ret_err <= _TO_MARK(y))
#define   __AFTER(x)                                            \
  if (ret_err >= _TO_MARK(x))

#define   __CATCH_END                                           \
  endCall();							\
  return ret_err;
#define   __CATCH                                               \
  endCall();							\
  return 0;							\
error_stop:							\
  endCall();							\
  return ret_err;
#define   __TRY__                                               \
  beginCall();
#define   __CATCH__                                             \
  endCall();							\
  return 0;
#define   __BREAK                                               \
  { goto error_stop; }
#define   __BREAK_OK                                            \
  { ret_err = 0; goto error_stop; }

#define     MESSAGE_DEBUG                       0x0001
#define     MESSAGE_ERROR                       0x0002
#define     MESSAGE_HALT                        0x0004

void      __MESSAGE(INT level, const char * _Format, ...);

#define   __INFO(level, _Format,...) {				\
    __MESSAGE(level,  _Format,##__VA_ARGS__);			\
  }
#define   __INFOb(level, _Format,...) {				\
    __MESSAGE(level, _Format,##__VA_ARGS__); __BREAK_OK		\
  }

#define   __DO1c_(val, func, _Format,...) {			\
    setLine();							\
    val = func;							\
    if (val == -1) 						\
      __INFO(MESSAGE_DEBUG, _Format,##__VA_ARGS__);		\
  }
#define   __DO1c(val, func) {					\
    setLine();							\
    val = func;							\
  }

#define   __DO1_(val, func, _Format,...) {			\
    setLine();							\
    val = func;							\
    if (val == -1) {						\
      __INFO(MESSAGE_DEBUG, _Format,##__VA_ARGS__);		\
      __BREAK							\
    }								\
  }
#define   __DO1(val, func) {					\
    setLine();							\
    val = func;							\
    if (val == -1) __BREAK;					\
  }

#define   __DOc_(func, _Format,...) {				\
    setLine();							\
    if (func)							\
      __INFO(MESSAGE_DEBUG, _Format,##__VA_ARGS__);		\
  }
#define   __DOc(func) {						\
    setLine();							\
    func;							\
  }

#define   __DO_(func, _Format,...) {				\
    setLine();							\
    if (func) {							\
      __INFO(MESSAGE_DEBUG, _Format,##__VA_ARGS__);		\
      __BREAK							\
    }								\
  }
#define   __DO(func) {						\
    setLine();							\
    if (func) __BREAK;						\
  }

#define   __DOb_(func, _Format,...) {				\
    setLine();							\
    if (func) {							\
      __INFO(MESSAGE_DEBUG, _Format,##__VA_ARGS__);		\
    }								\
  __BREAK							\
  }
#define   __DOb(func) {						\
    setLine();							\
    func;							\
    __BREAK;							\
  }


/*
 * any TLS except TraceInfo MUST be inherit from RThreadResource
 * volatile UINT RThreadResource::globalResourceOffset =
 *   PAD_TRACE_INFO + SIZE_TRACE_INFO;
 *   set in GLdbCommon.cpp
 */
class       RThreadResource
{
protected:
  UINT      nowOffset;
public:
  static    volatile UINT globalResourceOffset;
public:
  RThreadResource(UINT size) {
    SetResourceOffset(size);
  };
  UINT      SetResourceOffset(UINT size) {
    nowOffset = LockAdd(RThreadResource::globalResourceOffset, 
			PAD_INT(size, 0, 64));
    return nowOffset;
  };
};

#endif   // GLdb_COMMON_HPP

