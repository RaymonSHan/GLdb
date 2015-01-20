/*
 * GLdb memory pool header file
 *
 * GLdb use static memory alloc, alloc huge memory by mmap() once,
 *   then alloc to application by memory pool.
 * This alloc is thread-safe, and every thread have local buffer for high
 *   performance. These local buffer store in TLS. One buffer alloc by one thread 
 *   can be free by another thread, and reuse by all threads.
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
 *
 */

#ifndef     GLdb_MEMORY_HPP
#define     GLdb_MEMORY_HPP

#include   "GLdbCommon.hpp"

/*
 * const for MARK memory alloc
 */
#define     MARK_USED_END                        0x30
#define     MARK_UNUSED                          0x28
#define     MARK_FREE_END                        0x20
#define     MARK_USED                            0x10
#define     MARK_MAX	                         0x100

/*
 * The function get memroy from system
 */
#define     SEG_START_BUFFER                    (0x52LL << 40)  // 'R'

UINT        GetMemory(ADDR &addr, UINT size, UINT flag = 0);
UINT        GetStack(ADDR &addr);

/*
 * TLS for memory pool
 */
#define     GetThreadMemoryInfo()		\
  threadMemoryInfo *info;			\
  getThreadInfo(info, nowOffset);

/*
 * I have tested, three thread do nothing but GET/FREE, 
 * no performance improve after thread local cache large than 10.
 */
#define     MAX_LOCAL_CACHE                     16

typedef     struct threadMemoryInfo {
  STACK     memoryStack;
  ADDR      localCache [MAX_LOCAL_CACHE];
  UINT      threadFlag;
  ADDR      localUsedList;                      // usedList start
  threadMemoryInfo *threadListNext;             // pointer to next TLS
}threadMemoryInfo;

/*
 * for NonDirectly free memory pool, link every used item by usedList
 * when used, countDown set to CurrentTime + Timeout. when FutureTime equal
 *   countDown, set countDown to TIMEOUT_QUIT
 */
#define     TIMEOUT_QUIT                        2

class       CListItem {
public:
  ADDR      usedList;
  UINT      countDown;
};


/*
 * Memory Pool main class
 *
 * address of free item is store in address pointed by memoryArrayFree
 */
class       CMemoryAlloc : public RThreadResource {
private:                                        // for total memory
  ADDR      RealBlock;                          // address for memory start
  UINT      BorderSize;                         // real byte size for one item
  UINT      ArraySize;                          // number * sizeof(ADDR)
  UINT      TotalSize;                          // Total memory size in byte

private:                                        // for thread info
  UINT      TotalNumber;
  STACK     globalStack;
  threadMemoryInfo *threadListStart;            // TLS list start
  ADDR      memoryArrayStart;                   // array start
  ADDR      memoryArrayFree;                    // free now
  ADDR      memoryArrayEnd;                     // array end

private:                                        // spin lock
  LOCK      InProcess;                          // used for global GET/FREE
  PLOCK     pInProcess;

private:
  UINT      DirectFree;
  UINT      TimeoutInit;
  UINT      BufferSize;

public:
  CMemoryAlloc();
  ~CMemoryAlloc();

private:
  UINT      GetOneList(ADDR &nlist);
  UINT      FreeOneList(ADDR nlist);
  UINT      AddToUsed(ADDR nlist, UINT timeout);
  UINT      GetListGroup(ADDR &groupbegin, UINT number);
  UINT      FreeListGroup(ADDR &groupbegin, UINT number);
  UINT      CountTimeout(ADDR usedStart);

public:
  UINT      SetThreadArea(UINT getsize, UINT maxsize,
			  UINT freesize, UINT flag);
  UINT      SetMemoryBuffer(UINT number, UINT size, 
			    UINT border, UINT direct);
  UINT      DelMemoryBuffer(void);
  UINT      GetMemoryList(ADDR &nlist, UINT timeout);
  UINT      FreeMemoryList(ADDR nlist);
  UINT      TimeoutAll(void);
  UINT      GetNumber() { return TotalNumber; };
  
  void      DisplayFree(void);

#ifdef     _TESTCOUNT                           // for test function
public:                                         // statistics info for debug
  UINT      GetCount, GetSuccessCount;
  UINT      FreeCount, FreeSuccessCount;
  UINT      MinFree;  
  void      DisplayLocal(threadMemoryInfo* info);
  void      DisplayArray(void);
  void      DisplayInfo(void);
  void      DisplayContext(void);
#endif  // _TESTCOUNT
};


#define UsedList                pList->usedList
#define CountDown               pList->countDown

#endif   // GLdb_MEMORY_HPP
