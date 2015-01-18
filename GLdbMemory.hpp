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
INT         GetMemory(ADDR &addr, UINT size, UINT flag = 0);
INT         GetStack(ADDR &addr);

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
  UINT      getSize;
  UINT      freeSize;
  UINT      threadFlag;
  ADDR      localArrayStart;
  ADDR      localFreeStart;
  ADDR      localArrayEnd;
  ADDR      localUsedList;
  threadMemoryInfo *threadListNext;
  ADDR      localCache [MAX_LOCAL_CACHE];
}threadMemoryInfo;

/*
 * for NonDirectly free memory pool, link every used item by usedList
 * when used, countDown set to CurrentTime + TimeoutTime. when FutureTime equal
 *   countDown, set countDown to TIMEOUT_QUIT(2 for normal)
 */
class       CListItem {
public:
  ADDR      usedList;
  UINT      countDown;
};





#endif   // GLdb_MEMORY_HPP
