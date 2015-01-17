/*
 * GLdb common header file
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
 * GLdb is a Multi-thread customed Key-Value No-SQL memory database.
 * GLdb atomic insert voucher & update balance, provide interface for ERP.
 * GLdb have its own Async IO system, support Windows & Linux by IOCP & epoll.
 * GLdb request large memory, so only support 64bit system.
 */

#ifndef     GLdb_COMMON_HPP
#define     GLdb_COMMON_HPP

/*
 * BASIC typedef
 *
 * In GLdb, money is signed int64, 1 million means 1 dollar, 
 * range is about +/- 9 thousand billion, enough for enterprise use.
 */
#define     ONE_DOLLAR_VALUE                    (100*100*100)

typedef     char                                CHAR;
typedef     unsigned char                       UCHAR;
typedef     long long int                       INT;
typedef     unsigned long long int              UINT;
typedef     long long int                       MONEY;
typedef     volatile long long int              LOCK;
typedef     bool                                BOOL;

typedef     char*                               PCHAR;
typedef     unsigned char*                      PUCHAR;
typedef     long long int*                      PINT;
typedef     unsigned long long int*             PUINT;
typedef     void*                               PVOID;
typedef     long long int*                      PMONEY;
typedef     long long int*                      PLOCK;          // pointer always volatitle

/*
 * most common struct
 * I create this, for do C-style in C++. 
 */
#define     ADDR_INT_SELF_OPERATION(op)				\
  void operator op (const UINT &val) { this->aLong op (val); };

typedef union TypeUnion {
  UINT      aLong;
  PUCHAR    pChar;
  PUINT     pLong;
  TypeUnion *pAddr;


}ADDR;

#endif   // GLdb_COMMON_HPP
