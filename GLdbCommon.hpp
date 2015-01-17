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
union       ADDR;
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
typedef     long long int*                      PLOCK;          // pointer always volatitle

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
    this->pChar op (PUCHAR)(one); };

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
 * Thread Local Storage
 *
 * TLS for windows is not very will till VS2013.
 * I boild my own way, same way for Windows & Linux.
 * the way is similar Linux kernal, save it at the bottom of STACK.
 * in Linux I mmap() stack with MAP_FIXED for border at 2^N, while 2^N is stack size.
 * in Windows, _beginthread could NOT set stack border, 
 * so set stack size to 2^(N+1), but only use 2^N in border by change RSP
 */
#endif   // GLdb_COMMON_HPP
