/*
 * GLdb Encapsulate common network server head file
 *
 * GEncapsulate create network server framework via GLdbIOCP.
 * Application should implement interface defined by GEncapsulate
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

#include    "GCommon.hpp"

#ifdef    __GLdb_SELF_USE

#ifndef     GLdb_ENCAPSULATE_HPP
#define     GLdb_ENCAPSULATE_HPP

#include    "GMemory.hpp"
#include    "GIOCP.hpp"

#define     GlobalMemory                        GEncapsulate::globalMemory
#define     GlobalIOCP                          GEncapsulate::globalIOCP

#define     NUMBER_CONTEXT                      5
#define     NUMBER_BUFFER_SMALL                 10
#define     NUMBER_BUFFER_MIDDLE                2


typedef     class GEncapsulate
{
public:
  static    MEMORY globalMemory;
  static    IOCP globalIOCP;
public:
  HANDLE    handleIOCP;

public:
  static    RESULT InitEncapsulate(void);
  static    RESULT FreeEncapsulate(void);
  static    RESULT Doing(void);

public:
  RESULT    CreateApplication(PSOCK psock, PAPP papp);
  RESULT    StateApplication(PAPP papp, UINT state);
}ENCAP;

#endif   // GLdb_ENCAPSULATE_HPP

#endif  //__GLdb_SELF_USE
