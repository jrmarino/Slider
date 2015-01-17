--
--  Copyright (c) 2014-15 John Marino <draco@marino.st>
--
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--


with System;
with Interfaces.C;
with Interfaces.C.Strings;

package DragonFly.HAMMER.Binding is

   package IC renames Interfaces.C;


   -------------------------------
   ---  DATA TYPE DEFINITIONS  ---
   -------------------------------

   subtype uLong           is IC.unsigned_long;
   subtype hammer_tid      is uInt64;
   subtype hammer_off      is uInt64;
   subtype hammer_seq      is uInt32;
   subtype hammer_crc      is uInt32;
   subtype ioctl_command   is uLong;


   -------------------
   ---  CONSTANTS  ---
   -------------------

   HAMMER_MAX_HISTORY_ELMS     : constant Int32  := 64;
   HAMMER_IOC_HISTORY_ATKEY    : constant uInt32 := 16#01#;
   HAMMER_IOC_HISTORY_NEXT_TID : constant uInt32 := 16#02#;
   HAMMER_IOC_HISTORY_NEXT_KEY : constant uInt32 := 16#04#;
   HAMMER_IOC_HISTORY_EOF      : constant uInt32 := 16#08#;
   HAMMER_IOC_HISTORY_UNSYNCED : constant uInt32 := 16#10#;
   HAMMER_MIN_TID              : constant hammer_tid := 0;
   HAMMER_MAX_TID              : constant hammer_tid := 16#FFFFFFFFFFFFFFFF#;
   HAMMER_MAX_KEY              : constant Int64      := 16#7FFFFFFFFFFFFFFF#;
   O_RDONLY                    : constant uInt32 := 0;       -- fcntl.h

   --  These constants are revealed by dev-tools/print-ioctls.c

   HAMMERIOC_GETHISTORY        : constant ioctl_command := 16#c4506802#;


   --------------------
   ---  STRUCTURES  ---
   --------------------

   type hammer_ioc_head is record
      flags      : uInt32;
      error      : uInt32;
      reserved01 : uInt32;
      reserved02 : uInt32;
      reserved03 : uInt32;
      reserved04 : uInt32;
   end record;


   type hammer_ioc_hist_entry is record
      tid        : hammer_tid;
      time32     : uInt32;
      unused     : uInt32;
   end record;
   type history is array (0 .. HAMMER_MAX_HISTORY_ELMS - 1) of
                   hammer_ioc_hist_entry;

   type hammer_ioc_history is record
      head       : hammer_ioc_head;
      obj_id     : Int64;
      beg_tid    : hammer_tid;
      nxt_tid    : hammer_tid;
      end_tid    : hammer_tid;
      key        : Int64;
      nxt_key    : Int64;
      count      : Int32;
      reserved01 : Int32;
      hist_ary   : history;
   end record;
   type history_access is access hammer_ioc_history;


   ------------------
   --  FUNCTIONS  ---
   ------------------

   function ioctl (
      descriptor : file_descriptor;
      command    : ioctl_command;
      argument   : System.Address)
   return Int32;
   pragma Import (C, ioctl, "ioctl");
   --  The ioctl() system call manipulates the underlying device parameters
   --  of special files.  In particular, many operating characteristics of
   --  character special files (e.g. terminals) may be controlled with
   --  ioctl() requests.  The descriptor must point to an open file.


   procedure retrieve_history (
                open_file : in     file_descriptor;
                history   : in out hammer_ioc_history);
   --  The procedure is passed an open file descriptor and a pointer to
   --  the hammer history structure which is populated when the procedure
   --  returns.  The IOCTL_Failure exception can be raised if the internal
   --  call to ioctl() fails.


   function initialize_history (offset : Int64 := 0) return hammer_ioc_history;
   --  Returns an initialized hammer_ioc_history record that can be passed
   --  to the retrieve history function


   function last_error_message return String;
   --  Print a text description of the last seen error
   --  This is normally called as result of a raised exception that is
   --  caused by an error captured in the system's errno variable


   function open_file_for_reading (path : String) return file_descriptor;
   --  Given a path, this function will open the file for reading and
   --  return a file descriptor, otherwise it will raise an exception


   procedure close_file (descriptor : file_descriptor);
   --  Given a file descriptor to an open file, this function will close
   --  the file.


private

   function strerror (error_number : Integer) return IC.Strings.chars_ptr;
   pragma Import (C, strerror, "strerror");
   --  Get pointer to text description of the last error

   function fd_open (path : IC.Strings.chars_ptr; flags : uInt32)
   return file_descriptor;
   pragma Import (C, fd_open, "open");
   --  Return an open file descriptor given a path

   function fd_close (d : file_descriptor) return Int32;
   pragma Import (C, fd_close, "close");
   --  Close an open file given its descriptor

end DragonFly.HAMMER.Binding;
