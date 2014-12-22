--
--  Copyright (c) 2014 John Marino <draco@marino.st>
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

with GNAT.OS_Lib;

package body DragonFly.HAMMER.Binding is


   --------------------------
   --  initialize_history  --
   --------------------------

   function initialize_history (offset : Int64 := 0)
   return hammer_ioc_history is
      result : hammer_ioc_history := (
                  beg_tid    => HAMMER_MIN_TID,
                  nxt_tid    => 0,
                  end_tid    => HAMMER_MAX_TID,
                  head       => (others => 0),
                  hist_ary   => (others => (0, 0, 0)),
                  count      => 0,
                  reserved01 => 0,
                  others     => 0);
   begin
      if offset > 0 then
         result.key        := offset;
         result.nxt_key    := offset + 1;
         result.head.flags := HAMMER_IOC_HISTORY_ATKEY;
      end if;
      return result;
   end initialize_history;


   ------------------------
   --  retrieve_history  --
   ------------------------

   procedure retrieve_history (
                open_file : in     file_descriptor;
                history   : in out hammer_ioc_history)
   is
      result    : Int32;
   begin
      result := ioctl (
                   descriptor => open_file,
                   command    => HAMMERIOC_GETHISTORY,
                   argument   => history'Address);
      if result < 0 then
         raise IOCTL_Failure with "HAMMERIOC_GETHISTORY failed";
      end if;
   end retrieve_history;


   --------------------------
   --  last_error_message  --
   --------------------------

   function last_error_message return String is
      error_msg_pointer : IC.Strings.chars_ptr;
   begin
      error_msg_pointer := strerror (GNAT.OS_Lib.Errno);
      return IC.Strings.Value (error_msg_pointer);
   end last_error_message;


   ------------------------------
   ---  open_file_for_reading  --
   ------------------------------

   function open_file_for_reading (path : String) return file_descriptor is
      path_ptr : IC.Strings.chars_ptr;
      result   : file_descriptor;
   begin
      path_ptr := IC.Strings.New_String (path);
      result   := fd_open (path_ptr, O_RDONLY);
      IC.Strings.Free (path_ptr);
      if result < 0 then
         raise FILE_Failure with "File open operation failed";
      end if;
      return result;
   end open_file_for_reading;


   ------------------
   --  close_file  --
   ------------------

   procedure close_file (descriptor : file_descriptor) is
      result : Int32;
   begin
      result := fd_close (descriptor);
      if result < 0 then
         raise FILE_Failure with "File close operation failed";
      end if;
   end close_file;

end DragonFly.HAMMER.Binding;
