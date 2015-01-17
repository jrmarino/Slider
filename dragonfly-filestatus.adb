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


package body DragonFly.FileStatus is


   --------------------
   --  stat wrapper  --
   --------------------

   procedure stat (
                path   : in String;
                sb     : in out inode_data;
                result : out Int32)
   is
      path_ptr : ICS.chars_ptr;
   begin
      path_ptr := ICS.New_String (path);
      result   := private_stat (path_ptr, sb);
   end stat;


   -----------------------
   --  symlink wrapper  --
   -----------------------

   function symlink (source, newlink : String) return Boolean
   is
      source_ptr  : ICS.chars_ptr := ICS.New_String (source);
      newlink_ptr : ICS.chars_ptr := ICS.New_String (newlink);
      result      : Int32;
   begin
      result := private_symlink (source_ptr, newlink_ptr);
      return result = 0;
   end symlink;

end DragonFly.FileStatus;
