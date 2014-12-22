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


with Interfaces.C.Strings;

package DragonFly.FileStatus is


   package ICS renames Interfaces.C.Strings;


   --------------
   ---  Types  --
   --------------

   subtype Tino    is uInt64;
   subtype Tnlink  is uInt32;
   subtype Tdev    is uInt32;
   subtype Tmode   is uInt16;
   subtype Tuid    is uInt32;
   subtype Tgid    is uInt32;
   subtype Toffset is uInt64;
   subtype Ttime   is uInt64;  --  amd64 only, it's uInt32 on i386
   subtype Tnsec   is uInt64;  --  amd64 only. However i386 is not supported

   type timespec is record
      tv_sec      : Ttime;
      tv_nsec     : Tnsec;
   end record;

   type inode_data is record
      st_ino      : Tino;
      st_nlink    : Tnlink;
      st_dev      : Tdev;
      st_mode     : Tmode;
      st_padding1 : uInt16;
      st_uid      : Tuid;
      st_gid      : Tgid;
      st_rdev     : Tdev;
      st_atim     : timespec;
      st_mtim     : timespec;
      st_ctim     : timespec;
      st_size     : Toffset;
      st_blocks   : Int64;
      st_blksize  : uInt32;
      st_flags    : uInt32;
      st_gen      : uInt32;
      st_lspare   : Int32;
      st_qspare1  : Int32;
      st_qspare2  : Int32;
   end record;


   ------------------
   ---  Constants  --
   ------------------

   S_IFIFO  : constant Tmode :=  8#10000#;  --  named pipe (FIFO)
   S_IFCHR  : constant Tmode :=  8#20000#;  --  special character
   S_IFDIR  : constant Tmode :=  8#40000#;  --  directory
   S_IFBLK  : constant Tmode :=  8#60000#;  --  special block
   S_IFREG  : constant Tmode := 8#100000#;  --  regular
   S_IFDB   : constant Tmode := 8#110000#;  --  record access file
   S_IFLNK  : constant Tmode := 8#120000#;  --  symbolic link
   S_IFSOCK : constant Tmode := 8#140000#;  --  socket
   S_IFWHT  : constant Tmode := 8#160000#;  --  whiteout
   S_IFMT   : constant Tmode := 8#170000#;  --  file mask type
   S_ISVTX  : constant Tmode :=   8#1000#;  --  saved swapped text


   -------------------------
   ---  Import Functions  --
   -------------------------

   function fstat (fd : file_descriptor; sb : inode_data) return Int32;
   pragma Import (C, fstat, "fstat");
   --  polls information about an open file from a given file descriptor
   --  return 0 on success, -1 on failure

   procedure stat (
                path   : in String;
                sb     : in out inode_data;
                result : out Int32);
   --  polls information about a file from a given path (wrapper)
   --  return 0 on success, -1 on failure

private

   function private_stat (path : ICS.chars_ptr; sb : inode_data) return Int32;
   pragma Import (C, private_stat, "stat");
   --  polls information about a file from a given path
   --  return 0 on success, -1 on failure

end DragonFly.FileStatus;
