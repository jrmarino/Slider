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


with DragonFly.HAMMER.Binding;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Directories;

package DragonFly.HAMMER.Ghosts is

   package HB  renames DragonFly.HAMMER.Binding;
   package DIR renames Ada.Directories;

   package Filename_Container is
      new Ada.Containers.Indefinite_Vectors (Positive, String);

   package TID_Container is
      new Ada.Containers.Vectors (Positive, HB.hammer_tid);

   package SortEntry is new Filename_Container.Generic_Sorting;
   package SortTid   is new TID_Container.Generic_Sorting;

   procedure scan_for_file_ghosts (
      directory_path   : in String;
      file_ghosts      : out Filename_Container.Vector;
      directory_ghosts : out Filename_Container.Vector);
   --  Given an existing directory path, the procedure will produce
   --  both a list of deleted directories and a list of files deleted
   --  (but with available version) from that directory


private

   living_files : Filename_Container.Vector;
   living_dirs  : Filename_Container.Vector;

   procedure establish_baseline (directory_path : in String);
   --  Get current listing of ordinary files and directories

   function format_as_hex (bignum : HB.hammer_tid) return String;
   --  Format string with c-equivalent of %016x

   procedure scan_history_of_directory (
      fd             : in file_descriptor;
      directory_path : in String;
      ghosts_file    : in out Filename_Container.Vector;
      ghosts_dir     : in out Filename_Container.Vector);
   --  Look at a specific version of a directory, filtered by type, and
   --  determine which files have never been seen before (previously deleted)

   procedure scan_transaction (
      directory_path : in String;
      transaction    : in String;
      fkind          : in DIR.File_Kind;
      ghost          : in out Filename_Container.Vector);
   --  Scan historical version of directory for either deleted files or
   --  directories, and augment "ghost" container as necessary.

   function directory_has_files (directory_path : String) return Boolean;
   --  if the directory has at least one entry other than "." or "..", this
   --  function will return true.  If the path doesn't exist, it will return
   --  false, so it can check this too.

end DragonFly.HAMMER.Ghosts;
