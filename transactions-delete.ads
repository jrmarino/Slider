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


with Ada.Directories;
with Ada.Containers.Generic_Array_Sort;

package Transactions.Delete is

   package DIR renames Ada.Directories;

   procedure launch_deleted (path : in String; newpath : in String);
   --  Handle undelete functionality, called by main program (file)

   procedure launch_deleted_directory (
      path      : in String;
      cleanpath : in String;
      newpath   : in String);
   --  Handle undelete functionality, called by main program (directory)

private

   type menu_format is (found_binary, found_text, view, saveas);

   type listing_entry is record
      filetype : DIR.File_Kind;
      filename : String (1 .. 255);
      filesize : DIR.File_Size;
      modtime  : DHH.TraxTime;
   end record;

   type listing_array is array (Natural range <>) of listing_entry;

   function "<" (L, R : listing_entry) return Boolean;
   --  Define how to sort listing_entry

   procedure Sort is new Ada.Containers.Generic_Array_Sort
      (Natural, listing_entry, listing_array);
   --  Templated sort procedure for listing_arrays

   function retrieve_directory_listing (directory_path : in String)
      return listing_array;
   --  return a sorted array of directory contents records

   procedure start_command_window (path : in String);
   --  Create instruction windows

   procedure start_view_window;
   --  Create view windows

   procedure start_input_window;
   --  Create input window

   procedure show_menu (
      format : menu_format;
      scrollup : in Boolean := False;
      scrolldown : in Boolean := False);
   --  Context sensitive menu options

   procedure show_version (viewable : in Boolean);
   --  Show deleted version

   procedure show_directory_version (path : in String);
   --  Show deleted version of given directory path

   procedure recreate (origin : in String; destination : in String;
      success : out Boolean);
   --  The "undelete" function

   procedure indicate_success (destination : in String);
   --  return the name of the restored file (as much as can fit on 1 line)

   procedure show_page_count (page : in Positive; total_pages : in Positive);
   --  Add page # of ## footer during view contents functionality

   procedure clear_input_window;
   --  Clear input window without using "clear" function

   function confirm_save_as (destination : in String) return Boolean;
   --  Confirm save location

   procedure browse_file (path : in String);
   --  Handle viewing text file

   procedure duplicate_directory (
      origin : in String;
      destination : in String;
      success : out Boolean);
   --  copy a deleted directory to a new location

   procedure error_message (message : in String);
   --  Generic error message handler

   procedure browse_directory (directory_path : in String);
   --  Routine to browse directory in color and by pages

   function human_readable_size (filesize : DIR.File_Size) return String;
   --  Returns 5-character size using K, M, G as necessary

end Transactions.Delete;
