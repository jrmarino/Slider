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


package Transactions.Slide is

   procedure launch_slide (path : in String; newpath : in String);
   --  Handle undelete functionality, called by main program

private

   c_cursor  : TIC.Color_Pair;

   type menu_format is (found_binary, found_text, view, saveas);

   procedure start_command_window (path : in String);
   --  Create instruction windows

   procedure start_view_window;
   --  Create view windows

   procedure start_input_window;
   --  Create input window

   procedure show_menu (
      format     : menu_format;
      scrollup   : in Boolean := False;
      scrolldown : in Boolean := False);
   --  Context sensitive menu options

   procedure recreate (origin : in String; destination : in String;
      success : out Boolean);
   --  The "undelete" function

   procedure indicate_success (destination : in String);
   --  return the name of the restored file (as much as can fit on 1 line)

   procedure browse_file (path : in String;
      differential : in Boolean := False);
   --  Handle viewing text file, including diff files

   procedure show_page_count (page : in Positive; total_pages : in Positive);
   --  Add page # of ## footer during view contents functionality

   procedure clear_input_window;
   --  Clear input window without using "clear" function

   function confirm_save_as (destination : in String) return Boolean;
   --  Confirm save location

   procedure pick_version (viewable : in Boolean);
   --  main screen, shows all version

   procedure location (nindex : in Natural; ncolumn : out Natural;
      nrow : out Natural; max_columns : in Positive; total : in Positive);
   --  determine page, column, and row for the version menu

   function number_label (nindex : in Natural) return String;
   --  format the version prefix.

   function set_menu_format (viewable : Boolean) return menu_format;
   --  small format function helper

   procedure view_diff (path : in String; orig, dest : in Positive);
   --  handle diff viewing

   procedure indicate_no_difference (temporary_file : in String;
      older_version, newer_version : in String);
   --  Create a dummy output for diff when files are identifical

end Transactions.Slide;
