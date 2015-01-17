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

with DragonFly.HAMMER.Ghosts;

package Transactions.Ghosts is

   package DHG renames DragonFly.HAMMER.Ghosts;

   function scan_directory (path : in String) return String;
   --  Scan a directory for deleted files and directories

private

   file_ghosts : DHG.Filename_Container.Vector;
   dirs_ghosts : DHG.Filename_Container.Vector;

   c_dcursor  : TIC.Color_Pair;
   c_fcursor  : TIC.Color_Pair;

   type entry_type is (directory, file);

   type alacarte is record
      dir_entry : entry_type;
      index     : Positive;
      page      : Positive;
      row       : Natural;
   end record;

   type menudata is array (Positive range <>) of alacarte;

   procedure start_command_window (directory_path : in String);

   procedure start_input_window;

   procedure show_page_count (page : in Positive; total_pages : in Positive);

   procedure clear_input_window;

   procedure start_view_window;

   procedure show_menu (is_directory : Boolean);

   function get_listing return menudata;

   procedure list_deleted_entries (listing : in menudata;
      selection : in Positive);

end Transactions.Ghosts;
