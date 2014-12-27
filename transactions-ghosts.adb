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


with Ada.Directories;

package body Transactions.Ghosts is

   package DIR renames Ada.Directories;


   ----------------------
   --  scan_directory  --
   ----------------------

   function scan_directory (path : in String) return String
   is
      use type TIC.Column_Position;
      bust    : constant String := "  undetected  ";
      justdir : constant String := DIR.Containing_Directory (path & "/")
                                   & "/";
   begin
      DHG.scan_for_file_ghosts (
         directory_path   => path,
         file_ghosts      => file_ghosts,
         directory_ghosts => dirs_ghosts);

      if dirs_ghosts.Is_Empty and file_ghosts.Is_Empty then
         return bust;
      end if;

      TIC.Init_Screen;
      TIC.Set_Echo_Mode (False);
      TIC.Set_Raw_Mode (True);
      TIC.Set_Cbreak_Mode (True);
      if TIC.Columns > app_width then
         app_width := TIC.Columns;
      end if;
      TIC.Start_Color;
      TIC.Init_Pair (TIC.Color_Pair (1), TIC.Cyan,   TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (2), TIC.White,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (4), TIC.Black,  TIC.White);
      TIC.Init_Pair (TIC.Color_Pair (7), TIC.Yellow, TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (8), TIC.White,  TIC.Blue);
      TIC.Init_Pair (TIC.Color_Pair (9), TIC.Yellow, TIC.Blue);
      c_cyan    := TIC.Color_Pair (1);
      c_white   := TIC.Color_Pair (2);
      c_path    := TIC.Color_Pair (4);
      c_yellow  := TIC.Color_Pair (7);
      c_fcursor := TIC.Color_Pair (8);
      c_dcursor := TIC.Color_Pair (9);

      start_command_window (path);
      start_view_window;
      start_input_window;
      TIC.Set_KeyPad_Mode (Win => inpwindow, SwitchOn => True);

      declare
         listing   : constant menudata := get_listing;
         selection : Positive := 1;
         KeyCode   : TIC.Real_Key_Code;
         maxfiles  : constant Natural := listing'Length;
         vheight   : constant Positive := Positive (viewheight);
         carriage  : constant TIC.Real_Key_Code := 10;
      begin
         loop
            list_deleted_entries (listing, selection);
            show_menu (listing (selection).dir_entry = directory);
            TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
            KeyCode := TIC.Get_Keystroke (inpwindow);
            case KeyCode is
               when TIC.Key_Cursor_Up =>
                  if selection > 1 then
                     selection := selection - 1;
                  else
                     selection := maxfiles;
                  end if;
               when TIC.Key_Cursor_Down =>
                  if selection < maxfiles then
                     selection := selection + 1;
                  else
                     selection := 1;
                  end if;
               when TIC.Key_Cursor_Left =>
                  if vheight < maxfiles then
                     if selection > vheight then
                        selection := selection - vheight;
                     else
                        selection := maxfiles;
                     end if;
                  end if;
               when TIC.Key_Cursor_Right =>
                  if vheight < maxfiles then
                     if selection + vheight > maxfiles then
                        selection := 1;
                     else
                        selection := selection + vheight;
                     end if;
                  end if;
               when TIC.Key_F1 | carriage =>
                  if listing (selection).dir_entry = directory then
                     return justdir &
                        dirs_ghosts.Element (listing (selection).index);
                  else
                     return justdir &
                        file_ghosts.Element (listing (selection).index);
                  end if;
               when TIC.Key_F4 =>
                  clear_input_window;
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end;
      TIC.Delete (inpwindow);
      TIC.Delete (viewport);
      TIC.Delete (comwindow);
      TIC.End_Windows;

      return "";
   end scan_directory;


   ----------------------------
   --  start_command_window  --
   ----------------------------

   procedure start_command_window (directory_path : in String) is
      use type TIC.Column_Count;
      justdir : constant String := DIR.Containing_Directory
                                   (directory_path & "/")  & "/";
      bar     : String (1 .. Integer (app_width)) := (others => ' ');
      maxlen  : constant Positive := Positive (app_width) - 9;
   begin
      comwindow := TIC.Create (
                      Number_Of_Lines       => 2,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => 0,
                      First_Column_Position => 0);

      TIC.Move_Cursor (Win => comwindow, Line => 1, Column => 0);
      TIC.Set_Character_Attributes (Win => comwindow,
         Attr => TIC.Normal_Video, Color => TIC.Color_Pair (c_path));
      bar (bar'First .. bar'First + 6) := "Scanned";
      if justdir'Length > maxlen then
         bar (bar'First + 8 .. maxlen + 8) :=
            justdir (justdir'First .. maxlen);
      else
         bar (bar'First + 8 .. justdir'Length + 8) := justdir;
      end if;
      TIC.Add (Win => comwindow, Str => bar);
   end start_command_window;


   --------------------------
   --  start_input_window  --
   --------------------------

   procedure start_input_window is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
   begin
      inpwindow := TIC.Create (
                      Number_Of_Lines       => 1,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => viewheight + 2,
                      First_Column_Position => 0);
      TIC.Set_Character_Attributes (Win => inpwindow, Attr => bright);
   end start_input_window;


   -----------------------
   --  show_page_count  --
   -----------------------

   procedure show_page_count (page : in Positive; total_pages : in Positive)
   is
      use type TIC.Column_Count;
      whole_line : String (1 .. Natural (app_width)) := (others => ' ');
      info : constant String := "page" & page'Img & " of" & total_pages'Img;
      leftside : constant Positive := Positive (app_width) - info'Length + 1;
   begin
      whole_line (leftside .. whole_line'Last) := info;
      TIC.Set_Color (Win => inpwindow, Pair => c_yellow);
      TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
      TIC.Add (Win => inpwindow, Str => whole_line);
      TIC.Refresh (Win => inpwindow);
   end show_page_count;


   --------------------------
   --  clear_input_window  --
   --------------------------

   procedure clear_input_window is
      blank_line : String (1 .. Natural (app_width)) := (others => ' ');
   begin
      TIC.Erase (Win => inpwindow);
      TIC.Refresh (Win => inpwindow);
   end clear_input_window;


   -------------------------
   --  start_view_window  --
   -------------------------

   procedure start_view_window is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
   begin
      viewheight := TIC.Lines - 3;
      if viewheight < 7 then
         viewheight := 7;
      end if;
      viewport := TIC.Create (
                      Number_Of_Lines       => viewheight,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => 2,
                      First_Column_Position => 0);
   end start_view_window;


   -----------------
   --  show_menu  --
   -----------------

   procedure show_menu (is_directory : Boolean)
   is
      use type TIC.Column_Position;
      blank    : String (1 .. Integer (app_width)) := (others => ' ');
   begin
      TIC.Set_Character_Attributes (Win => comwindow, Attr => bright);
      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);
      TIC.Add (Win => comwindow, Str => blank);
      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);
      TIC.Set_Color (Win => comwindow, Pair => c_white);
      TIC.Add (Win => comwindow, Str => "Arrows: ");
      TIC.Set_Color (Win => comwindow, Pair => c_cyan);
      TIC.Add (Win => comwindow, Str => "move   ");
      TIC.Set_Color (Win => comwindow, Pair => c_white);
      TIC.Add (Win => comwindow, Str => "F1: ");
      TIC.Set_Color (Win => comwindow, Pair => c_cyan);
      if is_directory then
         TIC.Add (Win => comwindow, Str => "Select directory to resurrect");
      else
         TIC.Add (Win => comwindow, Str => "Select file to undelete");
      end if;

      TIC.Move_Cursor (Win => comwindow, Line => 0,
         Column => app_width - 8);

      TIC.Set_Color (Win => comwindow, Pair => c_white);
      TIC.Add (Win => comwindow, Str => "F4: ");
      TIC.Set_Color (Win => comwindow, Pair => c_cyan);
      TIC.Add (Win => comwindow, Str => "Quit");
      TIC.Refresh (Win => comwindow);

   end show_menu;


   -------------------
   --  get_listing  --
   -------------------

   function get_listing return menudata
   is
      total   : constant Natural := Natural (dirs_ghosts.Length) +
                   Natural (file_ghosts.Length);
      result  : menudata (1 .. total);
      page    : Positive := 1;
      counter : Natural := 0;
      row     : Natural := 0;
   begin
      for n in 1 .. Natural (dirs_ghosts.Length) loop
         counter := counter + 1;
         if row = Natural (viewheight) then
            page := page + 1;
            row  := 0;
         end if;
         result (counter).dir_entry := directory;
         result (counter).index := n;
         result (counter).page := page;
         result (counter).row  := row;
         row := row + 1;
      end loop;
      for n in 1 .. Natural (file_ghosts.Length) loop
         counter := counter + 1;
         if row = Natural (viewheight) then
            page := page + 1;
            row  := 0;
         end if;
         result (counter).dir_entry := file;
         result (counter).index := n;
         result (counter).page := page;
         result (counter).row  := row;
         row := row + 1;
      end loop;
      return result;
   end get_listing;


   ----------------------------
   --  list_deleted_entries  --
   ----------------------------

   procedure list_deleted_entries (listing : in menudata;
      selection : in Positive)
   is
      total : constant Natural := listing'Length;
      page  : constant Positive := 1 +
                                   ((selection - 1) / Positive (viewheight));
      pages : constant Positive := 1 + (total / Positive (viewheight));
      head  : constant Positive := 1 + Positive (viewheight) * (page - 1);
      tail  : Positive := Positive (viewheight) * page;
   begin
      if tail > total then
         tail := total;
      end if;
      TIC.Erase (Win => viewport);
      for x in head .. tail loop
         TIC.Move_Cursor (Win    => viewport,
                          Column => 0,
                          Line   => TIC.Line_Position (listing (x).row));
         if listing (x).dir_entry = directory then
            if selection = x then
               TIC.Set_Character_Attributes (Win => viewport, Attr => bright,
                  Color => c_dcursor);
            else
               TIC.Set_Character_Attributes (Win => viewport,
                  Attr => TIC.Normal_Video, Color => c_cyan);
            end if;
            TIC.Add (Win => viewport, Str => dirs_ghosts (listing (x).index));
         else
            if selection = x then
               TIC.Set_Character_Attributes (Win => viewport,
                  Attr => TIC.Normal_Video, Color => c_fcursor);
            else
               TIC.Set_Character_Attributes (Win => viewport,
                  Attr => TIC.Normal_Video, Color => c_white);
            end if;
            TIC.Add (Win => viewport, Str => file_ghosts (listing (x).index));
         end if;
      end loop;
      TIC.Refresh (Win => viewport);
      show_page_count (page, pages);
   end list_deleted_entries;

end Transactions.Ghosts;
