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


with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Direct_IO;
with Ada.Directories;
with GNAT.OS_Lib;

package body Transactions.Slide is

   package TIO renames Ada.Text_IO;
   package TTS renames Ada.Text_IO.Text_Streams;
   package DIR renames Ada.Directories;
   package GOS renames GNAT.OS_Lib;

   procedure launch_slide (path : in String; newpath : in String)
   is
      use type TIC.Column_Position;
      viewable : Boolean;
      onpage   : Positive := 1;
   begin
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
      TIC.Init_Pair (TIC.Color_Pair (3), TIC.Black,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (4), TIC.Black,  TIC.White);
      TIC.Init_Pair (TIC.Color_Pair (5), TIC.Green,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (6), TIC.Red,    TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (7), TIC.Yellow, TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (8), TIC.White,  TIC.Blue);
      c_cyan   := TIC.Color_Pair (1);
      c_white  := TIC.Color_Pair (2);
      c_black  := TIC.Color_Pair (3);
      c_path   := TIC.Color_Pair (4);
      c_green  := TIC.Color_Pair (5);
      c_red    := TIC.Color_Pair (6);
      c_yellow := TIC.Color_Pair (7);
      c_cursor := TIC.Color_Pair (8);

      start_command_window (path);
      start_view_window;
      start_input_window;
      TIC.Set_KeyPad_Mode (Win => inpwindow, SwitchOn => True);

      declare
         KeyCode : TIC.Real_Key_Code;
         maxhist : constant Positive := Positive (ScanData.history.Length);
         zero    : constant TIC.Key_Code := Character'Pos ('0');
         fdiff   : constant TIC.Key_Code := Character'Pos ('>');
         rdiff   : constant TIC.Key_Code := Character'Pos ('<');
         block   : constant Positive := Positive (viewheight) - 1;
         origin  : String := path & ScanData.history (maxhist).trax_id;
         mformat : menu_format;
      begin
         ScanData.diff_old := maxhist;
         ScanData.diff_new := maxhist - 1;
         viewable := textfile (origin);
         mformat  := set_menu_format (viewable);
         show_menu (format => mformat);
         pick_version (viewable);
         loop
            KeyCode := TIC.Get_Keystroke (inpwindow);
            case KeyCode is
               when TIC.Key_Cursor_Up =>
                  --  history is stored oldest to newests, so reverse logic
                  if ScanData.diff_new < maxhist then
                     ScanData.diff_new := ScanData.diff_new + 1;
                  else
                     ScanData.diff_new := 1;
                  end if;
                  show_menu (format => mformat);
                  pick_version (viewable);
               when TIC.Key_Cursor_Down =>
                  --  history is stored oldest to newests, so reverse logic
                  if ScanData.diff_new > 1 then
                     ScanData.diff_new := ScanData.diff_new - 1;
                  else
                     ScanData.diff_new := maxhist;
                  end if;
                  show_menu (format => mformat);
                  pick_version (viewable);
               when TIC.Key_Cursor_Left =>
                  --  history is stored oldest to newests, so reverse logic
                  if ScanData.diff_new <= maxhist - block then
                     ScanData.diff_new := ScanData.diff_new + block;
                     show_menu (format => mformat);
                     pick_version (viewable);
                  end if;
               when TIC.Key_Cursor_Right =>
                  --  history is stored oldest to newests, so reverse logic
                  if ScanData.diff_new > block then
                     ScanData.diff_new := ScanData.diff_new - block;
                     show_menu (format => mformat);
                     pick_version (viewable);
                  end if;
               when zero =>
                  if ScanData.diff_old /= ScanData.diff_new then
                     ScanData.diff_old := ScanData.diff_new;
                     origin := path &
                                  ScanData.history (ScanData.diff_old).trax_id;
                     viewable := textfile (origin);
                     mformat  := set_menu_format (viewable);
                     show_menu (format => mformat);
                     pick_version (viewable);
                  end if;
               when fdiff =>
                  if viewable and ScanData.diff_old /= ScanData.diff_new then
                     view_diff (path, ScanData.diff_old, ScanData.diff_new);
                     show_menu (format => mformat);
                     pick_version (viewable);
                     clear_input_window;
                  end if;
               when rdiff =>
                  if viewable and ScanData.diff_old /= ScanData.diff_new then
                     view_diff (path, ScanData.diff_new, ScanData.diff_old);
                     show_menu (format => mformat);
                     pick_version (viewable);
                     clear_input_window;
                  end if;
               when TIC.Key_F1 =>
                  if viewable then
                     declare
                        seepath : constant String := path & ScanData.history
                                     (ScanData.diff_new).trax_id;
                     begin
                        browse_file (seepath);
                     end;
                     show_menu (format => mformat);
                     pick_version (viewable);
                     clear_input_window;
                  end if;
               when TIC.Key_F2 =>
                  if ScanData.diff_old /= ScanData.diff_new then
                     show_menu (format => saveas);
                     declare
                        confirmed : Boolean;
                        success   : Boolean;
                        seepath : constant String := path & ScanData.history
                                     (ScanData.diff_new).trax_id;
                     begin
                        confirmed := confirm_save_as (path);
                        if confirmed then
                           recreate (seepath, path, success);
                           if success then
                              indicate_success (path);
                              clear_input_window;
                              exit;
                           end if;
                        end if;
                        show_menu (format => mformat);
                        pick_version (viewable);
                        clear_input_window;
                     end;
                  end if;
               when TIC.Key_F3 =>
                  show_menu (format => saveas);
                  declare
                     confirmed : Boolean;
                     success   : Boolean;
                     seepath   : constant String := path & ScanData.history
                                    (ScanData.diff_new).trax_id;
                  begin
                     confirmed := confirm_save_as (newpath);
                     if confirmed then
                        recreate (seepath, newpath, success);
                        if success then
                           indicate_success (newpath);
                           clear_input_window;
                           exit;
                        end if;
                     end if;
                     show_menu (format => mformat);
                     pick_version (viewable);
                     clear_input_window;
                  end;

               when TIC.Key_F4 =>
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

   end launch_slide;


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
                      First_Line_Position   => viewheight + 3,
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


   ----------------------------
   --  start_command_window  --
   ----------------------------

   procedure start_command_window (path : in String) is
      use type TIC.Column_Count;
      bar : String (1 .. Integer (app_width)) := (others => ' ');
   begin
      comwindow := TIC.Create (
                      Number_Of_Lines       => 3,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => 0,
                      First_Column_Position => 0);

      TIC.Move_Cursor (Win => comwindow, Line => 2, Column => 0);
      TIC.Set_Character_Attributes (Win => comwindow,
         Attr => TIC.Normal_Video, Color => TIC.Color_Pair (c_path));
      if path'Length < bar'Length then
         bar (bar'First .. path'Length) := path;
      else
         declare
            sindex : constant Positive :=
               path'Length - Integer (app_width) - 6;
         begin
            bar (bar'First + 1 .. bar'Last - 1) := "... " &
               path (sindex .. path'Last);
         end;
      end if;
      TIC.Add (Win => comwindow, Str => bar);
   end start_command_window;


   -------------------------
   --  start_view_window  --
   -------------------------

   procedure start_view_window is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
   begin
      viewheight := TIC.Lines - 4;
      if viewheight < 7 then
         viewheight := 7;
      end if;
      viewport := TIC.Create (
                      Number_Of_Lines       => viewheight,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => 3,
                      First_Column_Position => 0);
   end start_view_window;


   -----------------
   --  show_menu  --
   -----------------

   procedure show_menu (
      format     : menu_format;
      scrollup   : in Boolean := False;
      scrolldown : in Boolean := False)
   is
      use type TIC.Column_Position;
      blank    : String (1 .. Integer (app_width)) := (others => ' ');
      topmenu  : constant Boolean := format = found_text or
                                     format = found_binary;
      samefile : constant Boolean := ScanData.diff_old = ScanData.diff_new;
   begin
      TIC.Set_Character_Attributes (Win => comwindow, Attr => bright);
      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);
      TIC.Add (Win => comwindow, Str => blank);
      TIC.Move_Cursor (Win => comwindow, Line => 1, Column => 0);
      TIC.Add (Win => comwindow, Str => blank);
      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);
      if topmenu then
         TIC.Set_Color (Win => comwindow, Pair => c_white);
         TIC.Add (Win => comwindow, Str => "Arrows: ");
         TIC.Set_Color (Win => comwindow, Pair => c_cyan);
         TIC.Add (Win => comwindow, Str => "move   ");
         if format = found_text then
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => "F1: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "View contents     ");
         end if;
         if format = found_binary then
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "F1: View contents     ");
         end if;
         if samefile then
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "F2: Replace  ");
         else
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => "F2: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "Replace  ");
         end if;
         TIC.Set_Color (Win => comwindow, Pair => c_white);
         TIC.Add (Win => comwindow, Str => "F3: ");
         TIC.Set_Color (Win => comwindow, Pair => c_cyan);
         TIC.Add (Win => comwindow, Str => "Save As  ");
      end if;
      if format = view then
         if scrollup then
            TIC.Add (Win => comwindow, Str => "Up Arrow: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "view prev page  ");
         else
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "Up Arrow: view prev page  ");
         end if;
         if scrolldown then
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => "Down Arrow: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "view next page  ");
         else
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "Down Arrow: view next page  ");
         end if;
      end if;
      if format = saveas then
         TIC.Set_Color (Win => comwindow, Pair => c_white);
         TIC.Add (Win => comwindow, Str => "F1: ");
         TIC.Set_Color (Win => comwindow, Pair => c_cyan);
         TIC.Add (Win => comwindow, Str => "Confirm file save ");
      end if;
      TIC.Move_Cursor (Win => comwindow, Line => 0,
         Column => app_width - 8);

      TIC.Set_Color (Win => comwindow, Pair => c_white);
      TIC.Add (Win => comwindow, Str => "F4: ");
      TIC.Set_Color (Win => comwindow, Pair => c_cyan);
      if topmenu then
         TIC.Add (Win => comwindow, Str => "Quit");
      else
         TIC.Add (Win => comwindow, Str => "Back");
      end if;
      if topmenu then
         TIC.Move_Cursor (Win => comwindow, Line => 1, Column => 0);
         if samefile then
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "0: mark origin  ");
         else
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => "0: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "mark origin  ");
         end if;
         if not samefile and format = found_text then
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => "<: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "view reverse diff  ");
            TIC.Set_Color (Win => comwindow, Pair => c_white);
            TIC.Add (Win => comwindow, Str => ">: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "view diff from origin");
         end if;
         if samefile or format = found_binary then
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str =>
               "<: view reverse diff  >: view diff from origin");
         end if;
      end if;
      TIC.Refresh (Win => comwindow);

   end show_menu;


   --------------------
   --  number_label  --
   --------------------

   function number_label (nindex : in Natural) return String is
      Raw_Image : constant String := Integer'Image (nindex);
      result    : String := " 000) ";
   begin
      if nindex < 1000 then
         if nindex < 10 then
            result (4) := Raw_Image (2);
         elsif nindex < 100 then
            result (3 .. 4) := Raw_Image (2 .. 3);
         else
            result (2 .. 4) := Raw_Image (2 .. 4);
         end if;
      end if;
      return result;
   end number_label;


   ----------------
   --  location  --
   ----------------

   procedure location (nindex : in Natural; ncolumn : out Natural;
      nrow : out Natural; max_columns : in Positive; total : in Positive)
   is
      rowoffset : constant Natural := 1;
      height    : constant Positive := Positive (viewheight) - rowoffset;
      pagesize  : constant Positive := max_columns * height;
      modpage   : Natural;
      page      : Natural;
      usedcols  : Positive;
      coloffset : Positive;
   begin
      modpage   := nindex mod pagesize;
      page      := (nindex / pagesize) + 1;
      declare
         tpages : constant Positive := ((total - 1) / pagesize) + 1;
         lpage  : constant Natural  := ((total - 1) mod pagesize);
      begin
         if page < tpages then
            usedcols  := max_columns;
         else
            usedcols  := (lpage / height) + 1;
         end if;
      end;

      coloffset := (Positive (app_width) - (26 * usedcols)) / 2;
      ncolumn   := coloffset + ((modpage / height) * 26);
      nrow      := (modpage mod height) + rowoffset;
   end location;

   --------------------
   --  pick_version  --
   --------------------

   procedure pick_version (viewable : in Boolean)
   is
      maxCol  : Positive := 1;
      maxhist : constant Positive := Positive (ScanData.history.Length);
      ncolumn : array (1 .. maxhist) of Natural;
      nrow    : array (1 .. maxhist) of Natural;
      onpage  : Positive;
      perPage : Positive;

      procedure list_page (page : in Positive);

      procedure list_page (page : in Positive) is
         head    : Positive := ((page - 1) * perPage) + 1;
         tail    : Positive := (page * perPage);
         n       : Positive;
      begin
         if tail > maxhist then
            tail := maxhist;
         end if;
         for nn in head .. tail loop
            n := maxhist + 1 - nn;
            TIC.Move_Cursor (Win => viewport,
               Line => TIC.Line_Position (nrow (n)),
               Column => TIC.Column_Position (ncolumn (n)));
            if ScanData.diff_old = n then
               TIC.Set_Character_Attributes (Win => viewport, Attr => bright);
            else
               TIC.Set_Character_Attributes (Win => viewport,
                  Attr => TIC.Normal_Video);
            end if;
            TIC.Set_Color (Win => viewport, Pair => c_yellow);
            TIC.Add (Win => viewport, Str => number_label (nn - 1));
            if ScanData.diff_new = n then
               TIC.Set_Color (Win => viewport, Pair => c_cursor);
            else
               TIC.Set_Color (Win => viewport, Pair => c_white);
            end if;
            TIC.Add (Win => viewport, Str => ScanData.history (n).timestamp);
         end loop;
      end list_page;

   begin
      maxCol  := Positive (app_width) / 26;
      perPage := maxCol * (Positive (viewheight) - 1);
      onpage  := ((maxhist - ScanData.diff_new) / perPage) + 1;

      for j in 1 .. maxhist loop
         location (nindex => maxhist - j, nrow => nrow (j),
            ncolumn => ncolumn (j), max_columns => maxCol, total => maxhist);
      end loop;

      TIC.Erase (Win => viewport);
      list_page (onpage);
      TIC.Refresh (Win => viewport);
      TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);

   end pick_version;


   -----------------------
   --  set_menu_format  --
   -----------------------

   function set_menu_format (viewable : Boolean) return menu_format is
      result : menu_format := found_binary;
   begin
      if viewable then
         result := found_text;
      end if;
      return result;
   end set_menu_format;


   -------------------
   --  browse_file  --
   -------------------

   procedure browse_file (path : in String;
      differential : in Boolean := False)
   is
      File_Size : Natural := Natural (Ada.Directories.Size (path));
      max_pages : constant Positive := 100;
      max_size  : constant Positive := Positive (app_width) *
                           Positive (viewheight) * max_pages;
      offsets   : array (1 .. max_pages) of Positive := (others => 1);
      truncated : Boolean := False;
      cutmsg    : constant String := ASCII.LF & "--- View Truncated ---";

      procedure start_view_mode (File_Size : in Natural;
                   truncated : in Boolean);

      procedure start_view_mode (File_Size : in Natural;
                   truncated : in Boolean)
      is
         use type TIC.Real_Key_Code;
         subtype File_String    is String (1 .. File_Size);
         package File_String_IO is new Ada.Direct_IO (File_String);
         package SIO renames File_String_IO;

         KeyCode  : TIC.Real_Key_Code;
         File     : File_String_IO.File_Type;
         Contents : File_String;
         numCR    : Natural := 0;
         page     : Natural := 1;
         lastpage : Natural := 1;
         lastseen : Natural := 0;
      begin
         SIO.Open  (File, Mode => SIO.In_File, Name => path);
         SIO.Read  (File, Item => Contents);
         SIO.Close (File);
         if truncated then
            Contents (Contents'Last - cutmsg'Last + 1 .. Contents'Last) :=
               cutmsg;
         end if;
         for j in 1 .. File_Size loop
            case Contents (j) is
               when ASCII.NUL .. ASCII.BS | ASCII.VT .. ASCII.US => null;
               when ASCII.LF =>
                  numCR := numCR + 1;
                  if numCR = Natural (viewheight) then
                     numCR := 0;
                     page := page + 1;
                     if page > max_pages then
                        exit;
                     end if;
                     if j /= File_Size then
                        offsets (page) := j + 1;
                        lastpage := page;
                     end if;
                  end if;
               when others => null;
            end case;
         end loop;
         page := 1;
         TIC.Set_Color (Win => viewport, Pair => c_white);
         loop
            if page /= lastseen then
               show_menu (
                  format     => view,
                  scrollup   => page > 1,
                  scrolldown => page < lastpage);
               declare
                  use type TIC.Line_Position;
                  subtype blankStr is String (1 .. Positive (app_width));
                  marker : Natural := offsets (page);
                  mline  : TIC.Line_Position := 0;
                  mcol   : Natural := 1;
                  matrix : array (0 .. viewheight - 1) of blankStr :=
                           (others => (others => ' '));
               begin
                  loop
                     exit when marker > File_Size;
                     case Contents (marker) is
                        when ASCII.NUL .. ASCII.BS | ASCII.VT .. ASCII.US =>
                           null;
                        when ASCII.LF =>
                           mline := mline + 1;
                           mcol := 1;
                        when others =>
                           if mcol <= Natural (app_width) then
                              matrix (mline) (mcol) := Contents (marker);
                              mcol := mcol + 1;
                           end if;
                     end case;
                     exit when mline = viewheight;
                     marker := marker + 1;
                  end loop;
                  for j in 0 .. viewheight - 2 loop
                     TIC.Move_Cursor (Win => viewport, Line => j, Column => 0);
                     TIC.Add (Win => viewport, Str => matrix (j));
                  end loop;
                  declare
                     use type TIC.Line_Position;
                     use type TIC.Column_Position;
                     lastline : TIC.Line_Position := viewheight - 1;
                  begin
                     for x in 1 .. app_width loop
                        TIC.Add (Win    => viewport,
                                 Line   => lastline,
                                 Column => x - 1,
                                 Ch     => matrix (lastline)(Positive (x)));
                     end loop;
                  end;
               end;

               if differential then
                  declare
                     use type TIC.Line_Position;
                     FirstLine : TIC.Line_Position := 0;
                     peekchar  : TIC.Attributed_Character;
                     len       : Integer := Integer (app_width);
                  begin
                     if page = 1 then
                        for line in TIC.Line_Position range 0 .. 1 loop
                           TIC.Change_Attributes (Win => viewport,
                              Attr => bright, Color => c_white, Line => line,
                              Column => 0, Count => len);
                        end loop;
                        FirstLine := 2;
                     end if;
                     for line in FirstLine .. viewheight - 1 loop
                        peekchar := TIC.Peek (Win => viewport,
                           Line => line, Column => 0);
                        if peekchar.Ch = '+' then
                           TIC.Change_Attributes (Win => viewport,
                              Attr => TIC.Normal_Video, Color => c_green,
                              Line => line, Column => 0, Count => len);
                        elsif peekchar.Ch = '-' then
                           TIC.Change_Attributes (Win => viewport,
                              Attr => TIC.Normal_Video, Color => c_red,
                              Line => line, Column => 0, Count => len);
                        end if;
                     end loop;
                  end;
               end if;

               show_page_count (page, lastpage);
               TIC.Refresh (Win => viewport);
               lastseen := page;
            end if;
            KeyCode := TIC.Get_Keystroke (inpwindow);
            case KeyCode is
               when TIC.Key_Cursor_Up =>
                  if page > 1 then
                     page := page - 1;
                  end if;
               when TIC.Key_Cursor_Down =>
                  if page < lastpage then
                     page := page + 1;
                  end if;
               when TIC.Key_F4 => exit;
               when others => null;
            end case;
         end loop;

      end start_view_mode;

   begin
      if File_Size > max_size then
         File_Size := max_size;
         truncated := True;
      end if;
      start_view_mode (File_Size, truncated);

   end browse_file;


   ------------------------------
   --  indicate_no_difference  --
   ------------------------------

   procedure indicate_no_difference (temporary_file : in String;
      older_version, newer_version : in String)
   is
      tmpfile : TIO.File_Type;
   begin
      TIO.Open (File => tmpfile,
                  Mode => TIO.Append_File,
                  Name => temporary_file);
      TIO.Put_Line (tmpfile, "--- " & older_version);
      TIO.Put_Line (tmpfile, "+++ " & newer_version);
      TIO.New_Line (tmpfile);
      TIO.Put_Line (tmpfile, "    There is no difference between these " &
                             "two versions of the file.");
      TIO.Close (tmpfile);
   exception
      when TIO.End_Error =>
         if TIO.Is_Open (tmpfile) then
            TIO.Close (tmpfile);
         end if;
   end indicate_no_difference;


   -----------------
   --  view_diff  --
   -----------------

   procedure view_diff (path : in String; orig, dest : in Positive) is
      RC      : Integer;
      arg1    : constant String := "-upa";
      arg2    : constant String := path & ScanData.history (orig).trax_id;
      arg3    : constant String := path & ScanData.history (dest).trax_id;
      Args    : GOS.Argument_List := (
                   new String'(arg1),
                   new String'(arg2),
                   new String'(arg3));
      tmpfile : constant String := "/tmp/slider-" &
                   ScanData.history (orig).trax_id & "-" &
                   ScanData.history (dest).trax_id;
      success : Boolean;
   begin
      GOS.Spawn (Program_Name => "/usr/bin/diff",
                 Args         => Args,
                 Output_File  => tmpfile,
                 Return_Code  => RC,
                 Success      => success);
      for Index in Args'Range loop
         GOS.Free (Args (Index));
      end loop;
      if success then
         declare
            File_Size : Natural := Natural (Ada.Directories.Size (tmpfile));
         begin
            if File_Size = 0 then
               indicate_no_difference (temporary_file => tmpfile,
               older_version => arg2, newer_version => arg3);
            end if;
         end;

         browse_file (path => tmpfile, differential => True);
      end if;
      if DIR.Exists (tmpfile) then
         DIR.Delete_File (tmpfile);
      end if;
   end view_diff;


   -----------------------
   --  confirm_save_as  --
   -----------------------

   function confirm_save_as (destination : in String) return Boolean is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      msg1     : constant String := "Please confirm file should be " &
                    "restored to";
      msg2     : constant String := "Destination directory does not exist " &
                    "so restore is not possible:";
      row1     : constant TIC.Line_Position := (viewheight / 2) - 1;
      row2     : constant TIC.Line_Position := row1 + 1;
      leftside : TIC.Column_Count;
      sindex   : Positive;
      result   : Boolean := False;
      KeyCode  : TIC.Real_Key_Code;
      valid    : Boolean;
   begin
      TIC.Erase (Win => viewport);
      TIC.Set_Color (Win => viewport, Pair => c_yellow);
      declare
         dirname : constant String := DIR.Containing_Directory (destination);
      begin
         valid := DIR.Exists (dirname);
      end;
      if valid then
         leftside := (app_width - msg1'Length) / 2;
         TIC.Move_Cursor (Win => viewport, Line => row1, Column => leftside);
         TIC.Add (Win => viewport, Str => msg1);
      else
         leftside := (app_width - msg2'Length) / 2;
         TIC.Move_Cursor (Win => viewport, Line => row1, Column => leftside);
         TIC.Add (Win => viewport, Str => msg2);
         TIC.Change_Attributes (Win    => comwindow,
                                Attr   => bright,
                                Color  => c_black,
                                Line   => 0,
                                Column => 0,
                                Count  => 30);
         TIC.Refresh (Win => comwindow);
      end if;

      TIC.Set_Color (Win => viewport, Pair => c_white);
      if destination'Length < app_width then
         leftside := (app_width - destination'Length) / 2;
         TIC.Move_Cursor (Win => viewport, Line => row2, Column => leftside);
         TIC.Add (Win => viewport, Str => destination);
      else
         leftside := 1;
         sindex := destination'Length - (Positive (app_width) - 6);
         TIC.Move_Cursor (Win => viewport, Line => row2, Column => leftside);
         TIC.Add (Win => viewport, Str => "... " &
            destination (sindex .. destination'Last));
      end if;
      TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
      TIC.Refresh (Win => viewport);
      loop
         KeyCode := TIC.Get_Keystroke (inpwindow);
         case KeyCode is
            when TIC.Key_F1 =>
               if valid then
                  result := True;
                  exit;
               end if;
            when TIC.Key_F4 => exit;
            when others     => null;
         end case;
      end loop;
      return result;
   end confirm_save_as;


   ----------------
   --  recreate  --
   ----------------

   procedure recreate (origin : in String; destination : in String;
      success : out Boolean) is

      procedure error_message (message : in String);

      procedure error_message (message : in String) is
         msg : String (1 .. Integer (app_width) - 1) := (others => ' ');
      begin
         msg (msg'First .. message'Last) := message;
         TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
         TIC.Set_Color (Win => inpwindow, Pair => c_red);
         TIC.Add (Win => inpwindow, Str => msg);
         TIC.Refresh (Win => inpwindow);
      end error_message;

   begin
      success := False;
      DIR.Copy_File (
         Source_Name => origin,
         Target_Name => destination,
         Form        => "mode=overwrite");
      success := True;
   exception

      when TIO.Name_Error =>
         error_message ("FAILED! The target file name is invalid");

      when TIO.Use_Error =>
         error_message ("FAILED! You do not have permission to " &
                        "create the destination file.");
   end recreate;


   ------------------------
   --  indicate_success  --
   ------------------------

   procedure indicate_success (destination : in String) is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      msg1     : constant String := "Successfully restored file to";
      row1     : constant TIC.Line_Position := (viewheight / 2) - 1;
      row2     : constant TIC.Line_Position := row1 + 1;
      leftside : TIC.Column_Count;
      sindex   : Positive;
   begin
      TIC.Erase (viewport);
      leftside := (app_width - msg1'Length) / 2;
      TIC.Set_Character_Attributes (Win => viewport, Attr => bright,
         Color => TIC.Color_Pair (c_yellow));
      TIC.Move_Cursor (Win => viewport, Line => row1, Column => leftside);
      TIC.Add (Win => viewport, Str => msg1);
      TIC.Set_Character_Attributes (Win => viewport,
         Attr => TIC.Normal_Video, Color => TIC.Color_Pair (c_white));
      if destination'Length < app_width then
         leftside := (app_width - destination'Length) / 2;
         TIC.Move_Cursor (Win => viewport, Line => row2, Column => leftside);
         TIC.Add (Win => viewport, Str => destination);
      else
         leftside := 1;
         sindex := destination'Length - (Positive (app_width) - 6);
         TIC.Move_Cursor (Win => viewport, Line => row2, Column => leftside);
         TIC.Add (Win => viewport, Str => "... " &
            destination (sindex .. destination'Last));
      end if;
      TIC.Refresh (Win => viewport);
   end indicate_success;

end Transactions.Slide;
