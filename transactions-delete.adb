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
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;

package body Transactions.Delete is

   package TIO renames Ada.Text_IO;
   package TTS renames Ada.Text_IO.Text_Streams;
   package GOS renames GNAT.OS_Lib;


   ---------------------
   -- launch_deleted  --
   ---------------------

   procedure launch_deleted (path : in String; newpath : in String)
   is
      use type TIC.Column_Count;
      use type TIC.Real_Key_Code;
      viewable : Boolean;
      KeyCode  : TIC.Real_Key_Code;
      origin   : constant String := path & ScanData.history (1).trax_id;
      success  : Boolean;
      mformat  : menu_format := found_binary;
   begin
      viewable := textfile (origin);
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
      c_cyan   := TIC.Color_Pair (1);
      c_white  := TIC.Color_Pair (2);
      c_black  := TIC.Color_Pair (3);
      c_path   := TIC.Color_Pair (4);
      c_green  := TIC.Color_Pair (5);
      c_red    := TIC.Color_Pair (6);
      c_yellow := TIC.Color_Pair (7);

      start_command_window (path);
      start_view_window;

      if viewable then
         mformat := found_text;
      end if;
      show_menu (format => mformat);
      show_version (viewable => viewable);
      start_input_window;
      TIC.Set_KeyPad_Mode (Win => inpwindow, SwitchOn => True);
      loop
         KeyCode := TIC.Get_Keystroke (inpwindow);
         clear_input_window;
         case KeyCode is
            when TIC.Key_F1 =>
               browse_file (origin);
               show_menu (format => mformat);
               show_version (viewable => viewable);
            when TIC.Key_F2 =>
               recreate (origin, path, success);
               if success then
                  indicate_success (path);
                  exit;
               end if;
            when TIC.Key_F3 =>
               show_menu (format => saveas);
               declare
                  confirmed : Boolean;
               begin
                  confirmed := confirm_save_as (newpath);
                  show_menu (format => mformat);
                  show_version (viewable => viewable);
                  if confirmed then
                     recreate (origin, newpath, success);
                     if success then
                        indicate_success (newpath);
                        exit;
                     end if;
                  end if;
               end;
            when TIC.Key_F4 => exit;
            when others     => null;
         end case;
      end loop;
      TIC.Delete (inpwindow);
      TIC.Delete (viewport);
      TIC.Delete (comwindow);
      TIC.End_Windows;
   end launch_deleted;


   -------------------------------
   -- launch_deleted_directory  --
   -------------------------------

   procedure launch_deleted_directory (
      path      : in String;
      cleanpath : in String;
      newpath   : in String)
   is
      use type TIC.Column_Count;
      use type TIC.Real_Key_Code;
      mformat  : menu_format := found_text;
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
      TIC.Init_Pair (TIC.Color_Pair (10), TIC.Magenta, TIC.Black);
      c_cyan    := TIC.Color_Pair (1);
      c_white   := TIC.Color_Pair (2);
      c_black   := TIC.Color_Pair (3);
      c_path    := TIC.Color_Pair (4);
      c_green   := TIC.Color_Pair (5);
      c_red     := TIC.Color_Pair (6);
      c_yellow  := TIC.Color_Pair (7);
      c_magenta := TIC.Color_Pair (10);

      start_command_window (path);
      start_view_window;
      show_menu (format => mformat);
      show_directory_version (path);
      start_input_window;
      declare
         success   : Boolean;
         confirmed : Boolean;
         KeyCode   : TIC.Real_Key_Code;
      begin
         TIC.Set_KeyPad_Mode (Win => inpwindow, SwitchOn => True);
         loop
            TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
            KeyCode := TIC.Get_Keystroke (inpwindow);
            clear_input_window;
            case KeyCode is
               when TIC.Key_F1 =>
                  browse_directory (path);
                  show_menu (format => mformat);
                  show_directory_version (path);
               when TIC.Key_F2 =>
                  show_menu (format => saveas);
                  confirmed := confirm_save_as (cleanpath);
                  show_menu (format => mformat);
                  show_directory_version (path);
                  if confirmed then
                     duplicate_directory (path, cleanpath, success);
                     if success then
                        indicate_success (cleanpath);
                        exit;
                     end if;
                  end if;
               when TIC.Key_F3 =>
                  show_menu (format => saveas);
                  confirmed := confirm_save_as (newpath);
                  show_menu (format => mformat);
                  show_directory_version (path);
                  if confirmed then
                     duplicate_directory (path, newpath, success);
                     if success then
                        indicate_success (newpath);
                        exit;
                     end if;
                  end if;
               when TIC.Key_F4 => exit;
               when others     => null;
            end case;
         end loop;
      end;
      TIC.Delete (inpwindow);
      TIC.Delete (viewport);
      TIC.Delete (comwindow);
      TIC.End_Windows;
   end launch_deleted_directory;


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


   --------------------
   --  show_version  --
   --------------------

   procedure show_version (viewable : in Boolean) is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      center   : constant TIC.Line_Position := viewheight / 2;
      leftside : TIC.Column_Count;
   begin
      if viewable then
         leftside := (app_width - 54) / 2;
      else
         leftside := (app_width - 52) / 2;
      end if;
      TIC.Erase (Win => viewport);
      TIC.Set_Color (Win => viewport, Pair => c_green);
      TIC.Move_Cursor (Win => viewport, Line => center, Column => leftside);
      TIC.Add (Win => viewport, Str => "Deleted version found: ");
      TIC.Add (Win => viewport, Str => ScanData.history (1).timestamp);
      if viewable then
         TIC.Add (Win => viewport, Str => " (viewable)");
      else
         TIC.Add (Win => viewport, Str => " (binary)");
      end if;
      TIC.Change_Attributes (Win    => viewport,
                             Line   => center,
                             Column => leftside + 23,
                             Count  => 19);
      TIC.Refresh (Win => viewport);
   end show_version;


   ------------------------------
   --  show_directory_version  --
   ------------------------------

   procedure show_directory_version (path : in String)
   is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      center   : constant TIC.Line_Position := viewheight / 2;
      leftside : TIC.Column_Count;
   begin
      leftside := (app_width - 44) / 2;
      TIC.Erase (Win => viewport);
      TIC.Set_Color (Win => viewport, Pair => c_green);
      TIC.Move_Cursor (Win => viewport, Line => center, Column => leftside);
      TIC.Add (Win => viewport, Str => "Deleted directory found: ");

      TIC.Set_Color (Win => viewport, Pair => c_white);
      TIC.Add (Win => viewport, Str => DHH.modification_timestamp (path));

      TIC.Refresh (Win => viewport);
   end show_directory_version;


   ------------------------
   --  indicate_success  --
   ------------------------

   procedure indicate_success (destination : in String) is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      msg1     : constant String := "Successfully restored file to";
      row1     : constant TIC.Line_Position := (viewheight / 2) + 2;
      row2     : constant TIC.Line_Position := row1 + 1;
      leftside : TIC.Column_Count;
      sindex   : Positive;
   begin
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


   -----------------------
   --  confirm_save_as  --
   -----------------------

   function confirm_save_as (destination : in String) return Boolean is
      use type TIC.Line_Position;
      use type TIC.Column_Count;
      msg1     : constant String := "Please confirm this should be " &
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


   ----------------------------
   --  start_command_window  --
   ----------------------------

   procedure start_command_window (path : in String) is
      use type TIC.Column_Count;
      bar : String (1 .. Integer (app_width)) := (others => ' ');
   begin
      comwindow := TIC.Create (
                      Number_Of_Lines       => 2,
                      Number_Of_Columns     => app_width + 1,
                      First_Line_Position   => 0,
                      First_Column_Position => 0);

      TIC.Move_Cursor (Win => comwindow, Line => 1, Column => 0);
      TIC.Set_Character_Attributes (Win => comwindow,
         Attr => TIC.Normal_Video, Color => TIC.Color_Pair (c_path));
      if path'Length > bar'Length then
         bar := path (path'First .. bar'Length);
      else
         bar (bar'First .. path'Length) := path;
      end if;
      TIC.Add (Win => comwindow, Str => bar);
   end start_command_window;


   -----------------
   --  show_menu  --
   -----------------

   procedure show_menu (
      format : menu_format;
      scrollup : in Boolean := False;
      scrolldown : in Boolean := False)
   is
      use type TIC.Column_Count;
      space : constant TIC.Attributed_Character := (Ch => ' ',
                  Color => TIC.Color_Pair'First,
                  Attr => (others => False));
      blank : String (1 .. Integer (app_width)) := (others => ' ');
      topmenu : constant Boolean := format = found_text or
                                    format = found_binary;
   begin
      TIC.Set_Character_Attributes (Win => comwindow, Attr => bright,
         Color => TIC.Color_Pair (c_white));

      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);
      TIC.Add (Win => comwindow, Str => blank);
      TIC.Move_Cursor (Win => comwindow, Line => 0, Column => 0);

      if topmenu then
         if format = found_text then
            TIC.Add (Win => comwindow, Str => "F1: ");
            TIC.Set_Color (Win => comwindow, Pair => c_cyan);
            TIC.Add (Win => comwindow, Str => "View contents  ");
         end if;
         if format = found_binary then
            TIC.Set_Color (Win => comwindow, Pair => c_black);
            TIC.Add (Win => comwindow, Str => "F1: View contents  ");
         end if;
         TIC.Set_Color (Win => comwindow, Pair => c_white);
         TIC.Add (Win => comwindow, Str => "F2: ");
         TIC.Set_Color (Win => comwindow, Pair => c_cyan);
         TIC.Add (Win => comwindow, Str => "Undelete  ");
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
      TIC.Refresh (Win => comwindow);
   end show_menu;


   ---------------------
   --  error_message  --
   ---------------------

   procedure error_message (message : in String) is
      msg : String (1 .. Integer (app_width) - 1) := (others => ' ');
   begin
      msg (msg'First .. message'Last) := message;
      TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
      TIC.Set_Color (Win => inpwindow, Pair => c_red);
      TIC.Add (Win => inpwindow, Str => msg);
      TIC.Refresh (Win => inpwindow);
   end error_message;


   ----------------
   --  recreate  --
   ----------------

   procedure recreate (origin : in String; destination : in String;
      success : out Boolean) is
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


   ---------------------------
   --  duplicate_directory  --
   ---------------------------

   procedure duplicate_directory (
      origin : in String;
      destination : in String;
      success : out Boolean)
   is
      RC      : Integer;
      tmpfile : constant String := "/tmp/slider-cpdup-" &
                                   DIR.Simple_Name (destination);
      arg1    : constant String := "-VV";
      arg2    : constant String := "-i0";
      Args    : GOS.Argument_List := (
                   new String'(arg1),
                   new String'(arg2),
                   new String'(origin),
                   new String'(destination));
   begin
      GOS.Spawn (Program_Name => "/bin/cpdup",
                 Args         => Args,
                 Output_File  => tmpfile,
                 Return_Code  => RC,
                 Success      => success);
      for Index in Args'Range loop
         GOS.Free (Args (Index));
      end loop;
      if RC = 0 then
         if DIR.Exists (tmpfile) then
            DIR.Delete_File (tmpfile);
         end if;
      else
         error_message ("CPDUP FAILED!  see " & tmpfile);
         success := False;
      end if;
   end duplicate_directory;


   -------------------
   --  browse_file  --
   -------------------

   procedure browse_file (path : in String) is
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
                     offsets (page) := j + 1;
                     lastpage := page;
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
                     case Contents (marker) is
                        when ASCII.NUL .. ASCII.BS | ASCII.VT .. ASCII.US =>
                           null;
                        when ASCII.LF =>
                           mline := mline + 1;
                           if mline = viewheight then
                              exit;
                           end if;
                           mcol  := 1;
                        when others =>
                           if mcol <= Natural (app_width) then
                              matrix (mline) (mcol) := Contents (marker);
                              mcol := mcol + 1;
                           end if;
                     end case;
                     exit when mline > viewheight;
                     marker := marker + 1;
                     exit when marker > File_Size;
                  end loop;
                  for j in 0 .. viewheight - 1 loop
                     TIC.Move_Cursor (Win => viewport, Line => j, Column => 0);
                     TIC.Add (Win => viewport, Str => matrix (j));
                  end loop;
               end;
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


   ------------------------------------
   --  < operator for listing_entry  --
   ------------------------------------

   function "<" (L, R : listing_entry) return Boolean
   is
   begin
      return L.filename < R.filename;
   end "<";


   ----------------------------------
   --  retrieve_directory_listing  --
   ----------------------------------

   function retrieve_directory_listing (directory_path : in String)
   return listing_array
   is
      search : DIR.Search_Type;
      filter : DIR.Filter_Type := (others => True);
      data   : DIR.Directory_Entry_Type;
      total  : Natural := 0;
   begin
      DIR.Start_Search (
         Search    => search,
         Directory => directory_path,
         Pattern   => "",
         Filter    => filter);
      while DIR.More_Entries (search) loop
         DIR.Get_Next_Entry (search, data);
         declare
            sname : constant String := DIR.Simple_Name (data);
            okay  : constant Boolean := not (sname = "." or sname = "..");
         begin
            if okay then
               total := total + 1;
            end if;
         end;
      end loop;
      DIR.End_Search (search);

      declare
         result : listing_array (0 .. total - 1);
         index  : Natural := 0;
      begin
         DIR.Start_Search (
            Search    => search,
            Directory => directory_path,
            Pattern   => "",
            Filter    => filter);
         while DIR.More_Entries (search) loop
            DIR.Get_Next_Entry (search, data);
            declare
               use type DIR.File_Kind;
               fname : String (1 .. 255) := (others => ' ');
               sname : constant String := DIR.Simple_Name (data);
               okay  : constant Boolean := not (sname = "." or sname = "..");
            begin
               if okay then
                  fname (fname'First .. sname'Last) := sname;
                  result (index).filename := fname;
                  result (index).filetype := DIR.Kind (data);
                  if result (index).filetype /= DIR.Ordinary_File then
                     result (index).filesize := 0;
                  else
                     result (index).filesize := DIR.Size (data);
                  end if;
                  result (index).modtime  := DHH.modification_timestamp (
                     directory_path & "/" & sname);
                  index := index + 1;
               end if;
            end;
         end loop;
         DIR.End_Search (search);
         Sort (result);
         return result;
      end;
   end retrieve_directory_listing;


   ---------------------------
   --  human_readable_size  --
   ---------------------------

   function human_readable_size (filesize : DIR.File_Size) return String
   is
      result : String := "    0";
      size   : constant Natural  := Natural (filesize);
      raw    : constant String   := size'Img;
      kilo   : constant Positive := 1024;
      mega   : constant Positive := kilo * kilo;
      giga   : constant Positive := mega * kilo;
   begin
      if size < 10 then
         result (5) := raw (2);
      elsif size < 100 then
         result (4 .. 5) := raw (2 .. 3);
      elsif size < 1000 then
         result (3 .. 5) := raw (2 .. 4);
      elsif size < 10000 then
         result (2 .. 5) := raw (2 .. 5);
      elsif size < 100000 then
         result (1 .. 5) := raw (2 .. 6);
      elsif size < mega then
         declare
            K     : constant Positive := size / kilo;
            K_raw : constant String := K'Img;
         begin
            result := " " & K_raw (2 .. 4) & "K";
         end;
      elsif size < mega * 10 then
         declare
            M10   : constant Positive := (10 * size) / mega;
            M10_raw : constant String := M10'Img;
         begin
            result := " " & M10_raw (2) & "." & M10_raw (3) & "M";
         end;
      elsif size < giga then
         declare
            M     : constant Positive := size / mega;
            M_raw : constant String := M'Img;
         begin
            if size < mega * 100 then
               result := "  " & M_raw (2 .. 3) & "M";
            else
               result := " " & M_raw (2 .. 4) & "M";
            end if;
         end;
      else  --  assume all files < 10G
         declare
            G10     : constant Positive := size / (giga / 10);
            G10_raw : constant String := G10'Img;
         begin
            result := " " & G10_raw (2) & "." & G10_raw (3) & "G";
         end;
      end if;
      return result;
   end human_readable_size;


   ------------------------
   --  browse_directory  --
   ------------------------

   procedure browse_directory (directory_path : in String)
   is
      contents : constant listing_array :=
                    retrieve_directory_listing (directory_path);
      listsize : constant Natural := contents'Length;
      pages    : constant Positive :=
                    1 + ((listsize - 1) / Positive (viewheight));
      page     : Positive := 1;

      procedure display_page (page : Positive);
      procedure display_nothing;

      procedure display_nothing
      is
      begin
         TIC.Erase (viewport);
         TIC.Set_Character_Attributes (
            Win  => viewport,
            Attr => TIC.Normal_Video);
         TIC.Add (Win => viewport,
                  Str => "This directory is completely empty!");
         TIC.Refresh (Win => viewport);
         TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
      end display_nothing;

      procedure display_page (page : Positive)
      is
         use type TIC.Line_Position;
         minindex : Natural := (page - 1) * Positive (viewheight);
         maxindex : Natural := (page * Positive (viewheight)) - 1;
         line     : TIC.Line_Position := 0;
         displen  : constant Positive := Positive (app_width) - 23;
      begin
         if maxindex >= listsize then
            maxindex := listsize - 1;
         end if;
         TIC.Erase (viewport);
         for index in minindex .. maxindex loop
            TIC.Move_Cursor (Win => viewport, Line => line, Column => 0);
            case contents (index).filetype is
               when DIR.Directory =>
                  TIC.Set_Character_Attributes (Win => viewport,
                     Attr => bright, Color => c_cyan);
               when DIR.Special_File => null;
                  TIC.Set_Character_Attributes (Win => viewport,
                     Attr => bright, Color => c_magenta);
               when DIR.Ordinary_File => null;
                  TIC.Set_Character_Attributes (Win => viewport,
                     Attr => TIC.Normal_Video);
            end case;
            TIC.Add (Win => viewport,
                     Str => contents (index).filename (1 .. displen));
            TIC.Move_Cursor (Win => viewport, Line => line,
               Column => TIC.Column_Position (displen + 1));
            TIC.Set_Character_Attributes (Win => viewport,
               Attr => TIC.Normal_Video);
            TIC.Add (Win => viewport, Str => human_readable_size (
                     contents (index).filesize) & " ");
            TIC.Set_Color (Win => viewport, Pair => c_yellow);
            TIC.Add (Win => viewport,
                     Str => contents (index).modtime (1 .. 16));
            line := line + 1;
         end loop;
         TIC.Refresh (Win => viewport);
      end display_page;
   begin
      declare
         KeyCode  : TIC.Real_Key_Code;
      begin
         loop
            show_menu (
               format     => view,
               scrollup   => page > 1,
               scrolldown => page < pages
            );
            if listsize > 0 then
               show_page_count (page, pages);
               display_page (page);
            else
               display_nothing;
            end if;
            TIC.Move_Cursor (Win => inpwindow, Line => 0, Column => 0);
            KeyCode := TIC.Get_Keystroke (inpwindow);
            case KeyCode is
               when TIC.Key_Cursor_Up =>
                  if page > 1 then
                     page := page - 1;
                  end if;
               when TIC.Key_Cursor_Down =>
                  if page < pages then
                     page := page + 1;
                  end if;
               when TIC.Key_F4 =>
                  clear_input_window;
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end;
   end browse_directory;

end Transactions.Delete;
