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


with Ada.Text_IO;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.Containers;
with Transactions.Delete;
with Transactions.Ghosts;
with Transactions.Slide;

package body Transactions is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package DEL renames Transactions.Delete;
   package GST renames Transactions.Ghosts;
   package SLD renames Transactions.Slide;


   --------------
   --  launch  --
   --------------

   procedure launch (path, newpath : in String; twoparams : in Boolean)
   is
      use type DHH.flush_state;
      use type DHH.search_result;
      use type DIR.File_Kind;
      use type Ada.Containers.Count_Type;

      function gen_save_as (clean_ghost : String) return String;
      function ghost_clean (ghost : String) return String;

      function gen_save_as (clean_ghost : String) return String
      is
      begin
         if twoparams then
            return newpath;
         else
            return clean_ghost & restored;
         end if;
      end gen_save_as;

      function ghost_clean (ghost : String) return String
      is
         atsign  : Natural := 0;
         counter : Positive := 1;
      begin
         if ghost'Length = 0 then
            return ghost;
         end if;
         loop
            if ghost (counter) = '@' then
               atsign := counter;
               exit;
            end if;
            counter := counter + 1;
            exit when counter > ghost'Last;
         end loop;
         if atsign = 0 then
            return ghost;
         else
            return ghost (ghost'First .. atsign - 1) &
                   ghost (atsign + 21 .. ghost'Last);
         end if;
      end ghost_clean;

   begin
      if DIR.Exists (path) and then DIR.Kind (path) = DIR.Directory then
         declare
            ghost    : constant String := GST.scan_directory (path);
            clean    : constant String := ghost_clean (ghost);
            newghost : constant String := gen_save_as (clean);
         begin
            if ghost = bust then
               TIO.Put_Line ("Sorry, no deleted entries were detected in " &
                  "this directory.");
            elsif ghost = nocolor then
               TIO.Put_Line (msg_mono1);
               TIO.Put_Line (msg_mono2);
            elsif ghost = "" then
               null;
            else
               if DIR.Exists (ghost) and then
                  DIR.Kind (ghost) = DIR.Directory
               then
                  DEL.launch_deleted_directory (ghost, clean, newghost);
                  return;
               end if;
               DHH.scan_history (ghost, ScanData);
               DEL.launch_deleted (ghost, newghost);
            end if;
            return;
         end;
      end if;

      DHH.scan_history (path, ScanData);
      if ScanData.path_check = DHH.not_found then
         TIO.Put_Line ("Error: There is no current file with that name, " &
            "nor is has any trace of a");
         TIO.Put_Line ("deleted version of '" & path & "' being found.");
         return;
      end if;
      if ScanData.path_check = DHH.secret then
         TIO.Put_Line ("Error: The file exists, but its permissions " &
                       "prevent Slider from reading it.");
         return;
      end if;
      if ScanData.path_check = DHH.deleted then
         DEL.launch_deleted (path, gen_save_as (path));
         return;
      end if;
      if ScanData.history.Is_Empty then
         TIO.Put_Line ("Sorry, this file has no history at all.");
         return;
      end if;
      if ScanData.history.Length < 2 then
         TIO.Put_Line ("Sorry, this file has no previous version available.");
         return;
      end if;
      if ScanData.state = DHH.dirty then
         TIO.Put_Line ("Sorry, the file state is dirty.  Wait up to 30 " &
            "seconds and try again.");
         return;
      end if;
      SLD.launch_slide (path, gen_save_as (path));
   end launch;


   ----------------
   --  textfile  --
   ----------------

   function textfile (path : in String) return Boolean
   is
      File_Size : Natural := Natural (DIR.Size (path));
   begin
      if File_Size > 2048 then
         File_Size := 2048;
      end if;
      if File_Size = 0 then
         return True;
      end if;
      return textfile_private (path, File_Size);
   end textfile;


   ------------------------
   --  textfile_private  --
   ------------------------

   function textfile_private (path : in String; File_Size : in Natural)
   return Boolean is
      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      package SIO renames File_String_IO;

      nonprint_limit : constant Natural := Natural (File_Size * 15 / 100);
      nonprint       : Natural := 0;

      File     : File_String_IO.File_Type;
      Contents : File_String;
      result   : Boolean := True;
   begin
      SIO.Open  (File, Mode => SIO.In_File, Name => path);
      SIO.Read  (File, Item => Contents);
      SIO.Close (File);
      for j in 1 .. File_Size loop
         case Contents (j) is
            when ASCII.NUL =>
               result := False;
               exit;
            when ASCII.SOH .. ASCII.BS | ASCII.VT .. ASCII.US =>
               nonprint := nonprint + 1;
            when others =>
               null;
         end case;
      end loop;
      if nonprint > nonprint_limit then
         result := False;
      end if;
      return result;
   end textfile_private;

end Transactions;
