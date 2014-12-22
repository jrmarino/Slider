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
with Ada.Command_Line;
with Transactions;

procedure slider is

   package CL  renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   SVER : constant String := "1.00";

   procedure echo_usage;

   procedure echo_usage is
      DASH : constant String (1 .. 47) := (others => '=');
   begin
      TIO.New_Line;
      TIO.Put_Line (DASH);
      TIO.Put_Line ("  HAMMER file system time slider utility " & SVER);
      TIO.Put_Line (DASH);
      TIO.Put_Line ("       Copyright (C) 2014 John R. Marino");
      TIO.New_Line;
      TIO.New_Line;
      TIO.Put_Line ("Usage: slider file [save-as-file]");
      TIO.New_Line;
      TIO.Put_Line ("This tool enables the user to browse through all " &
                    "available versions of the");
      TIO.Put_Line ("given file stored in HAMMER history, view differences " &
                    "between two versions,");
      TIO.Put_Line ("replace the current version with any previous version," &
                    " restore a deleted file,");
      TIO.Put_Line ("and save any previous or deleted version to a new " &
                    "file.");
      TIO.New_Line;
   end echo_usage;

begin

   if CL.Argument_Count < 1 or CL.Argument_Count > 2 then
      echo_usage;
   else
      if CL.Argument (1) = "-h" or
         CL.Argument (1) = "--help" or
         CL.Argument (1) = "-v" or
         CL.Argument (1) = "--version"
      then
         echo_usage;
      else
         if CL.Argument_Count = 1 then
            Transactions.launch (CL.Argument (1),
               CL.Argument (1) & ".restored");
         else
            Transactions.launch (CL.Argument (1), CL.Argument (2));
         end if;
      end if;
   end if;

end slider;
