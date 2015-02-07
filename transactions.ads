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


with DragonFly.HAMMER.History;
with Terminal_Interface.Curses;

package Transactions is

   package DHH renames DragonFly.HAMMER.History;
   package TIC renames Terminal_Interface.Curses;

   ScanData   : DHH.scan_result;
   app_width  : TIC.Column_Count := 80;
   comwindow  : TIC.Window;
   viewport   : TIC.Window;
   inpwindow  : TIC.Window;
   viewheight : TIC.Line_Count;
   c_green    : TIC.Color_Pair;
   c_path     : TIC.Color_Pair;
   c_cyan     : TIC.Color_Pair;
   c_white    : TIC.Color_Pair;
   c_black    : TIC.Color_Pair;
   c_red      : TIC.Color_Pair;
   c_yellow   : TIC.Color_Pair;
   c_magenta  : TIC.Color_Pair;
   Key_Num1   : constant TIC.Key_Code := Character'Pos ('1');
   Key_Num2   : constant TIC.Key_Code := Character'Pos ('2');
   Key_Num3   : constant TIC.Key_Code := Character'Pos ('3');
   Key_Num4   : constant TIC.Key_Code := Character'Pos ('4');
   bright     : constant TIC.Character_Attribute_Set := (
                   Bold_Character   => True,
                   others => False);
   restored   : constant String := ".restored";
   bust       : constant String := "  undetected  ";
   nocolor    : constant String := "  monochrome  ";
   msg_mono1  : constant String := "Sorry, this terminal does not support " &
                                    "color through curses.";
   msg_mono2  : constant String := "As color support is currently a " &
                                   "requirement, Slider is unable to launch.";

   procedure launch (path, newpath : in String; twoparams : in Boolean);
   --  entry procedure which launches other routines as needed

   function textfile (path : in String) return Boolean;
   --  Returns true if there are no null characters in the file

private

   function textfile_private (path : in String; File_Size : in Natural)
   return Boolean;

end Transactions;
