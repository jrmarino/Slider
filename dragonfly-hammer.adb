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


package body DragonFly.HAMMER is

   -------------------
   --  zeropad_hex  --
   -------------------

   function zeropad_hex (bignum : uInt64) return Hex16Final is
      result   : Hex16Final := "0000000000000000";
      walk     : Integer := 16;
      hex      : constant uInt64 := 16;
      temp     : uInt64 := bignum;
      quotient : uInt64;
      remains  : uInt64;
   begin
      for X in 0 .. 15 loop
         quotient := temp / hex;
         remains  := temp mod hex;
         if remains > 0 then
            case remains is
               when 10 .. 15 =>
                  result (walk) := Character'Val (87 + remains);
               when others =>
                  result (walk) := Character'Val (48 + remains);
            end case;
         end if;
         exit when quotient = 0;
         walk := walk - 1;
         temp := quotient;
      end loop;
      return result;
   end zeropad_hex;


   --------------------------------------------
   --  Format %016x : format_as_hex (int64)  --
   --------------------------------------------

   function format_as_hex (bignum : Int64) return String is
      converted_num : uInt64;
   begin
      if bignum < 0 then
         converted_num := uInt64'Last - uInt64 (0 - (bignum + 1));
      else
         converted_num := uInt64 (bignum);
      end if;
      return zeropad_hex (converted_num);
   end format_as_hex;

end DragonFly.HAMMER;
