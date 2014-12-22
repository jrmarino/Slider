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


package DragonFly is

   pragma Pure;

   -------------------------------
   ---  DATA TYPE DEFINITIONS  ---
   -------------------------------

   type uInt64 is mod 2 ** 64;
   type uInt32 is mod 2 ** 32;
   type uInt16 is mod 2 ** 16;
   type Int64  is range -(2 ** 63) .. +(2 ** 63) - 1;
   type Int32  is range -(2 ** 31) .. +(2 ** 31) - 1;

   subtype file_descriptor is Int32;

end DragonFly;
