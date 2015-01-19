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


with DragonFly.HAMMER.Binding;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Containers.Vectors;

package DragonFly.HAMMER.History is

   package HB renames DragonFly.HAMMER.Binding;
   package CAL renames Ada.Calendar;
   package CTZ renames Ada.Calendar.Time_Zones;


   ---------------------------
   --  Types and Constants  --
   ---------------------------

   type flush_state   is (dirty, clean);
   type search_result is (found, deleted, not_found, secret);

   subtype Trax       is String (1 .. 20);
   subtype TraxTime   is String (1 .. 19);

   failed_search : constant Trax := "@@0xFAIL            ";

   type Transaction is record
      trax_id   : Trax;
      timestamp : TraxTime;
   end record;

   package Transaction_Container is
      new Ada.Containers.Vectors (Positive, Transaction);

   type scan_result is record
      state      : flush_state;
      path_check : search_result;
      diff_old   : Positive;
      diff_new   : Positive;
      history    : Transaction_Container.Vector;
   end record;

   -----------------
   --  Functions  --
   -----------------

   procedure scan_history (path : in String; result : out scan_result);
   --   This saves hammer history or the latest found version in the case
   --   of a deleted file.

   function match_against_directory_trax (filename : String) return Trax;
   --  This returns the first transaction that has a prefix of filename.
   --  It is only used if the file doesn't exist, so we are looking for
   --  deleted files.

   function modification_timestamp (path : in String) return TraxTime;
   --  Convert file modification time YYYY-MM-DD HH:MM:SS format
   --  There is no handling for bad path

private

   tz_offset : constant CTZ.Time_Offset := CTZ.UTC_Time_Offset;

   function format_as_hex (bignum : HB.hammer_tid) return String;
   --  Format string with c-equivalent of %016x

   function format_timestamp (raw : uInt32) return TraxTime;
   --  Convert uInt32 to YYYY-MM-DD HH:MM:SS format with system offset

   procedure collect_history (
               fd        : in DragonFly.file_descriptor;
               filename  : in String;
               suffix    : out Trax;
               timestamp : out uInt32);
   --  Used to find latest version of a deleted file

end DragonFly.HAMMER.History;
