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
with Ada.Strings.Fixed;
with Ada.Calendar.Formatting;
with Ada.Calendar.Conversions;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with DragonFly.FileStatus;

package body DragonFly.HAMMER.History is

   package TIO renames Ada.Text_IO;
   package CFM renames Ada.Calendar.Formatting;
   package CCV renames Ada.Calendar.Conversions;
   package DIR renames Ada.Directories;
   package SFX renames Ada.Strings.Fixed;
   package STR renames Ada.Strings;
   package DFS renames DragonFly.FileStatus;


   --------------------
   --  scan_history  --
   --------------------

   procedure scan_history (path : in String; result : out scan_result)
   is
      descriptor : file_descriptor;
      history    : HB.hammer_ioc_history;
      formhack   : TraxTime;
   begin
      result.state      := clean;
      result.path_check := not_found;

      declare
      begin
         descriptor := HB.open_file_for_reading (path);
         result.path_check := found;
      exception
         when FILE_Failure => null;
      end;
      if result.path_check = not_found then
         declare
            dname    : Trax;
            delname  : Trax;
            delstamp : uInt32;
         begin
            dname := match_against_directory_trax (path);
            descriptor := HB.open_file_for_reading (path => path & dname);
            collect_history (fd => descriptor, filename => path,
               suffix => delname, timestamp => delstamp);
            if delname = failed_search then
               raise Match_Failure;
            end if;
            HB.close_file (descriptor);
            descriptor := HB.open_file_for_reading (path => path & delname);
            formhack   := format_timestamp (delstamp);
            result.path_check := deleted;
            result.history.Append (New_Item => (delname, formhack));
         exception
            when FILE_Failure => raise Match_Failure;
         end;
      end if;


      if result.path_check = found then
         history := HB.initialize_history;
         HB.retrieve_history (open_file => descriptor, history => history);
         if (history.head.flags and HB.HAMMER_IOC_HISTORY_UNSYNCED) > 0 then
            result.state := dirty;
         end if;
         History_Loop :
            loop
               for j in 0 .. history.count - 1 loop
                  formhack := format_timestamp (history.hist_ary (j).time32);
                  result.history.Append (New_Item => ("@@0x" &
                     format_as_hex (history.hist_ary (j).tid), formhack));
               end loop;
               exit History_Loop when
                  (history.head.flags and HB.HAMMER_IOC_HISTORY_EOF) > 0;
               exit History_Loop when
                  (history.head.flags and HB.HAMMER_IOC_HISTORY_NEXT_KEY) > 0;
               exit History_Loop when
                  (history.head.flags and HB.HAMMER_IOC_HISTORY_NEXT_TID) = 0;
               history.beg_tid := history.nxt_tid;
               HB.retrieve_history (
                  open_file => descriptor,
                  history   => history);
            end loop History_Loop;
      end if;

   exception
      when IOCTL_Failure | Match_Failure => null;
      when Error : others =>
         TIO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end scan_history;


   ------------------------------------
   --  match_against_directory_trax  --
   ------------------------------------

   function match_against_directory_trax (filename : String) return Trax is
      fd       : file_descriptor;
      slash    : Natural;
      result   : String := failed_search;
      dontcare : uInt32;
   begin
      slash := SFX.Index (
                  Source => filename,
                  Pattern => "/",
                  Going => STR.Backward);
      if slash = 0 then
         fd := HB.open_file_for_reading (".");
      else
         fd := HB.open_file_for_reading (
            filename (filename'First .. slash - 1));
      end if;
      if fd > 0 then
         collect_history (fd => fd, filename => filename, suffix => result,
            timestamp => dontcare);
         HB.close_file (fd);
      end if;
      if result = failed_search then
         raise Match_Failure;
      end if;
      return result;   -- except will be caught upstream if fd < 0
   end match_against_directory_trax;


   -----------------------
   --  collect_history  --
   -----------------------

   procedure collect_history (
               fd        : in file_descriptor;
               filename  : in String;
               suffix    : out Trax;
               timestamp : out uInt32)
   is
      history : HB.hammer_ioc_history;
      top_tid : HB.hammer_tid := 0;
      eject   : Boolean;
   begin
      suffix := failed_search;
      history := HB.initialize_history;
      history.nxt_key := HB.HAMMER_MAX_KEY;
      history.head.flags := HB.HAMMER_IOC_HISTORY_ATKEY;
      HB.retrieve_history (open_file => fd, history => history);
      loop
         eject := True;
         for j in 0 .. history.count - 1 loop
            if history.hist_ary (j).tid > top_tid then
               declare
                  temptrx : constant String := "@@0x" &
                               format_as_hex (history.hist_ary (j).tid);
                  testfd  : file_descriptor;
                  inode   : DFS.inode_data;
                  result  : Int32;
               begin
                  DFS.stat (filename & temptrx, inode, result);
                  if result < 0 then
                     raise FILE_Failure;
                  end if;
                  if (inode.st_mode and DFS.S_IFIFO) > 0 then
                     raise Fake_Transaction;
                  end if;
                  testfd := HB.open_file_for_reading (filename & temptrx);
                  if testfd > 0 then
                     top_tid   := history.hist_ary (j).tid;
                     timestamp := history.hist_ary (j).time32;
                     suffix    := temptrx;
                     HB.close_file (testfd);
                  end if;
               exception
                  when FILE_Failure => null;
                  when Fake_Transaction => null;
               end;
            end if;
         end loop;
         exit when (history.head.flags and HB.HAMMER_IOC_HISTORY_EOF) > 0;
         if (history.head.flags and HB.HAMMER_IOC_HISTORY_NEXT_KEY) > 0 and
            history.key /= history.nxt_key
         then
            history.key := history.nxt_key;
            history.nxt_key := HB.HAMMER_MAX_KEY;
            eject := False;
         end if;
         if (history.head.flags and HB.HAMMER_IOC_HISTORY_NEXT_TID) > 0 and
            history.beg_tid /= history.nxt_tid
         then
            history.beg_tid := history.nxt_tid;
            eject := False;
         end if;
         exit when eject;
         HB.retrieve_history (open_file => fd, history => history);
      end loop;
   end collect_history;


   ------------------------------------------
   --  Format %016x : format_as_hex (tid)  --
   ------------------------------------------

   function format_as_hex (bignum : HB.hammer_tid) return String is
   begin
      return zeropad_hex (uInt64 (bignum));
   end format_as_hex;


   ------------------------
   --  format_timestamp  --
   ------------------------

   function format_timestamp (raw : uInt32) return TraxTime is
      hack : HB.IC.long := HB.IC.long (raw);
      hack_time : CAL.Time;
   begin
      hack_time := CCV.To_Ada_Time (Unix_Time => hack);
      return CFM.Image (Date => hack_time, Time_Zone => tz_offset);
   end format_timestamp;


   ------------------------------
   --  modification_timestamp  --
   ------------------------------

   function modification_timestamp (path : in String) return TraxTime is
      modtime : constant Ada.Calendar.Time := DIR.Modification_Time (path);
   begin
      return CFM.Image (Date => modtime, Time_Zone => tz_offset);
   end modification_timestamp;

end DragonFly.HAMMER.History;
