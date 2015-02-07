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


with DragonFly.FileStatus;

package body DragonFly.HAMMER.Ghosts is

   package DFS renames DragonFly.FileStatus;


   ----------------------------
   --  scan_for_file_ghosts  --
   ----------------------------

   procedure scan_for_file_ghosts (
      directory_path   : in String;
      file_ghosts      : out Filename_Container.Vector;
      directory_ghosts : out Filename_Container.Vector)
   is
      use type DIR.File_Kind;
      descriptor     : file_descriptor;
      just_directory : constant String := DIR.Containing_Directory
                                          (directory_path & "/")  & "/";
   begin
      file_ghosts.Clear;
      directory_ghosts.Clear;

      if DIR.Exists (directory_path) and then
         DIR.Kind (directory_path) = DIR.Directory
      then
         descriptor := HB.open_file_for_reading (just_directory);
         if descriptor > 0 then
            establish_baseline (just_directory);
            scan_history_of_directory (
               fd             => descriptor,
               directory_path => just_directory,
               ghosts_file    => file_ghosts,
               ghosts_dir     => directory_ghosts);
            HB.close_file (descriptor);
            SortEntry.Sort (file_ghosts);
            SortEntry.Sort (directory_ghosts);
         end if;
      end if;
   end scan_for_file_ghosts;


   --------------------------
   --  establish_baseline  --
   --------------------------

   procedure establish_baseline (directory_path : in String)
   is
   begin
      living_files.Clear;
      living_dirs.Clear;
      declare
         Search     : DIR.Search_Type;
         item       : DIR.Directory_Entry_Type;
         dir_filter : constant DIR.Filter_Type := (True, False, False);
      begin
         DIR.Start_Search (Search    => Search,
                           Directory => directory_path,
                           Pattern   => "",
                           Filter    => dir_filter);
         while DIR.More_Entries (Search) loop
            DIR.Get_Next_Entry (Search => Search, Directory_Entry => item);
            living_dirs.Append (DIR.Simple_Name (item));
         end loop;
      end;
      declare
         Search     : DIR.Search_Type;
         item       : DIR.Directory_Entry_Type;
         ord_filter : constant DIR.Filter_Type := (False, True, False);
      begin
         DIR.Start_Search (Search    => Search,
                           Directory => directory_path,
                           Pattern   => "",
                           Filter    => ord_filter);
         while DIR.More_Entries (Search) loop
            DIR.Get_Next_Entry (Search => Search, Directory_Entry => item);
            living_files.Append (DIR.Simple_Name (item));
         end loop;
      end;
   end establish_baseline;


   ------------------------------------------
   --  Format %016x : format_as_hex (tid)  --
   ------------------------------------------

   function format_as_hex (bignum : HB.hammer_tid) return String is
   begin
      return zeropad_hex (uInt64 (bignum));
   end format_as_hex;


   ------------------------
   --  scan_transaction  --
   ------------------------

   procedure scan_transaction (
      directory_path : in String;
      transaction    : in String;
      fkind          : in DIR.File_Kind;
      ghost          : in out Filename_Container.Vector)
   is
      use type DIR.File_Kind;
      filter : DIR.Filter_Type := (others => False);
      Search : DIR.Search_Type;
      item   : DIR.Directory_Entry_Type;

   begin
      filter (fkind) := True;
      DIR.Start_Search (Search    => Search,
                        Directory => directory_path & transaction,
                        Pattern   => "",
                        Filter    => filter);
      while DIR.More_Entries (Search) loop
         DIR.Get_Next_Entry (Search => Search, Directory_Entry => item);
         declare
            ndx      : Filename_Container.Extended_Index;
            filename : constant String := DIR.Simple_Name (item);
            truedir  : constant String := transaction & "/" & filename;
            traxpath : constant String := directory_path & truedir;
            okay     : constant Boolean :=
                          not (filename = "." or filename = "..");
            found    : Boolean := False;
         begin
            if okay then
               if fkind = DIR.Directory then
                  ndx := living_dirs.Find_Index (Item => filename);
                  if ndx = Filename_Container.No_Index then
                     for k in 1 .. Natural (ghost.Length) loop
                        if not found and then
                           ghost.Element (k) (22 .. ghost.Element (k)'Last) =
                              filename
                        then
                           found := True;
                        end if;
                     end loop;
                     if not found and then
                        directory_has_files (traxpath)
                     then
                        ghost.Append (truedir);
                     end if;
                  end if;
               else
                  ndx := living_files.Find_Index (Item => filename);
                  if ndx = Filename_Container.No_Index then
                     ndx := ghost.Find_Index (Item => filename);
                     if ndx = Filename_Container.No_Index then
                        declare
                           inode   : DFS.inode_data;
                           result  : Int32;
                        begin
                           DFS.stat (traxpath, inode, result);
                           if result < 0 then
                              raise FILE_Failure;
                           end if;
                           if (inode.st_mode and DFS.S_IFIFO) > 0 then
                              raise Fake_Transaction;
                           end if;
                           ghost.Append (filename);
                        exception
                           when FILE_Failure => null;
                           when Fake_Transaction => null;
                        end;
                     end if;
                  end if;
               end if;
            end if;
         end;
      end loop;

   end scan_transaction;


   ---------------------------------
   --  scan_history_of_directory  --
   ---------------------------------

   procedure scan_history_of_directory (
      fd             : in file_descriptor;
      directory_path : in String;
      ghosts_file    : in out Filename_Container.Vector;
      ghosts_dir     : in out Filename_Container.Vector)
   is
      history : HB.hammer_ioc_history;
      eject   : Boolean;
      tids    : TID_Container.Vector;
      ndx     : TID_Container.Extended_Index;
   begin
      history            := HB.initialize_history;
      history.nxt_key    := HB.HAMMER_MAX_KEY;
      history.head.flags := HB.HAMMER_IOC_HISTORY_ATKEY;
      HB.retrieve_history (open_file => fd, history => history);
      tids.Clear;
      scan_loop :
         loop
            for j in 0 .. history.count - 1 loop
               ndx := tids.Find_Index (Item => history.hist_ary (j).tid);
               if ndx = TID_Container.No_Index then
                  tids.Append (history.hist_ary (j).tid);
               end if;
            end loop;
            exit scan_loop when
               (history.head.flags and HB.HAMMER_IOC_HISTORY_EOF) > 0;

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
            exit scan_loop when eject;
            HB.retrieve_history (open_file => fd, history => history);
         end loop scan_loop;
      SortTid.Sort (tids);
      for x in reverse 1 .. Natural (tids.Length) loop
         declare
            temptrx : constant String := "@@0x" & format_as_hex (tids (x));
            inode   : DFS.inode_data;
            result  : Int32;
         begin
            DFS.stat (directory_path & temptrx, inode, result);
            if result < 0 then
               raise FILE_Failure;
            end if;
            if (inode.st_mode and DFS.S_IFDIR) = 0 then
               raise FILE_Failure;
            end if;
            scan_transaction (
               directory_path => directory_path,
               transaction    => temptrx,
               fkind          => DIR.Directory,
               ghost          => ghosts_dir);
            scan_transaction (
               directory_path => directory_path,
               transaction    => temptrx,
               fkind          => DIR.Ordinary_File,
               ghost          => ghosts_file);
            exception
               when FILE_Failure => null;
         end;
      end loop;

   end scan_history_of_directory;


   ---------------------------
   --  directory_has_file  --
   ---------------------------

   function directory_has_files (directory_path : String) return Boolean
   is
      search : DIR.Search_Type;
      filter : DIR.Filter_Type := (others => True);
      data   : DIR.Directory_Entry_Type;
      result : Boolean := False;
   begin
      if not DIR.Exists (directory_path) then
         return False;
      end if;
      DIR.Start_Search (
         Search    => search,
         Directory => directory_path,
         Pattern   => "",
         Filter    => filter);
      while not result and DIR.More_Entries (search) loop
         DIR.Get_Next_Entry (search, data);
         declare
            sname : constant String := DIR.Simple_Name (data);
            okay  : constant Boolean := not (sname = "." or sname = "..");
         begin
            if okay then
               result := True;
            end if;
         end;
      end loop;
      DIR.End_Search (search);
      return result;
   end directory_has_files;

end DragonFly.HAMMER.Ghosts;
