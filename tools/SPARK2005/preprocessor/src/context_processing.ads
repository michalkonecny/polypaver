------------------------------------------------------------------------------
--                                                                          --
--                    ASIS APPLICATION TEMPLATE COMPONENTS                  --
--                                                                          --
--                     C O N T E X T _ P R O C E S S I N G                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 2000, Free Software Foundation, Inc.            --
--                                                                          --
-- ASIS  Application  Templates are  free software; you can redistribute it --
-- and/or  modify it under  terms  of the  GNU  General  Public  License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option) any later version. ASIS Application Templates are distributed in --
-- the hope that they will be useful, but  WITHOUT  ANY  WARRANTY; without  --
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
-- PURPOSE. See the GNU General Public License for more details. You should --
-- have  received a copy of the GNU General Public License distributed with --
-- distributed  with  GNAT;  see  file  COPYING. If not, write to the Free  --
-- Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
-- USA.                                                                     --
--                                                                          --
-- ASIS Application Templates were developed and are now maintained by Ada  --
-- Core Technologies Inc (http://www.gnat.com).                             --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for high-level processing of
--  (terating through) an ASIS Context

with Asis;

package Context_Processing is

   procedure Process_Context
     (The_Context : Asis.Context;
      Trace       : Boolean := False;
      Output_Path : String);
   --  This procedure iterates through the whole content of its argument
   --  Context and it calls a unit processing routine for those ASIS
   --  Compilation Units which are of An_Application_Unit origin (that is,
   --  user-defined units). If Trace parameter is set ON, it generate the
   --  simple trace of the unit processing (consisting of the names of the
   --  units in the Context being processed or skipped).

end Context_Processing;
