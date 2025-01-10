{ hblr - LazReport for (x)Harbour

  Copyright (C) 2025 Michał Gawrycki info..gmsystems.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

library hblr;

{$MODE Delphi}

uses
  SysUtils,
  Classes,
  Interfaces,
  hblrintf in 'hblrintf.pas',
  hblrclass in 'hblrclass.pas' {HBLRObj: TDataModule},
  smpassocar in 'smpassocar.pas',
  Forms;

exports
  hblr_Init,
  hblr_ProcessMessages,
  hblr_New,
  hblr_Free,
  hblr_AddValueC,
  hblr_AddValueNI,
  hblr_AddValueNF,
  hblr_AddValueL,
  hblr_AddValueD,
  hblr_AddDataset,
  hblr_AddHbDataset,
  hblr_GetRowCount,
  hblr_ClearData,
  hblr_AddReport,
  hblr_ClearReports,
  hblr_LoadFromFile,
  hblr_SaveToFile,
  hblr_LoadFromMemory,
  hblr_LoadFromXMLFile,
  hblr_SaveToXMLFile,
  hblr_LoadFromXMLMemory,
  hblr_LoadPreparedReport,
  hblr_SavePreparedReport,
  hblr_ShowReport,
  hblr_ShowPreparedReport,
  hblr_PrepareReport,
  hblr_PrintPreparedReport,
  hblr_DesignReport,
  hblr_EditPreparedReport,
  hblr_GetTitle,
  hblr_SetTitle,
  hblr_GetInitialZoom,
  hblr_SetInitialZoom,
  hblr_GetGrayedButtons,
  hblr_SetGrayedButtons,
  hblr_GetModifyPrepared,
  hblr_SetModifyPrepared,
  hblr_GetReportType,
  hblr_SetReportType,
  hblr_GetShowProgress,
  hblr_SetShowProgress,
  hblr_GetDoublePass,
  hblr_SetDoublePass,
  hblr_GetModalPreview,
  hblr_SetModalPreview,
  hblr_SetPrinter,
  hblr_GetErrorMsg,
  hblr_SetOnClosePreview,
  hblr_GetOnClosePreview,
  hblr_GetPageCount,
  hblr_SetMargins;

{$R *.res}

begin

end.
