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

unit hblrintf;

{$MODE Delphi}
{$PACKRECORDS C}

interface

uses
  Classes, Win32Int, hblrclass;

type
  Thb_dynsymFindName = function(AName: PChar): Pointer; cdecl;
  Thb_dynsymSymbol = function(ASym: Pointer): Pointer; cdecl;
  Thb_vmPushSymbol = procedure(ASym: Pointer); cdecl;
  Thb_vmPushNil = procedure; cdecl;
  Thb_vmPushString = procedure(AStr: PChar; ALen: LongInt); cdecl;
  Thb_vmPushNumber = procedure(AVal: Double; ADec: LongInt); cdecl;
  Thb_vmPushLogical = procedure(AVal: LongBool); cdecl;
  Thb_vmPushDate = procedure(AVal: LongInt); cdecl;
  Thb_vmPushItemRef = procedure(AItem: Pointer); cdecl;
  Thb_vmPushTimeStamp = procedure(AJulian, AMSec: LongInt); cdecl;
  Thb_vmFunction = procedure(AVal: Word); cdecl;
  Thb_vmDo = procedure(AVal: Word); cdecl;
  Thb_vmRequestReenter = function: LongBool; cdecl;
  Thb_vmRequestRestore = procedure; cdecl;
  Thb_vmRequestQuery = function: Word; cdecl;
  Thb_parinfo = function(AVal: LongInt): LongWord; cdecl;
  Thb_parc = function(AVal: LongInt): PChar; cdecl;
  Thb_parclen = function(AVal: LongInt): LongInt; cdecl;
  Thb_parl = function(AVal: LongInt): LongBool; cdecl;
  Thb_pardl = function(AVal: LongInt): LongInt; cdecl;
  Thb_parnd = function(AVal: LongInt): Double; cdecl;
  Thb_parni = function(AVal: LongInt): LongInt; cdecl;
  Thb_partd = function(AVal: LongInt): Double; cdecl;
  Thb_dateDecode = procedure(AVal: LongInt; var Y, M, D: LongInt); cdecl;
  Thb_dateEncode = function(Y, M, D: LongInt): LongInt; cdecl;
  Thb_timeStampUnpackDT = procedure(ATimeStamp: Double; var Yulian, MiliSec: Longint); cdecl;
  Thb_timeStampPack = function(Y, M, D, H, Mi, S, MS: Integer): Double; cdecl;
  Thb_timeStampUnpack = procedure(ATimeStamp: Double; var Y, M, D, H, Mi, S, MS: Integer); cdecl;
  Thb_xvmSeqBegin = procedure; cdecl;
  Thb_xvmSeqEnd = function: LongBool; cdecl;
  Thb_xvmSeqRecover = function: LongBool; cdecl;
  Thb_xvmSeqEndTest = function: LongBool; cdecl;
  Thb_stackPop = procedure; cdecl;
  Thb_errorBlock = function: Pointer; cdecl;
  Thb_itemRelease = function(AItem: Pointer): LongBool; cdecl;
  Thb_itemClone = function(AItem: Pointer): Pointer; cdecl;

  PHbFunctions = ^THbFunctions;
  THbFunctions = packed record
    hb_dynsymFindName: Thb_dynsymFindName;
    hb_dynsymSymbol: Thb_dynsymSymbol;
    hb_vmPushSymbol: Thb_vmPushSymbol;
    hb_vmPushNil: Thb_vmPushNil;
    hb_vmPushString: Thb_vmPushString;
    hb_vmPushNumber: Thb_vmPushNumber;
    hb_vmPushLogical: Thb_vmPushLogical;
    hb_vmPushDate: Thb_vmPushDate;
    hb_vmPushItemRef: Thb_vmPushItemRef;
    hb_vmPushTimeStamp: Thb_vmPushTimeStamp;
    hb_vmFunction: Thb_vmFunction;
    hb_vmDo: Thb_vmDo;
    hb_vmRequestReenter: Thb_vmRequestReenter;
    hb_vmRequestRestore: Thb_vmRequestRestore;
    hb_vmRequestQuery: Thb_vmRequestQuery;
    hb_parinfo: Thb_parinfo;
    hb_parc: Thb_parc;
    hb_parclen: Thb_parclen;
    hb_parl: Thb_parl;
    hb_pardl: Thb_pardl;
    hb_parnd: Thb_parnd;
    hb_parni: Thb_parni;
    hb_partd: Thb_partd;
    hb_dateDecode: Thb_dateDecode;
    hb_dateEncode: Thb_dateEncode;
    hb_timeStampUnpackDT: Thb_timeStampUnpackDT;
    hb_timeStampPack: Thb_timeStampPack;
    hb_timeStampUnpack: Thb_timeStampUnpack;
    hb_xvmSeqBegin: Thb_xvmSeqBegin;
    hb_xvmSeqEnd: Thb_xvmSeqEnd;
    hb_xvmSeqRecover: Thb_xvmSeqRecover;
    hb_xvmSeqEndTest: Thb_xvmSeqEndTest;
    hb_stackPop: Thb_stackPop;
    hb_errorBlock: Thb_errorBlock;
    hb_itemRelease: Thb_itemRelease;
    hb_itemClone: Thb_itemClone;
  end;

function hblr_Init(AWinHandle: PtrUInt; ACodePage: PChar; AFunctions: PHbFunctions): Integer; stdcall;
function hblr_ProcessMessages: Integer; StdCall;

function hblr_New(AComposite: LongBool): Pointer; stdcall;
function hblr_Free(AHandle: Pointer): Integer; stdcall;

function hblr_AddValueC(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: PChar): Integer; stdcall;
function hblr_AddValueNI(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: Integer): Integer; stdcall;
function hblr_AddValueNF(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: Double): Integer; stdcall;
function hblr_AddValueL(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: LongBool): Integer; stdcall;
function hblr_AddValueD(AHandle: Pointer; AVariable: LongBool; AName: PChar; AYear, AMonth, ADay: Integer): Integer; stdcall;

function hblr_AddDataset(AHandle: Pointer; AName: PChar): Integer; stdcall;
function hblr_AddHbDataset(AHandle: Pointer; AName, AExprCheckEof, AExprFirst, AExprNext: PChar): Integer; stdcall;

function hblr_GetRowCount(AHandle: Pointer; AName: PChar): Integer; stdcall;

function hblr_ClearData(AHandle: Pointer): Integer; stdcall;

function hblr_AddReport(AHandle: Pointer; AReport: Pointer): Integer; stdcall;
function hblr_ClearReports(AHandle: Pointer): Integer; stdcall;

function hblr_LoadFromFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_SaveToFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_LoadFromXMLFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_SaveToXMLFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_LoadPreparedReport(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_SavePreparedReport(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
function hblr_LoadFromMemory(AHandle: Pointer; AData: Pointer; ALength: Integer): Integer; stdcall;
function hblr_LoadFromXMLMemory(AHandle: Pointer; AData: Pointer; ALength: Integer): Integer; stdcall;

function hblr_PrepareReport(AHandle: Pointer): Integer; stdcall;
function hblr_ShowReport(AHandle: Pointer): Integer; stdcall;
function hblr_ShowPreparedReport(AHandle: Pointer): Integer; stdcall;
function hblr_PrintPreparedReport(AHandle: Pointer; APages: PChar; ACopies: Integer): Integer; stdcall;
function hblr_DesignReport(AHandle: Pointer): Integer; stdcall;
function hblr_EditPreparedReport(AHandle: Pointer; APageIndex: Integer): Integer; stdcall;

function hblr_GetPageCount(AHandle: Pointer; var AValue: Integer): Integer; stdcall;
function hblr_SetMargins(AHandle: Pointer; APage: Integer; ALeft, ARight, ATop, ABottom: Integer): Integer; stdcall;

function hblr_ClosePreview(AHandle: Pointer): Integer; stdcall;
function hblr_IsPreviewVisible(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;

function hblr_GetTitle(AHandle: Pointer; ATitle: PChar): Integer; stdcall;
function hblr_SetTitle(AHandle: Pointer; ATitle: PChar): Integer; stdcall;
function hblr_GetInitialZoom(AHandle: Pointer; var AZoom: Integer): Integer; stdcall;
function hblr_SetInitialZoom(AHandle: Pointer; AZoom: Integer): Integer; stdcall;
function hblr_GetGrayedButtons(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
function hblr_SetGrayedButtons(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
function hblr_GetModifyPrepared(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
function hblr_SetModifyPrepared(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
function hblr_GetReportType(AHandle: Pointer; var AType: Integer): Integer; stdcall;
function hblr_SetReportType(AHandle: Pointer; AType: Integer): Integer; stdcall;
function hblr_GetShowProgress(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
function hblr_SetShowProgress(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
function hblr_GetDoublePass(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
function hblr_SetDoublePass(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
function hblr_GetModalPreview(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
function hblr_SetModalPreview(AHandle: Pointer; AValue: LongBool): Integer; stdcall;

function hblr_GetOnClosePreview(AHandle: Pointer; AEvent: PChar): Integer; stdcall;
function hblr_SetOnClosePreview(AHandle: Pointer; AEvent: PChar): Integer; stdcall;

function hblr_SetPrinter(AHandle: Pointer; APrinterName: PChar): Integer; stdcall;

function hblr_GetErrorMsg(AHandle: Pointer; AMessage: PChar): Integer; stdcall;

function HbEval(AExpr: String; AParams: array of const; DoExec: Boolean = False): Variant;

implementation

uses
  SysUtils, Forms, LR_View, LR_Class, ExtCtrls, Variants, DateUtils, Windows,
  LR_Prntr, Printers, LR_Desgn, LR_E_HTM, LR_E_CSV, LR_E_TXT, LR_RRect, LR_BarC,
  LR_Shape, LR_ChBox, LR_e_img, LR_e_htmldiv, lrPDFExport, LConvEncoding,
  Translations;

type

  { TMainFormHandler }

  TMainFormHandler = class
    procedure OnGetMainHandle(var Handle: HWND);
  end;

var
  ReportList: TList;
  ConvertCodePage: String = '';
  HbFunc: THbFunctions;
  MainFormHandle: HWND;
  MFHandler: TMainFormHandler = nil;

  Designer: TfrDesigner = nil;
  frCheckBoxObject: TfrCheckBoxObject = nil;
  frHtmlDivExport: TfrHtmlDivExport = nil;
  frImageExport: TfrImageExport = nil;
  frShapeObject: TfrShapeObject = nil;
  frBarCodeObject: TfrBarCodeObject = nil;
  frRoundRectObject: TfrRoundRectObject = nil;
  frTextExport: TfrTextExport = nil;
  frCSVExport: TfrCSVExport = nil;
  frHTMExport: TfrHTMExport = nil;
  lrPDFExport: TlrPDFExport = nil;

function ConvertFrom(ASrc: PChar): String; inline;
var
  Enc: Boolean = False;
begin
  if ConvertCodePage <> '' then
    Result := ConvertEncodingToUTF8(ASrc, ConvertCodePage, Enc);
  if not Enc then
    Result := ASrc;
end;

function ConverTo(ASrc: String): PChar; inline;
var
  Enc: Boolean = False;
begin
  Result := nil;
  if ConvertCodePage <> '' then
    Result := StrNew(PChar(ConvertEncodingFromUTF8(ASrc, ConvertCodePage, Enc)));
  if not Enc then
  begin
    if Assigned(Result) then
      StrDispose(Result);
    Result := StrNew(PChar(ASrc));
  end;
end;

procedure InitPrn;
begin
  //Prn := TfrPrinter.Create;
  Prn.Printer := Printer;
end;

function CheckHandle(AHandle: Pointer): Boolean;
begin
  Result := (AHandle <> nil) and (ReportList.IndexOf(AHandle) >= 0);
end;

function hblr_Init(AWinHandle: PtrUInt; ACodePage: PChar;
  AFunctions: PHbFunctions): Integer; stdcall;
var
  Lang, FallbackLang, PODir: String;
begin
  try
    MainFormHandle := AWinHandle;
    MFHandler := TMainFormHandler.Create;
    Application.OnGetMainFormHandle := MFHandler.OnGetMainHandle;
    Application.Initialize;
    Application.MainFormOnTaskBar := True;
    InitPrn;
    ConvertCodePage := ACodePage;
    HbFunc := AFunctions^;

    Designer := TfrDesigner.Create(nil);
    frCheckBoxObject := TfrCheckBoxObject.Create(nil);
    frHtmlDivExport := TfrHtmlDivExport.Create(nil);
    frImageExport := TfrImageExport.Create(nil);
    frShapeObject := TfrShapeObject.Create(nil);
    frBarCodeObject := TfrBarCodeObject.Create(nil);
    frRoundRectObject := TfrRoundRectObject.Create(nil);
    frTextExport := TfrTextExport.Create(nil);
    frCSVExport := TfrCSVExport.Create(nil);
    frHTMExport := TfrHTMExport.Create(nil);
    lrPDFExport := TlrPDFExport.Create(nil);

    PODir := 'languages\';
    with GetLanguageID do
    begin
      Lang := LanguageID;
      FallbackLang := LanguageCode;
    end;
    Translations.TranslateUnitResourceStrings('LCLStrConsts', PODir + 'lclstrconsts.%s.po', Lang, FallbackLang);
    Translations.TranslateUnitResourceStrings('LR_Const', PODir + 'lr_const.%s.po', Lang, FallbackLang);

    Result := 0;
  except
    Result := -2;
  end;
end;

function hblr_ProcessMessages: Integer; StdCall;
begin
  try
    Application.ProcessMessages;
    Result := 0;
  except
    Result := -2;
  end;
end;

function hblr_New(AComposite: LongBool): Pointer; stdcall;
var
  HObj: THBLRObj;
begin
  Result := nil;
  try
    HObj := THBLRObj.CreateC(nil, AComposite);
    ReportList.Add(HObj);
    Result := HObj;
  except

  end;
end;

function hblr_Free(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      ReportList.Remove(AHandle);
      THBLRObj(AHandle).Free;
      Result := 0;
    end
    else
      Result := -1;
  except
    Result := -2;
  end;
end;

function hblr_AddValueC(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddValue(AVariable, ConvertFrom(AName), ConvertFrom(AValue))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddValueNI(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddValue(AVariable, ConvertFrom(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddValueNF(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: Double): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddValue(AVariable, ConvertFrom(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddValueL(AHandle: Pointer; AVariable: LongBool; AName: PChar; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddValue(AVariable, ConvertFrom(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddValueD(AHandle: Pointer; AVariable: LongBool; AName: PChar; AYear, AMonth, ADay: Integer): Integer; stdcall;
var
  D: TDateTime;
begin
  try
    if CheckHandle(AHandle) then
      if TryEncodeDate(AYear, AMonth, ADay, D) then
        Result := THBLRObj(AHandle).AddValue(AVariable, ConvertFrom(AName), D)
      else
      begin
        THBLRObj(AHandle).LastErrorMsg := 'Invalid date';
        Result := -3
      end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddDataset(AHandle: Pointer; AName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddDataset(ConvertFrom(AName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddHbDataset(AHandle: Pointer; AName, AExprCheckEof, AExprFirst, AExprNext: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).AddHbDataset(ConvertFrom(AName), ConvertFrom(AExprCheckEof),
        ConvertFrom(AExprFirst), ConvertFrom(AExprNext))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetRowCount(AHandle: Pointer; AName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).GetRowCount(ConvertFrom(AName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_ClearData(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).ClearData
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_AddReport(AHandle: Pointer; AReport: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) and CheckHandle(AReport) then
      Result := THBLRObj(AHandle).AddReport(THBLRObj(AReport).Report)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_ClearReports(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).ClearReports
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_LoadFromFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).LoadFromFile(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SaveToFile(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).SaveToFile(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_LoadFromXMLFile(AHandle: Pointer; AFileName: PChar): Integer;
  stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).LoadFromXMLFile(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SaveToXMLFile(AHandle: Pointer; AFileName: PChar): Integer;
  stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).SaveToXMLFile(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_LoadPreparedReport(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).LoadPreparedReport(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SavePreparedReport(AHandle: Pointer; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).SavePreparedReport(ConvertFrom(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_LoadFromMemory(AHandle: Pointer; AData: Pointer; ALength: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).LoadFromMemory(AData, ALength)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_LoadFromXMLMemory(AHandle: Pointer; AData: Pointer;
  ALength: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).LoadFromXMLMemory(AData, ALength)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_PrepareReport(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).PrepareReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_ShowReport(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).ShowReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_ShowPreparedReport(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).ShowPreparedReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_PrintPreparedReport(AHandle: Pointer; APages: PChar; ACopies: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).PrintPreparedReport(ConvertFrom(APages), ACopies)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_DesignReport(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).DesignReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_EditPreparedReport(AHandle: Pointer; APageIndex: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBLRObj(AHandle).EditPreparedReport(APageIndex)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetPageCount(AHandle: Pointer; var AValue: Integer): Integer;
  stdcall;
begin
  try
    if CheckHandle(AHandle) then
      AValue := THBLRObj(AHandle).Report.Pages.Count
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetMargins(AHandle: Pointer; APage: Integer; ALeft, ARight,
  ATop, ABottom: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle)
      and (THBLRObj(AHandle).Report.Pages.Count > 0)
      and (APage >= 0) and (APage < THBLRObj(AHandle).Report.Pages.Count) then
    begin
      THBLRObj(AHandle).Report.Pages[APage].Margins.AsRect :=
        Classes.Rect(ALeft * 18 div 5, ATop * 18 div 5, ARight * 18 div 5, ABottom * 18 div 5);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_ClosePreview(AHandle: Pointer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      //THBLRObj(AHandle).Report.ClosePreview;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_IsPreviewVisible(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).PreviewFormVisible;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetTitle(AHandle: Pointer; ATitle: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      S := ConverTo(Copy(THBLRObj(AHandle).Report.Title, 1, 255));
      StrCopy(ATitle, S);
      StrDispose(S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetTitle(AHandle: Pointer; ATitle: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.Title := ConvertFrom(ATitle);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetInitialZoom(AHandle: Pointer; var AZoom: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AZoom := Ord(THBLRObj(AHandle).Report.InitialZoom);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetInitialZoom(AHandle: Pointer; AZoom: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.InitialZoom := TfrPreviewZoom(AZoom);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetGrayedButtons(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).Report.GrayedButtons;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetGrayedButtons(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.GrayedButtons := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetModifyPrepared(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).Report.ModifyPrepared;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetModifyPrepared(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.ModifyPrepared := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetReportType(AHandle: Pointer; var AType: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AType := Ord(THBLRObj(AHandle).Report.ReportType);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetReportType(AHandle: Pointer; AType: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.ReportType := TfrReportType(AType);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetShowProgress(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).Report.ShowProgress;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetShowProgress(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.ShowProgress := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetDoublePass(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).Report.DoublePass;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetDoublePass(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.DoublePass := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetModalPreview(AHandle: Pointer; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBLRObj(AHandle).Report.ModalPreview;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetModalPreview(AHandle: Pointer; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).Report.ModalPreview := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetOnClosePreview(AHandle: Pointer; AEvent: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      S := ConverTo(Copy(THBLRObj(AHandle).OnClosePrev, 1, 255));
      StrCopy(AEvent, S);
      StrDispose(S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetOnClosePreview(AHandle: Pointer; AEvent: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBLRObj(AHandle).OnClosePrev := ConvertFrom(AEvent);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_SetPrinter(AHandle: Pointer; APrinterName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      Result := THBLRObj(AHandle).SetPrinter(ConvertFrom(APrinterName));
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hblr_GetErrorMsg(AHandle: Pointer; AMessage: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      S := ConverTo(Copy(THBLRObj(AHandle).LastErrorMsg, 1, 255));
      StrCopy(AMessage, S);
      StrDispose(S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBLRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

const
  HB_IT_INTEGER = 2;
  HB_IT_LONG = 8;
  HB_IT_DOUBLE = $10;
  HB_IT_DATE = $20;
  HB_IT_TIMESTAMP = $40;
  HB_IT_LOGICAL = $80;
  HB_IT_STRING = $400;
  HB_IT_MEMO = $400 or $800;

function HbEval(AExpr: String; AParams: array of const; DoExec: Boolean): Variant;

procedure RestoreErrorBlock(ABlock: Pointer);
begin
  if ABlock <> nil then
  begin
    HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(HbFunc.hb_dynsymFindName(PChar('ERRORBLOCK'))));
    HbFunc.hb_vmPushNil;
    HbFunc.hb_vmPushItemRef(ABlock);
    HbFunc.hb_vmFunction(1);
    HbFunc.hb_itemRelease(ABlock);
  end;
end;

var
  I: Integer;
  S: String;
  V: TVarRec;
  Pc: PChar;
  Y,M,D,H,MN,SC,MS: Integer;
  OldErrorBlock: Pointer;
  FncSym: Pointer;
  TmpY, TmpMS: LongInt;
begin
  Result := Null;
  OldErrorBlock := nil;
  if AExpr = '' then
    Exit;
  if not HbFunc.hb_vmRequestReenter then
    Exit;
  if DoExec then
    S := 'hblr_Exec'
  else
    S := 'hblr_Eval';

  OldErrorBlock := HbFunc.hb_itemClone(HbFunc.hb_errorBlock);

  FncSym := HbFunc.hb_dynsymFindName(PChar('hblr_SetErrorBlock'));
  if FncSym = nil then
  begin
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('Function not found: hblr_SetErrorBlock');
  end;
  HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(FncSym));
  HbFunc.hb_vmPushNil;
  HbFunc.hb_vmDo(0);

  HbFunc.hb_xvmSeqBegin;

  FncSym := HbFunc.hb_dynsymFindName(PChar(S));
  if FncSym = nil then
  begin
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('Function not found: ' + S);
  end;
  HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(FncSym));
  HbFunc.hb_vmPushNil;
  HbFunc.hb_vmPushString(PChar(AExpr), Length(AExpr));

  if Length(AParams) > 0 then
  begin
    for I := Low(AParams) to High(AParams) do
    begin
      V := AParams[I];
      case V.VType of
        vtInteger: HbFunc.hb_vmPushNumber(V.VInteger, 255);
        vtExtended: HbFunc.hb_vmPushNumber(V.VExtended^, 255);
        vtString: begin
          S := V.VString^;
          Pc := ConverTo(S);
          HbFunc.hb_vmPushString(Pc, StrLen(Pc));
          StrDispose(Pc);
        end;
        vtAnsiString: begin
          S := AnsiString(V.VAnsiString^);
          Pc := ConverTo(S);
          HbFunc.hb_vmPushString(Pc, StrLen(Pc));
          StrDispose(Pc);
        end;
        vtPChar: begin
          HbFunc.hb_vmPushString(V.VPChar, StrLen(V.VPChar));
        end;
        vtPWideChar: begin
          HbFunc.hb_vmPushString(V.VPChar, StrLen(V.VPChar));
        end;
        vtBoolean: begin
          HbFunc.hb_vmPushLogical(V.VBoolean);
        end;
        vtVariant:
          case VarType(V.VVariant^) of
            varDate: begin
              if TimeOf(V.VVariant^) = 0 then
                HbFunc.hb_vmPushDate(HbFunc.hb_dateEncode(YearOf(V.VVariant^),
                  MonthOf(V.VVariant^), DayOf(V.VVariant^)))
              else
              begin
                HbFunc.hb_timeStampUnpackDT(HbFunc.hb_timeStampPack(YearOf(V.VVariant^),
                  MonthOf(V.VVariant^), DayOf(V.VVariant^), HourOf(V.VVariant^),
                  MinuteOf(V.VVariant^), SecondOf(V.VVariant^), MilliSecondOf(V.VVariant^)),
                  TmpY, TmpMS);
                HbFunc.hb_vmPushTimeStamp(TmpY, TmpMS);
              end;
            end;
            varString: begin
              S := V.VVariant^;
              Pc := ConverTo(S);
              HbFunc.hb_vmPushString(Pc, StrLen(Pc));
              StrDispose(Pc);
            end;
            varSmallint, varSingle, varShortInt, varInteger, varDouble,
            varCurrency, varByte, varWord, varLongWord: HbFunc.hb_vmPushNumber(V.VVariant^, 255);
            varBoolean: HbFunc.hb_vmPushLogical(V.VVariant^);
            else HbFunc.hb_vmPushNil;
          end;
        else HbFunc.hb_vmPushNil;
      end;
    end;
  end;
  //HbFunc.hb_vmDo(Length(AParams) + 1);
  HbFunc.hb_vmFunction(Length(AParams) + 1);
  if HbFunc.hb_xvmSeqEndTest then
  begin
    if HbFunc.hb_xvmSeqRecover then
      HbFunc.hb_stackPop;
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('An error occurred while executing harbour code')
  end
  else
  begin
    case HbFunc.hb_parinfo( -1 ) of
      HB_IT_INTEGER: Result := HbFunc.hb_parni( -1 );
      HB_IT_LONG, HB_IT_DOUBLE: Result := HbFunc.hb_parnd( -1 );
      HB_IT_DATE: begin
        HbFunc.hb_dateDecode( HbFunc.hb_pardl(-1), Y, M, D);
        Result := EncodeDate(Y, M, D);
      end;
      HB_IT_TIMESTAMP: begin
        HbFunc.hb_timeStampUnpack(HbFunc.hb_partd(-1), Y, M, D, H, MN, SC, MS);
        Result := EncodeDateTime(Y, M, D, H, MN, SC, MS);
      end;
      HB_IT_LOGICAL: begin
        Result := Boolean(HbFunc.hb_parl(-1));
      end;
      HB_IT_STRING, HB_IT_MEMO: begin
        Result := ConvertFrom(HbFunc.hb_parc(-1))
      end;
      else
        Result := Null;
    end;
  end;
  HbFunc.hb_vmRequestRestore;
  RestoreErrorBlock(OldErrorBlock);
end;

procedure FreeReports;
begin
  while ReportList.Count > 0 do
  begin
    THBLRObj(ReportList[0]).Free;
    ReportList.Delete(0);
  end;
end;

procedure FreeLRClasses;
begin
  if Assigned(frCheckBoxObject) then
    frCheckBoxObject.Free;
  if Assigned(frHtmlDivExport) then
    frHtmlDivExport.Free;
  if Assigned(frImageExport) then
    frImageExport.Free;
  if Assigned(frShapeObject) then
    frShapeObject.Free;
  if Assigned(frBarCodeObject) then
    frBarCodeObject.Free;
  if Assigned(frRoundRectObject) then
    frRoundRectObject.Free;
  if Assigned(frTextExport) then
    frTextExport.Free;
  if Assigned(frCSVExport) then
    frCSVExport.Free;
  if Assigned(frHTMExport) then
    frHTMExport.Free;
  if Assigned(lrPDFExport) then
    lrPDFExport.Free;
  if Assigned(Designer) then
    Designer.Free;
end;

{ TMainFormHandler }

procedure TMainFormHandler.OnGetMainHandle(var Handle: HWND);
begin
  Handle := MainFormHandle;
end;

initialization
  ReportList := TList.Create;

finalization
  FreeReports;
  FreeLRClasses;

end.
