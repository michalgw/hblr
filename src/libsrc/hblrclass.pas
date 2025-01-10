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

unit hblrclass;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, LR_Class, LR_DSet, smpassocar, Forms, LR_View;

const
  NAME_SEPARATOR = ':';
  SAFE_SEPARATOR = '_';

type
  THbDataset = class(TfrDataset)
  public
    IsError: Boolean;
    ExprCheckEOF: String;
    ExprFirst: String;
    ExprNext: String;
    function Eof: Boolean; override;
    procedure First; override;
    procedure Next; override;
  end;

  { THBLRObj }

  THBLRObj = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FData: TSmpAssocArray;
    FDatasets: TList;
    FOldCloseEvent: TCloseEvent;
    function DatasetByName(ADataSet: String): TfrUserDataset;
    function DecodeName(AName: String; out ANames: TStringList; ASeparator: Char = NAME_SEPARATOR): Boolean;
    function ValueExist(AName: String): Boolean;
    function GetByName(AName: String): TSmpAssocArray;
    procedure DSCheckEof(ASender: TObject; var Eof: Boolean);
    procedure DSFirst(ASender: TObject);
    procedure DSNext(ASender: TObject);
    procedure FRGetValue(const ParName: String; var ParValue: Variant);
    procedure FRUserFunction(const Name: String; p1, p2, p3: Variant;
                             var Val: Variant);
    procedure OnClosePreview(Sender: TObject; var CloseAction: TCloseAction);
    procedure ReportBeforePreview(var PrForm: TfrPreviewForm);
  public
    Report: TfrReport;
    LastErrorMsg: String;
    OnClosePrev: String;
    PreviewFormVisible: Boolean;
    constructor CreateC(AOwner: TComponent; AComposite: Boolean = False);
    function AddValue(AVariable: Boolean; AValueName: String; AValue: Variant): Integer;
    function AddDataset(ADatasetName: String): Integer;
    function AddHbDataset(ADatasetName: String; AExprCheckEof, AExprFirst, AExprNext: String): Integer;
    function GetRowCount(ATable: String): Integer;
    function ClearData: Integer;
    function DeleteData(AName: String): Integer;

    function AddReport(AReport: TfrReport): Integer;
    function ClearReports: Integer;

    function DesignReport: Integer;
    function PrepareReport: Integer;
    function ShowPreparedReport: Integer;
    function ShowReport: Integer;
    function EditPreparedReport(APageIndex: Integer): Integer;
    function PrintPreparedReport(APageNumber: String; ACopies: Integer): Integer;

    function SetPrinter(APrinterName: String): Integer;

    function LoadPreparedReport(AFileName: String): Integer;
    function SavePreparedReport(AFileName: String): Integer;
    function LoadFromFile(AFileName: String): Integer;
    function SaveToFile(AFileName: String): Integer;
    function LoadFromXMLFile(AFileName: String): Integer;
    function SaveToXMLFile(AFileName: String): Integer;
    function LoadFromMemory(ASource: Pointer; ALength: Integer): Integer;
    function LoadFromXMLMemory(ASource: Pointer; ALength: Integer): Integer;
  end;

var
  HBLRObj: THBLRObj;

implementation

{$R *.lfm}

uses
  Printers, hblrintf, Variants, Windows;

{ TTHBLRObj }

function THBLRObj.AddDataset(ADatasetName: String): Integer;
var
  DS: TfrUserDataset;
begin
  DS := TfrUserDataset.Create(Self);
  DS.Name := StringReplace(UpperCase(ADatasetName), NAME_SEPARATOR, SAFE_SEPARATOR, [rfReplaceAll]);
  DS.Tag := 0;
  DS.OnCheckEOF := DSCheckEof;
  DS.OnFirst := DSFirst;
  DS.OnNext := DSNext;
  FDatasets.Add(DS);
  Result := 0;
end;

function THBLRObj.AddHbDataset(ADatasetName: String; AExprCheckEof, AExprFirst,
  AExprNext: String): Integer;
var
  DS: THbDataset;
begin
  if (ADatasetName = '') or (AExprCheckEof = '') or (AExprFirst = '') or
    (AExprNext = '') then
  begin
    Result := -1;
    Exit;
  end;
  DS := THbDataset.Create(Self);
  DS.Name := StringReplace(UpperCase(ADatasetName), NAME_SEPARATOR, SAFE_SEPARATOR, [rfReplaceAll]);
  DS.IsError := False;
  DS.ExprCheckEOF := AExprCheckEof;
  DS.ExprFirst := AExprFirst;
  DS.ExprNext := AExprNext;
  FDatasets.Add(DS);
  Result := 0;
end;

function THBLRObj.AddReport(AReport: TfrReport): Integer;
begin
  if Report is TfrCompositeReport then
  begin
    TfrCompositeReport(Report).Reports.Add(AReport);
    Result := 0;
  end
  else
    Result := -3;
end;

function THBLRObj.AddValue(AVariable: Boolean; AValueName: String; AValue: Variant): Integer;
var
  A: TSmpAssocArray;
  I: Integer;
  FNames: TStringList;
begin
  if AVariable then
  begin
    frVariables[AValueName] := AValue;
    Result := 0;
  end
  else
  begin
    Result := -3;
    AValueName := UpperCase(AValueName);
    if DecodeName(AValueName, FNames) then
    begin
      A := FData[FNames[0]];
      if FNames.Count  > 1 then
        for I := 1 to FNames.Count - 1 do
          A := A[FNames[I]];
      A.Value := AValue;
      Result := 0;
    end;
    FNames.Free;
  end;
end;

function THBLRObj.ClearData: Integer;
begin
  while FDatasets.Count > 0 do
  begin
    TfrDataset(FDatasets[0]).Free;
    FDatasets.Delete(0);
  end;
  FData.Clear;
  Result := 0;
end;

function THBLRObj.ClearReports: Integer;
begin
  if Report is TfrCompositeReport then
  begin
    TfrCompositeReport(Report).Reports.Clear;
    Result := 0;
  end
  else
    Result := -3;
end;

constructor THBLRObj.CreateC(AOwner: TComponent; AComposite: Boolean);
begin
  inherited Create(AOwner);
  if AComposite then
    Report := TfrCompositeReport.Create(Self)
  else
    Report := TfrReport.Create(Self);
  Report.OnBeforePreview := ReportBeforePreview;
end;

procedure THBLRObj.DataModuleCreate(Sender: TObject);
begin
  FData := TSmpAssocArray.Create;
  FDatasets := TList.Create;
  Report.OnGetValue := FRGetValue;
  Report.OnUserFunction := FRUserFunction;
  Report.PreviewButtons := Report.PreviewButtons - [pbHelp];
  OnClosePrev := '';
end;

procedure THBLRObj.DataModuleDestroy(Sender: TObject);
begin
  ClearData;
  FDatasets.Free;
  FData.Free;
  Report.Free;
end;

function THBLRObj.DatasetByName(ADataSet: String): TfrUserDataset;
var
  I: Integer;
begin
  ADataSet := UpperCase(ADataSet);
  Result := nil;
  for I := 0 to FDatasets.Count - 1 do
    if (TObject(FDatasets[I]) is TfrUserDataset) and (TfrUserDataset(FDatasets[I]).Name = ADataSet) then
    begin
      Result := TfrUserDataset(FDatasets[I]);
      Exit;
    end;
end;

function THBLRObj.DecodeName(AName: String; out ANames: TStringList; ASeparator: Char): Boolean;
var
  I: Integer;
begin
  ANames := TStringList.Create;
  ANames.Clear;
  ANames.Delimiter := ASeparator;
  ANames.DelimitedText := AName;
  for I := 0 to ANames.Count - 1 do
    ANames[I] := Trim(ANames[I]);
  Result := ANames.Count > 0;
end;

function THBLRObj.DeleteData(AName: String): Integer;
var
  It: TSmpAssocArray;
begin
  AName := UpperCase(AName);
  It := GetByName(AName);
  if (It <> nil) and (It.Parent <> nil) then
  begin
    It.Parent.Delete(It.Name);
    Result := 0;
  end
  else
  begin
    Result := -3;
    LastErrorMsg := 'Element not found';
  end;
end;

function THBLRObj.DesignReport: Integer;
begin
  //if Assigned(frDesigner) and frDesigner.Visible then
  //  frDesigner.Visible := False;
  Report.DesignReport;
  Result := 0;
end;

procedure THBLRObj.DSCheckEof(ASender: TObject; var Eof: Boolean);
var
  DS1, DS2: TfrUserDataset;
  FNames: TStringList;
begin
  Eof := True;
  if DecodeName(TfrUserDataset(ASender).Name, FNames, SAFE_SEPARATOR) then
  begin
    case FNames.Count of
      // Master dataset
      1: Eof := TfrUserDataset(ASender).Tag >= GetRowCount(FNames[0]);
      // Detail dataset
      2: begin
        DS1 := DatasetByName(FNames[0]);
        if (DS1 <> nil) and ValueExist(FNames[0] + NAME_SEPARATOR +
          IntToStr(DS1.Tag) + NAME_SEPARATOR + FNames[1]) then
        begin
          Eof := TfrUserDataset(ASender).Tag >= GetRowCount(FNames[0] +
            NAME_SEPARATOR + IntToStr(DS1.Tag) + NAME_SEPARATOR + FNames[1]);
        end;
      end;
      // Subdetail dataset
      3: begin
        DS1 := DatasetByName(FNames[0]);
        DS2 := DatasetByName(FNames[0] + SAFE_SEPARATOR + FNames[1]);
        if (DS1 <> nil) and (DS2 <> nil) and ValueExist(FNames[0] + NAME_SEPARATOR +
          IntToStr(DS1.Tag) + NAME_SEPARATOR + FNames[1] + NAME_SEPARATOR + IntToStr(DS2.Tag) +
          NAME_SEPARATOR + FNames[2]) then
        begin
          Eof := TfrUserDataset(ASender).Tag >= GetRowCount(FNames[0] +
            NAME_SEPARATOR + IntToStr(DS1.Tag) + NAME_SEPARATOR + FNames[1] +
            NAME_SEPARATOR + IntToStr(DS2.Tag) + NAME_SEPARATOR + FNames[2]);
        end;
      end;
    end;
  end;
  FNames.Free;
end;

procedure THBLRObj.DSFirst(ASender: TObject);
begin
  TfrUserDataset(ASender).Tag := 0;
end;

procedure THBLRObj.DSNext(ASender: TObject);
begin
  TfrUserDataset(ASender).Tag := TfrUserDataset(ASender).Tag + 1;
end;

function THBLRObj.EditPreparedReport(APageIndex: Integer): Integer;
begin
  Report.EditPreparedReport(APageIndex);
  Result := 0;
end;

procedure THBLRObj.FRGetValue(const ParName: String; var ParValue: Variant);
var
  DS1, DS2, DS3: TfrUserDataset;
  FNames: TStringList;
  PN: String;
begin
  PN := Trim(ParName);
  if (PN[1] = '{') and  (PN[Length(PN)] = '}') then
    ParValue := HbEval(Copy(PN, 2, Length(PN) - 2), [], True)
  else
    if ValueExist(ParName) then
      ParValue := GetByName(ParName).Value
    else
    begin
      if DecodeName(UpperCase(ParName), FNames) then
        case FNames.Count of
          // Master dataset
          2: begin
            DS1 := DatasetByName(FNames[0]);
            if (DS1 <> nil) and ValueExist(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
              NAME_SEPARATOR + FNames[1]) then
              ParValue := GetByName(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
                NAME_SEPARATOR + FNames[1]).Value;
          end;
          // Detail dataset
          3: begin
            DS1 := DatasetByName(FNames[0]);
            DS2 := DatasetByName(FNames[0] + SAFE_SEPARATOR + FNames[1]);
            if (DS1 <> nil) and (DS2 <> nil) and ValueExist(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
              NAME_SEPARATOR + FNames[1] + NAME_SEPARATOR + IntToStr(DS2.Tag) +
              NAME_SEPARATOR + FNames[2]) then
              ParValue := GetByName(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
                NAME_SEPARATOR + FNames[1] + NAME_SEPARATOR + IntToStr(DS2.Tag) +
                NAME_SEPARATOR + FNames[2]).Value;
          end;
          // Subdetail dataset
          4: begin
            DS1 := DatasetByName(FNames[0]);
            DS2 := DatasetByName(FNames[0] + SAFE_SEPARATOR + FNames[1]);
            DS3 := DatasetByName(FNames[0] + SAFE_SEPARATOR + FNames[1] + SAFE_SEPARATOR + FNames[2]);
            if (DS1 <> nil) and (DS2 <> nil) and (DS3 <> nil) and ValueExist(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
              NAME_SEPARATOR + FNames[1] + NAME_SEPARATOR + IntToStr(DS2.Tag) +
              NAME_SEPARATOR + FNames[2] + NAME_SEPARATOR + IntToStr(DS3.Tag) +
              NAME_SEPARATOR + FNames[3]) then
              ParValue := GetByName(FNames[0] + NAME_SEPARATOR + IntToStr(DS1.Tag) +
                NAME_SEPARATOR + FNames[1] + NAME_SEPARATOR + IntToStr(DS2.Tag) +
                NAME_SEPARATOR + FNames[2] + NAME_SEPARATOR + IntToStr(DS3.Tag) +
                NAME_SEPARATOR + FNames[3]).Value;
          end;
        end;
      FNames.Free;
    end;
end;

function RemoveQuotes(AStr: String): String;
begin
  if AStr[1] in ['''', '"'] then
    Result := Copy(AStr, 2, Length(AStr));
  if Result[Length(Result)] in ['''', '"'] then
    Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure THBLRObj.FRUserFunction(const Name: String; p1, p2, p3: Variant;
  var Val: Variant);
begin
  if UpperCase(Name) = 'EVAL' then
    Val:=HbEval(RemoveQuotes(Trim(p1)), [p2, p3])
  else
  if UpperCase(Name) = 'ROWCOUNT' then
    Val := GetRowCount(RemoveQuotes(Name))
  else
    if UpperCase(Name) = 'VALUEEXIST' then
      if ValueExist(RemoveQuotes(p1)) then
        Val := 1
      else
        Val := 0;
end;

function THBLRObj.GetByName(AName: String): TSmpAssocArray;
var
  A: TSmpAssocArray;
  I: Integer;
  FNames: TStringList;
begin
  Result := Nil;
  AName := UpperCase(AName);
  if DecodeName(AName, FNames) then
  begin
    A := FData.ByName(FNames[0]);
    I := 1;
    while (A <> nil) and (I < FNames.Count) do
    begin
      A := A.ByName(FNames[I]);
      Inc(I);
    end;
    if I = FNames.Count then
      Result := A;
  end;
  FNames.Free;
end;

function THBLRObj.GetRowCount(ATable: String): Integer;
var
  A: TSmpAssocArray;
begin
  A := GetByName(ATable);
  if A <> nil then
  begin
    Result := A.Count;
  end
  else
    Result := 0;
end;

function THBLRObj.LoadFromFile(AFileName: String): Integer;
begin
  Report.LoadFromFile(AFileName);
  Result := 0;
end;

function THBLRObj.LoadFromMemory(ASource: Pointer; ALength: Integer): Integer;
var
  MS: TMemoryStream;
  Dst: Pointer;
begin
  MS := TMemoryStream.Create;
  try
    MS.Size := ALength;
    Dst := MS.Memory;
    Move(ASource^, Dst^, ALength);
    Report.LoadFromStream(MS);
    Result := 0;
  except
    on E: Exception do
    begin
      LastErrorMsg := E.Message;
      Result := -3;
    end;
  end;
  MS.Free;
end;

function THBLRObj.LoadFromXMLMemory(ASource: Pointer; ALength: Integer
  ): Integer;
var
  MS: TMemoryStream;
  Dst: Pointer;
begin
  MS := TMemoryStream.Create;
  try
    MS.Size := ALength;
    Dst := MS.Memory;
    Move(ASource^, Dst^, ALength);
    Report.LoadFromXMLStream(MS);
    Result := 0;
  except
    on E: Exception do
    begin
      LastErrorMsg := E.Message;
      Result := -3;
    end;
  end;
  MS.Free;
end;

function THBLRObj.LoadPreparedReport(AFileName: String): Integer;
begin
  Report.LoadPreparedReport(AFileName);
  Result := 0;
end;

procedure THBLRObj.OnClosePreview(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  PreviewFormVisible := False;
  if Assigned(FOldCloseEvent) then
    FOldCloseEvent(Sender, CloseAction);
  if OnClosePrev <> '' then
    HbEval(OnClosePrev, [], True);
end;

procedure THBLRObj.ReportBeforePreview(var PrForm: TfrPreviewForm);
begin
  FOldCloseEvent := PrForm.OnClose;
  PrForm.OnClose := OnClosePreview;
  PrForm.ShowInTaskBar := stAlways;
  PrForm.BorderIcons := PrForm.BorderIcons + [biMinimize];
  PrForm.Icon.LoadFromResourceName(HInstance, 'FRMICON');
  PreviewFormVisible := True;
end;

function THBLRObj.PrepareReport: Integer;
begin
  if Report.PrepareReport then
    Result := 0
  else
    Result := -3;
end;

function THBLRObj.PrintPreparedReport(APageNumber: String;
  ACopies: Integer): Integer;
begin
  Report.PrintPreparedReport(APageNumber, ACopies);
  Result := 0;
end;

function THBLRObj.SavePreparedReport(AFileName: String): Integer;
begin
  Report.SavePreparedReport(AFileName);
  Result := 0;
end;

function THBLRObj.SaveToFile(AFileName: String): Integer;
begin
  Report.SaveToFile(AFileName);
  Result := 0;
end;

function THBLRObj.LoadFromXMLFile(AFileName: String): Integer;
begin
  Report.LoadFromXMLFile(AFileName);
  Result := 0;
end;

function THBLRObj.SaveToXMLFile(AFileName: String): Integer;
begin
  Report.SaveToXMLFile(AFileName);
  Result := 0;
end;

function THBLRObj.SetPrinter(APrinterName: String): Integer;
var
  ActIdx, Idx: Integer;
begin
  ActIdx := Printer.PrinterIndex;
  Idx := Printer.Printers.IndexOf(APrinterName);
  if (Idx >= 0) and Report.ChangePrinter(ActIdx, Idx) then
    Result := 0
  else
    Result := -3;
end;

function THBLRObj.ShowPreparedReport: Integer;
begin
  Report.ShowPreparedReport;
  Application.ProcessMessages;
  Result := 0;
end;

function THBLRObj.ShowReport: Integer;
begin
  Report.ShowReport;
  Application.ProcessMessages;
  Result := 0;
end;

function THBLRObj.ValueExist(AName: String): Boolean;
begin
  Result := Assigned(GetByName(UpperCase(AName)));
end;

{ THbDataset }

const
  ERR_MSG_FMT = 'An error occurred while performing Harbour code in dataset:' + #13#10 +
    'Dataset name: %s' + #13#10 +
    'Expression: %s';

function THbDataset.Eof: Boolean;
var
  V: Variant;
begin
  Result := True;
  if (not IsError) and (ExprCheckEOF <> '') then
  try
    V := HbEval(ExprCheckEOF, [], True);
    if VarIsType(V, varBoolean) then
      Result := V
    else
      if VarIsNumeric(V) then
        Result := V <> 0;
  except
    on E: Exception do
    begin
      IsError := True;
      Application.MessageBox(PChar(Format(ERR_MSG_FMT, [Name, ExprCheckEOF])), 'Error', MB_OK + MB_ICONERROR);
    end;
  end;
end;

procedure THbDataset.First;
begin
  if (not IsError) and (ExprFirst <> '') then
  try
    HbEval(ExprFirst, [], True);
  except
    on E: Exception do
    begin
      IsError := True;
      Application.MessageBox(PChar(Format(ERR_MSG_FMT, [Name, ExprFirst])), 'Error', MB_OK + MB_ICONERROR);
    end;
  end;
end;

procedure THbDataset.Next;
begin
  if (not IsError) and (ExprNext <> '') then
  try
    HbEval(ExprNext, [], True);
  except
    on E: Exception do
    begin
      IsError := True;
      Application.MessageBox(PChar(Format(ERR_MSG_FMT, [Name, ExprNext])), 'Error', MB_OK + MB_ICONERROR);
    end;
  end;
end;

end.
