/* hblr - LazReport for (x)Harbour

  Copyright (C) 2025 Michal Gawrycki info..gmsystems.pl

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
*/

#include "hbclass.ch"
#include "hbdyn.ch"
#include "hblr.ch"
#include "hbgtinfo.ch"

STATIC nHbLrLibHandle := NIL
STATIC nHbLrLibIdle := NIL

CREATE CLASS TLazReport
   HIDDEN:
   DATA pObjHandle
   DATA lComposite

   METHOD CheckRes( nRes )
   METHOD GetLastError()

   EXPORTED:

   // Create new report object
   // lComposite - if True, a composite report will be created
   METHOD New(lComposite) CONSTRUCTOR

   // Returns .T. if current report object is composite report
   METHOD IsComposite()


   // Add report object to composite report
   METHOD AddReport( oReport )

   // Remove all reports objects from composite report
   METHOD ClearReports()


   // Loads report from file with cFileName name. File must have .frf extention (FreeReport Form) or .lfr (LazReport Form).
   METHOD LoadFromFile( cFileName )

   // Saves report to file with cFileName name. File must have .frf extention (FreeReport Form) or .lfr (LazReport Form).
   METHOD SaveToFile( cFileName )

   // Loads report from file with cFileName name. File must be LazReport Form.
   METHOD LoadFromXMLFile( cFileName )

   // Saves report to file with cFileName name. Saves in LazReport format.
   METHOD SaveToXMLFile( cFileName )

   // Loads prepared report from file with cFileName name. File must have .frp extention (FreeReport/LazReport Prepared report).
   METHOD LoadPreparedReport( cFileName )

   // Saves prepared report to file with cFileName name. File must have .frp extetion (FreeReport/LazReport Prepared report).
   METHOD SavePreparedReport( cFileName )

   // Load FreeReport report from string. Helpful when loading a report from database
   METHOD LoadFromMemory( cData )

   // Load LazReport report from string. Helpful when loading a report from database
   METHOD LoadFromXMLMemory( cData )


   // Add value to report
   // cValueName - name of value
   // xValue - value (can be numeric, string, memo, date, timestamp, boolean)
   // lIsVariable - if true value will be assigned to FreeReport variable
   METHOD AddValue( cValueName, xValue, lIsVariable )

   // Add row to dataset
   // cTableName - dataset name - string
   // aValues - array with values or hash table
   // aNames - if aValues is simple array then aNames is string array containing column names
   METHOD AddRow( cTableName, aValues, aNames )

   // Register simple dataset
   METHOD AddDataset( cDatasetName )

   // Register harbour dataset - dataset controled by harbour side expressions or functions
   // cDatasetName - name of dataset
   // cExprCheckEOF - expression that checks end of data, should return true or false
   // cExprFirst - expression, go to first record
   // cExprNext - expression, skip to next record
   METHOD AddHbDataset( cDatasetName, cExprCheckEOF, cExprFirst, cExprNext )

   // Return row count of dataset - only for simple datasets, where data is added by AddRow
   METHOD RowCount( cTableName )

   // Remove all datasets and clear data
   METHOD ClearData()


   // Builds and shows report in preview window.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   // Prepared report clears after closing preview window.
   METHOD ShowReport()

   // Runs report designer.
   METHOD DesignReport()

   // Starts report building process. If user interrupts it, returns False.
   METHOD PrepareReport()

   // Shows prepared report in preview window.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   // Prepared report clears after closing preview window.
   METHOD ShowPreparedReport()

   // Edits page of prepared report. If no designer in compliled project, does nothing.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   METHOD EditPreparedReport( nPageIndex )

   // Prints prepared report. Printed pages taken from cPages string,
   // which can contains page numbers separated by comma, or page ranges (for example, "1,3,5-12").
   // If this string is empty, prints all pages.
   // nCopies parameter sets number of copies to print.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   METHOD PrintPreparedReport( cPages, nCopies )


   // Close preview window
   METHOD ClosePreview()

   // Returns true if preview window is visible
   METHOD IsPreviewVisible()


   // Set printer name
   METHOD SetPrinter( cPrinterName )


   // Get number of pages
   METHOD GetPageCount()

   // Set page margins
   METHOD SetMargins( nPage, nLeft, nRight, nTop, nBottom )

   DESTRUCTOR FreeLR()


   // Report title - shows in preview and progress windows and assigns to the printed job.
   // string
   ACCESS Title METHOD GetTitle
   ASSIGN Title METHOD SetTitle

   // Initial zoom for preview
   // numeric
   ACCESS InitialZoom METHOD GetInitialZoom
   ASSIGN InitialZoom METHOD SetInitialZoom

   // If this property is True, Preview window will has grayed buttons.
   // If your project contains report designer, you can set this option in "Designer options" dialog.
   // boolean
   ACCESS GrayedButtons METHOD GetGrayedButtons
   ASSIGN GrayedButtons METHOD SetGrayedButtons

   // Allows to modify prepared report
   // boolean
   ACCESS ModifyPrepared METHOD GetModifyPrepared
   ASSIGN ModifyPrepared METHOD SetModifyPrepared

   // Report type. Can be set to HBFR_RT_MULTIPLE or HBFR_RT_SIMPLE (default).
   // Multiple report uses Dataset property to build report for each record in this dataset.
   // numeric
   ACCESS ReportType METHOD GetReportType
   ASSIGN ReportType METHOD SetReportType

   // If True, shows progress window when building, printing or exporting report.
   // boolean
   ACCESS ShowProgress METHOD GetShowProgress
   ASSIGN ShowProgress METHOD SetShowProgress

   // If True, build the report twice, inserting total number of pages on the second run
   // boolean
   ACCESS DoublePass METHOD GetDoublePass
   ASSIGN DoublePass METHOD SetDoublePass

   // If this property is True, Preview window will be modal. Non-modal preview allows you to open several previews.
   // boolean
   ACCESS ModalPreview METHOD GetModalPreview
   ASSIGN ModalPreview METHOD SetModalPreview

   // expression that will be executed after closing preview window
   // string
   ACCESS OnClosePreview METHOD GetOnClosePreview
   ASSIGN OnClosePreview METHOD SetOnClosePreview

ENDCLASS

FUNCTION hblr_IsInt( nVal )
   RETURN ( Abs( nVal ) - Int( Abs( nVal ) ) ) == 0

FUNCTION hblr_ProcessMessages()

   LOCAL nRes

   IF nHbLrLibHandle == NIL
      RETURN -1
   ENDIF
   nRes := hb_DynCall( { 'hblr_ProcessMessages', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ) } )

   RETURN nRes

// Load and initialize library
// Params:
//    cLibName - library path
//    cCodePage - code page (see hblr.ch HBLR_CP_* )
FUNCTION hblr_LoadLibrary( cLibName, cCodePage )

   LOCAL nRes

   IF nHbLrLibHandle != NIL
      RETURN .T.
   ENDIF

   hb_default( @cLibName, HBLR_LIB_NAME )
   hb_default( @cCodePage, "" )

   nHbLrLibHandle := hb_libLoad( cLibName )
   IF nHbLrLibHandle == NIL
      RETURN .F.
   ELSE
      nRes := hb_DynCall( { 'hblr_Init', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
         HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_VOID_PTR }, ;
         hb_gtInfo( HB_GTI_WINHANDLE ), cCodePage, GetHbLrPasFuncs() )
      IF nRes != 0
         nHbLrLibHandle := NIL
         RETURN .F.
      ELSE
         hb_IdleAdd( { || hblr_ProcessMessages() } )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION hblr_FreeLibrary()

   IF nHbLrLibIdle != NIL
      hb_idleDel( nHbLrLibIdle )
      nHbLrLibIdle := NIL
   ENDIF
   IF nHbLrLibHandle != NIL
      hb_libFree( nHbLrLibHandle )
      nHbLrLibHandle := NIL
   ENDIF

   RETURN NIL

METHOD CheckRes( nRes ) CLASS TLazReport

   LOCAL cErrMsg, oErr

   IF nRes < 0
      DO CASE
         CASE nRes == -1
            cErrMsg := 'Invalid object handle'
         CASE nRes == -2 .OR. nRes == -3
            cErrMsg := ::GetLastError()
         CASE nRes == -4
            cErrMsg := 'Invalid argument type'
         CASE nRes == -5
            cErrMsg := 'Can not load library ' + HBLR_LIB_NAME
         CASE nRes == -6
            cErrMsg := 'Can not create report object'
         OTHERWISE
            cErrMsg := 'Unknown error'
      ENDCASE
      oErr := ErrorNew()
      oErr:genCode := nRes
      oErr:description := cErrMsg
      Break oErr
   ENDIF

   RETURN NIL

METHOD GetLastError() CLASS TLazReport

   LOCAL nRes, cError := Space( 255 )

   nRes := hb_DynCall( { 'hblr_GetErrorMsg', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, @cError)
   IF nRes = 0
      RETURN cError
   ENDIF

   RETURN ''

METHOD New( lComposite ) CLASS TLazReport

   IF !hblr_LoadLibrary()
      ::CheckRes( -5 )
   ENDIF
   IF Empty( lComposite )
      lComposite := .F.
   ENDIF
   ::lComposite := lComposite
   ::pObjHandle := hb_DynCall( { 'hblr_New', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_VOID_PTR, HB_DYN_CALLCONV_STDCALL ), ;
     HB_DYN_CTYPE_BOOL }, lComposite )
   IF ::pObjHandle == NIL
      ::CheckRes( -6 )
   ENDIF
   RETURN Self

METHOD FreeLR() CLASS TLazReport

   RETURN hb_DynCall( { 'hblr_Free', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle )

METHOD IsComposite() CLASS TLazReport

   RETURN ::lComposite

METHOD AddReport( oReport ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_AddReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle, oReport:pObjHandle ) )

   RETURN NIL

METHOD ClearReports() CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_ClearReports', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD LoadFromFile( cFileName ) CLASS TLazReport

   ::CheckRes(hb_DynCall( { 'hblr_LoadFromFile', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD SaveToFile( cFileName ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SaveToFile', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD LoadFromXMLFile( cFileName ) CLASS TLazReport

   ::CheckRes(hb_DynCall( { 'hblr_LoadFromXMLFile', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD SaveToXMLFile( cFileName ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SaveToXMLFile', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD LoadPreparedReport( cFileName ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_LoadPreparedReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD SavePreparedReport( cFileName ) CLASS TLazReport

   ::CheckRes(hb_DynCall( { 'hblr_SavePreparedReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cFileName ) )

   RETURN NIL

METHOD LoadFromMemory( cData ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_LoadFromMemory', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR + HB_DYN_ENC_RAW, HB_DYN_CTYPE_INT }, ::pObjHandle, cData, Len( cData ) ) )

   RETURN NIL

METHOD LoadFromXMLMemory( cData ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_LoadFromXMLMemory', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR + HB_DYN_ENC_RAW, HB_DYN_CTYPE_INT }, ::pObjHandle, cData, Len( cData ) ) )

   RETURN NIL

METHOD AddValue( cValueName, xValue, lIsVariable ) CLASS TLazReport

   LOCAL cParamType, nRet

   hb_default( @lIsVariable, .F. )
   cParamType := ValType( xValue )
   DO CASE
      CASE cParamType == 'C' .OR. cParamType == 'M'
         nRet := hb_DynCall( { 'hblr_AddValueC', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
            HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR }, ;
            ::pObjHandle, lIsVariable, cValueName, xValue )
      CASE cParamType == 'N'
         IF hblr_IsInt( xValue )
            nRet := hb_DynCall( { 'hblr_AddValueNI', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
               HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT }, ;
               ::pObjHandle, lIsVariable, cValueName, xValue )
         ELSE
            nRet := hb_DynCall( { 'hblr_AddValueNF', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
               HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_DOUBLE }, ;
               ::pObjHandle, lIsVariable, cValueName, xValue )
         ENDIF
      CASE cParamType == 'D'
         nRet := hb_DynCall( { 'hblr_AddValueD', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
            HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT}, ;
            ::pObjHandle, lIsVariable, cValueName, Year( xValue ), Month( xValue ), Day( xValue ) )
      CASE cParamType == 'L'
         nRet := hb_DynCall( { 'hblr_AddValueL', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
            HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_BOOL }, ;
            ::pObjHandle, lIsVariable, cValueName, xValue )
      OTHERWISE
         nRet := -4
   ENDCASE
   ::CheckRes( nRet )

   RETURN NIL

METHOD AddRow( cTableName, aValues, aNames ) CLASS TLazReport

   LOCAL nRow, i, cVType

   cVType := ValType( aValues )
   IF cVType == 'H' .AND. aNames == NIL
      aNames := hb_HKeys( aValues )
   ENDIF
   nRow := ::RowCount( cTableName )
   FOR i := 1 TO Len( aValues )
      IF cVType == 'H'
         ::AddValue( cTableName + ':' + AllTrim( Str( nRow ) ) + ':' + aNames[i], hb_HValueAt( aValues, i ) )
      ELSE
         ::AddValue( cTableName + ':' + AllTrim( Str( nRow ) ) + ':' + aNames[i], aValues[i] )
      ENDIF
   NEXT

   RETURN NIL

METHOD AddDataset( cDatasetName ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_AddDataset', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cDatasetName ) )

   RETURN NIL

METHOD AddHbDataset( cDatasetName, cExprCheckEOF, cExprFirst, cExprNext ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_AddHbDataset', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR }, ;
      ::pObjHandle, cDatasetName, cExprCheckEOF, cExprFirst, cExprNext ) )

   RETURN NIL

METHOD RowCount( cTableName ) CLASS TLazReport

   LOCAL nRes

   nRes := hb_DynCall( { 'hblr_GetRowCount', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cTableName )
   IF nRes < 0
      ::CheckRes( nRes )
   ENDIF

   RETURN nRes

METHOD ClearData() CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_ClearData', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD ShowReport()CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_ShowReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD DesignReport() CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_DesignReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD PrepareReport() CLASS TLazReport

   LOCAL nRes, lRes

   nRes := hb_DynCall( { 'hblr_PrepareReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle )
   SWITCH nRes
   CASE 0
      lRes := .T.
      EXIT
   CASE -3
      lRes := .F.
      EXIT
   OTHERWISE
      lRes := .F.
      ::CheckRes( nRes )
      EXIT
   ENDSWITCH

   RETURN lRes

METHOD ShowPreparedReport() CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_ShowPreparedReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD EditPreparedReport( nPageIndex ) CLASS TLazReport

   ::CheckRes(hb_DynCall( {'hblr_EditPreparedReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, nPageIndex ) )

   RETURN NIL

METHOD PrintPreparedReport( cPages, nCopies ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_PrintPreparedReport', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, cPages, nCopies ) )

   RETURN NIL

METHOD ClosePreview() CLASS TLazReport

   ::CheckRes(hb_DynCall( { 'hblr_ClosePreview', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR }, ::pObjHandle ) )

   RETURN NIL

METHOD IsPreviewVisible() CLASS TLazReport

   LOCAL lGButt := .F.

   ::CheckRes( hb_DynCall( { 'hblr_IsPreviewVisible', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ),;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lGButt ) )

   RETURN lGButt


METHOD SetPrinter( cPrinterName ) CLASS TLazReport

   ::CheckRes(hb_DynCall( { 'hblr_SetPrinter', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cPrinterName ) )

   RETURN NIL

METHOD GetPageCount() CLASS TLazReport

   LOCAL nPages := 0

   ::CheckRes(hb_DynCall( { 'hblr_GetPageCount', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, @nPages ) )

   RETURN nPages

METHOD SetMargins( nPage, nLeft, nRight, nTop, nBottom ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetMargins', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT }, ;
      ::pObjHandle, nPage, nLeft, nRight, nTop, nBottom ) )

   RETURN NIL

METHOD GetTitle() CLASS TLazReport

   LOCAL cTitle := Space( 255 )

   ::CheckRes( hb_DynCall( { 'hblr_GetTitle', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR + HB_DYC_OPT_NULLTERM }, ::pObjHandle, @cTitle ) )

   RETURN cTitle

METHOD SetTitle( cTitle ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetTitle', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cTitle ) )

   RETURN NIL

METHOD GetInitialZoom() CLASS TLazReport

   LOCAL nZoom := 0

   ::CheckRes( hb_DynCall( { 'hblr_GetInitialZoom', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, @nZoom ) )

   RETURN nZoom

METHOD SetInitialZoom( nZoom ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetInitialZoom', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, nZoom ) )

   RETURN NIL

METHOD GetGrayedButtons() CLASS TLazReport

   LOCAL lGButt := .F.

   ::CheckRes( hb_DynCall( { 'hblr_GetGrayedButtons', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lGButt ) )

   RETURN lGButt

METHOD SetGrayedButtons( lGButt ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetGrayedButtons', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, lGButt ) )

   RETURN NIL

METHOD GetModifyPrepared() CLASS TLazReport

   LOCAL lMPrep := .F.

   ::CheckRes( hb_DynCall( { 'hblr_GetModifyPrepared', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lMPrep ) )

   RETURN lMPrep

METHOD SetModifyPrepared( lMPrep ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetModifyPrepared', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, lMPrep ) )

   RETURN NIL

METHOD GetReportType() CLASS TLazReport

   LOCAL nRepTyp := 0

   ::CheckRes( hb_DynCall( { 'hblr_GetReportType', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, @nRepTyp ) )

   RETURN nRepTyp

METHOD SetReportType( nRepTyp ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetReportType', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_INT }, ::pObjHandle, nRepTyp ) )

   RETURN NIL

METHOD GetShowProgress() CLASS TLazReport

   LOCAL lProg := .F.

   ::CheckRes( hb_DynCall( { 'hblr_GetShowProgress', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lProg ) )

   RETURN lProg

METHOD SetShowProgress( lProg ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetShowProgress', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, lProg ) )

   RETURN NIL

METHOD GetDoublePass() CLASS TLazReport

   LOCAL lVal := .F.

   ::CheckRes( hb_DynCall( { 'hblr_GetDoublePass', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lVal ) )

   RETURN lVal

METHOD SetDoublePass( lVal ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetDoublePass', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, lVal ) )
   RETURN NIL

METHOD GetModalPreview() CLASS TLazReport

   LOCAL lVal := .F.

   ::CheckRes( hb_DynCall( { 'hblr_GetModalPreview', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, @lVal ) )

   RETURN lVal

METHOD SetModalPreview( lVal ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetModalPreview', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_BOOL }, ::pObjHandle, lVal ) )

   RETURN NIL

METHOD GetOnClosePreview() CLASS TLazReport

   LOCAL cTitle := Space( 255 )

   ::CheckRes( hb_DynCall( { 'hblr_GetOnClosePreview', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR + HB_DYC_OPT_NULLTERM }, ::pObjHandle, @cTitle ) )

   RETURN cTitle

METHOD SetOnClosePreview( cTitle ) CLASS TLazReport

   ::CheckRes( hb_DynCall( { 'hblr_SetOnClosePreview', nHbLrLibHandle, hb_bitOr( HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL ), ;
      HB_DYN_CTYPE_VOID_PTR, HB_DYN_CTYPE_CHAR_PTR }, ::pObjHandle, cTitle ) )

   RETURN NIL

FUNCTION hblr_Eval( cExpr, p1, p2, p3, p4, p5 )

   RETURN Eval( &(cExpr), p1, p2, p3, p4, p5 )

FUNCTION hblr_Exec( cExpr )

   RETURN &cExpr

PROCEDURE hblr_SetErrorBlock()

   ErrorBlock( { | oE | Break( oE ) } )

   RETURN

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hbxvm.h"
#include "hbstack.h"
#include "hbapierr.h"

struct ExpPasFunc {
   void* Thb_dynsymFindName;
   void* Thb_dynsymSymbol;
   void* Thb_vmPushSymbol;
   void* Thb_vmPushNil;
   void* Thb_vmPushString;
   void* Thb_vmPushNumber;
   void* Thb_vmPushLogical;
   void* Thb_vmPushDate;
   void* Thb_vmPushItemRef;
   void* Thb_vmPushTimeStamp;
   void* Thb_vmFunction;
   void* Thb_vmDo;
   void* Thb_vmRequestReenter;
   void* Thb_vmRequestRestore;
   void* Thb_vmRequestQuery;
   void* Thb_parinfo;
   void* Thb_parc;
   void* Thb_parclen;
   void* Thb_parl;
   void* Thb_pardl;
   void* Thb_parnd;
   void* Thb_parni;
   void* Thb_partd;
   void* Thb_dateDecode;
   void* Thb_dateEncode;
   void* Thb_timeStampUnpackDT;
   void* Thb_timeStampPack;
   void* Thb_timeStampUnpack;
   void* Thb_xvmSeqBegin;
   void* Thb_xvmSeqEnd;
   void* Thb_xvmSeqRecover;
   void* Thb_xvmSeqEndTest;
   void* Thb_stackPop;
   void* Thb_errorBlock;
   void* Thb_itemRelease;
   void* Thb_itemClone;
};

static struct ExpPasFunc sExtPasFunc;

HB_FUNC( GETHBLRPASFUNCS )
{
   sExtPasFunc.Thb_dynsymFindName = &hb_dynsymFindName;
   sExtPasFunc.Thb_dynsymSymbol = &hb_dynsymSymbol;
   sExtPasFunc.Thb_vmPushSymbol = &hb_vmPushSymbol;
   sExtPasFunc.Thb_vmPushNil = &hb_vmPushNil;
   sExtPasFunc.Thb_vmPushString = &hb_vmPushString;
   sExtPasFunc.Thb_vmPushNumber = &hb_vmPushNumber;
   sExtPasFunc.Thb_vmPushLogical = &hb_vmPushLogical;
   sExtPasFunc.Thb_vmPushDate = &hb_vmPushDate;
   sExtPasFunc.Thb_vmPushItemRef = &hb_vmPushItemRef;
   sExtPasFunc.Thb_vmPushTimeStamp = &hb_vmPushTimeStamp;
   sExtPasFunc.Thb_vmFunction = &hb_vmFunction;
   sExtPasFunc.Thb_vmDo = &hb_vmDo;
   sExtPasFunc.Thb_vmRequestReenter = &hb_vmRequestReenter;
   sExtPasFunc.Thb_vmRequestRestore = &hb_vmRequestRestore;
   sExtPasFunc.Thb_vmRequestQuery = &hb_vmRequestQuery;
   sExtPasFunc.Thb_parinfo = &hb_parinfo;
   sExtPasFunc.Thb_parc = &hb_parc;
   sExtPasFunc.Thb_parclen = &hb_parclen;
   sExtPasFunc.Thb_parl = &hb_parl;
   sExtPasFunc.Thb_pardl = &hb_pardl;
   sExtPasFunc.Thb_parnd = &hb_parnd;
   sExtPasFunc.Thb_parni = &hb_parni;
   sExtPasFunc.Thb_partd = &hb_partd;
   sExtPasFunc.Thb_dateDecode = &hb_dateDecode;
   sExtPasFunc.Thb_dateEncode = &hb_dateEncode;
   sExtPasFunc.Thb_timeStampUnpackDT = &hb_timeStampUnpackDT;
   sExtPasFunc.Thb_timeStampPack = &hb_timeStampPack;
   sExtPasFunc.Thb_timeStampUnpack = &hb_timeStampUnpack;
   sExtPasFunc.Thb_xvmSeqBegin = &hb_xvmSeqBegin;
   sExtPasFunc.Thb_xvmSeqEnd = &hb_xvmSeqEnd;
   sExtPasFunc.Thb_xvmSeqRecover = &hb_xvmSeqRecover;
   sExtPasFunc.Thb_xvmSeqEndTest = &hb_xvmSeqEndTest;
   sExtPasFunc.Thb_stackPop = &hb_stackPop;
   sExtPasFunc.Thb_errorBlock = &hb_errorBlock;
   sExtPasFunc.Thb_itemRelease = &hb_itemRelease;
   sExtPasFunc.Thb_itemClone = &hb_itemClone;
   hb_retptr( &sExtPasFunc );
}

#pragma ENDDUMP