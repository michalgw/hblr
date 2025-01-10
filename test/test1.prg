#include "hblr.ch"
#include "fileio.ch"

FUNCTION Main()

   LOCAL oLR1, oLR2, oLR3, oLR4, oLRC, hRow := hb_hash(), i, nt, ny, nu, f, cData

   AltD()

   ? "Loading library"
   IF ! hblr_LoadLibrary()
      ? "Can not load library."
      RETURN
   ENDIF

   ? "Loading report #1"
   USE TEST
   INDEX ON last + first TO testn
   oLR1 := TLazReport():New()
   DBGoTop()
   DO WHILE !Eof()
      hRow[ 'first' ] := AllTrim( test->first )
      hRow[ 'last' ] := AllTrim( test->last )
      hRow[ 'street' ] := AllTrim( test->street )
      hRow[ 'city' ] := AllTrim( test->city )
      hRow[ 'state' ] := AllTrim( test->state )
      hRow[ 'zip' ] := AllTrim( test->zip )
      hRow[ 'hiredate' ] := test->hiredate
      hRow[ 'married' ] := test->married
      hRow[ 'age' ] := test->age
      hRow[ 'salary' ] := test->salary
      hRow[ 'notes' ] := AllTrim( test->notes )
      oLR1:AddRow( 'test', hRow )
      DBSkip()
   ENDDO
   oLR1:AddDataset( 'test' )
   oLR1:LoadFromFile( 'test1.lrf' )
   oLR1:Title := 'Report #1'
   oLR1:ModalPreview := .F.
   oLR1:ShowReport()

   ? "Loading report #2"
   oLR2 := TLazReport():New()
   FOR i := 0 TO 7
      oLR2:AddValue( 'test:' + AllTrim( Str( i ) ) + ':col', 'Value #' + AllTrim( Str( i ) ) )
   NEXT
   f := FOpen( 'test2.lrf' )
   i := FSeek( f, 0, FS_END )
   FSeek( f, 0 )
   cData := Space( i )
   FRead( f, @cData, i )
   FClose( f )
   oLR2:LoadFromXMLMemory( cData )
   oLR2:Title := 'Report #2'
   oLR2:ModalPreview := .F.
   oLR2:ShowReport()

   ? "Loading report #3"
   oLR3 := TLazReport():New()
   FOR nt := 0 TO 10
      oLR3:AddValue( 'tab1:' + Str( nt ) + ':col1', 'ABC' + Str( nt ) )
      oLR3:AddValue( 'tab1:' + Str( nt ) + ':col2', nt )
      oLR3:AddValue( 'tab1:' + Str( nt ) + ':col3', 0d20121201 )
      FOR ny := 0 TO 5
         oLR3:AddValue( 'tab1:' + Str( nt ) + ':tab2:' + Str( ny ) + ':col1', 'CDE' + Str( ny ) )
         oLR3:AddValue( 'tab1:' + Str( nt ) + ':tab2:' + Str( ny ) + ':col2', 10 * nt + ny )
         FOR nu := 0 TO 5
            oLR3:AddValue( 'tab1:' + Str( nt ) + ':tab2:' + Str( ny ) + ':tab3:' + Str( nu ) + ':col1', 'CDE' + Str( nu ) )
            oLR3:AddValue( 'tab1:' + Str( nt ) + ':tab2:' + Str( ny ) + ':tab3:' + Str( nu ) + ':col2', 100 * nt + 10 * ny + nu )
         NEXT
      NEXT
   NEXT
   oLR3:AddDataset( 'tab1' )
   oLR3:AddDataset( 'tab1:tab2' )
   oLR3:AddDataset( 'tab1:tab2:tab3' )
   oLR3:LoadFromFile( 'test3.lrf' )
   oLR3:Title := 'Report #3'
   oLR3:ModalPreview := .F.
   oLR3:ShowReport()

   oLR4 := TLazReport():New()
   oLR4:AddHbDataset( 'data', 'Eof()', 'DbGoTop()', 'DbSkip()' )
   oLR4:LoadFromFile( 'test4.lrf' )
   oLR4:Title := 'Report #4'
   oLR4:ModalPreview := .F.
   oLR4:ShowReport()
   
   ? "Loading composite report"
   oLRC := TLazReport():New( .T. )
   oLRC:AddReport( oLR1 )
   oLRC:AddReport( oLR2 )
   oLRC:AddReport( oLR3 )
   oLRC:AddReport( oLR4 )
   oLRC:DoublePass := .T.
   oLRC:Title := 'Test reports 1,2,3 and 4'
   oLRC:ModalPreview := .F.
   oLRC:ShowReport()

   ? "Press ESC to exit"
   i := 0
   DO WHILE i <> 27
      i := Inkey( 0 )
      ? "Key pressed: " + Str( i )
   ENDDO

   oLR1 := NIL
   oLR2 := NIL
   oLR3 := NIL
   oLR4 := NIL
   oLRC := NIL