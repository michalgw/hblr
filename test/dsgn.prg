#include "hblr.ch"

FUNCTION Main( cParam1, cParam2, cParam3, cParam4 )

   LOCAL oLR

   oLR := TLazReport():New()

   IF ! Empty( cParam1 )
      oLR:AddDataset( cParam1 )
   ENDIF
   IF ! Empty( cParam2 )
      oLR:AddDataset( cParam2 )
   ENDIF
   IF ! Empty( cParam3 )
      oLR:AddDataset( cParam3 )
   ENDIF
   IF ! Empty( cParam4 )
      oLR:AddDataset( cParam4 )
   ENDIF

   oLR:DesignReport()

   oLR := NIL

   RETURN