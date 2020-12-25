      SUBROUTINE PDSZRO
C
C *** CLEAR COMMON AREA OF IN-CORE PDS DATA MANAGEMENT
C
CMOD  PARAMETER   ( MAXMEM = 20000  , MXWORK =  3000000  )
      INCLUDE  'READPINC'
C
      CHARACTER*8     CZMEMB,SCMEMB,FLMODE,DATAKP
      CHARACTER*12    DDNAME,FILENM
      CHARACTER*68    PATHNM
C
      COMMON /PDSPDS/ DDNAME(125),IST(15),IRW(15),IOS(35),NC(5,20),
     &                IFLSW,FILENM,ECODE,TEMP
C
      COMMON /PDSWK3/ PATHNM(15),FLMODE(15),DATAKP(15),CZMEMB(MAXMEM),
     1                SCMEMB(MAXMEM)
      COMMON /PDSWK2/ IZWRIT,IZMXDT,IZWCNT,IZDWTL,ICNTMX,
     1                LENPAT(15),INCORE(15),ICNTSC,
     2                IZDTLN(MAXMEM),IZDTTL(MAXMEM),IDUMZ,
     3                IPOSDD(MAXMEM),IPOSSC(MAXMEM),
     4                RZDATA(MXWORK)
C
      IFLSW     = 0
      CALL    ICLEA ( IST , 15 , 0 )
      CALL    ICLEA ( IRW , 15 , 0 )
      CALL    ICLEA ( IOS , 35 , 0 )
      CALL    ICLEA ( NC  ,100 , 0 )
      CALL    ICLEA ( LENPAT , 15 , 0 )
      CALL    ICLEA ( INCORE , 15 , 0 )
C
      IZWRIT    = MAXMEM
      IZMXDT    = MXWORK
      IZWCNT    = 0
      IZDWTL    = 0
      ICNTMX    = 0
      IDUMZ     = 0
      ICNTSC    = 0
C
      DO  100 I = 1 , MAXMEM
CKSK  CZMEMB(I) = 8H
      CZMEMB(I) = '        '
CKSK  SCMEMB(I) = 8H
      SCMEMB(I) = '        '
  100 CONTINUE
C
      DO  110 I = 1 , 15
CKSK  DDNAME(I) = 12H
      DDNAME(I) = '            '
CKSK  FLMODE(I) =  8H
      FLMODE(I) = '        '
CKSK  DATAKP(I) =  8H
      DATAKP(I) = '        '
CKSK  PATHNM(I) = 68H
CKSK +
      PATHNM(I) = '                                  '//
     +            '                                  '
  110 CONTINUE
C
      CALL ICLEA( IZDTLN , MAXMEM , 0   )
      CALL ICLEA( IZDTTL , MAXMEM , 0   )
      CALL ICLEA( IPOSDD , MAXMEM , 0   )
      CALL ICLEA( IPOSSC , MAXMEM , 0   )
      CALL  CLEA( RZDATA , MXWORK , 0.0 )
C
C
      RETURN
      END
