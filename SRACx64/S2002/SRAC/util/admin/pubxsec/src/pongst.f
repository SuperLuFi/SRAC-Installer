C=======================================================================
      SUBROUTINE PONGST( LENGI, LENGO, IPOS, ITOP )
C=======================================================================
C  SEARCH A GHOST MEMBER WITH SUFFICIENTLY LARGE LENGTH (LENGO>=LENGI)
C                                      BY K.OKUMURA(JAERI),OCT/1999
C
C  I   LENGI  : >0 REQUIRED DATA LENGTH
C
C  O   LENGO  : =0 REQUIRED GHOST MEMBER NOT FOUND
C               >0 REGUIRED GHOST MEMBER WITH LENGTH = LENGO FOUND
C  O   IPOS   : =0 WHEN LENGO = 0
C               >0 SEQUENTIAL POSITION OF THE GHOST IN MEMBER LIST
C  O   ITOP   : =0 WHEN LENGO = 0
C               >0 TOP ADDRESS OF THE GHOST MEMBER IN VFR
C
C  NOTE : GHOST MEMBER HAS NON-ZERO LENG, IPOS, ITOP
C=======================================================================
C
      INCLUDE 'INCPDS'
      COMMON  /VPDSI1/ LMEMBR, NMEMBR, LASTAD, LOCUPY, ISTATS, NFAIL
      COMMON  /VPDSI2/ MLENG(MXMBR), MATTR(MXMBR), MADRS(MXMBR)
      COMMON  /VPDSC1/ MNAME(MXMBR), MDACS(MXPDS)
      CHARACTER*8  MNAME, BLANK
      CHARACTER*4  MDACS
C
C=======================================================================
      LENGO = 0
      IPOS  = 0
      ITOP  = 0
      BLANK = '        '
      IF ( LMEMBR.LE.0 ) GOTO 9999
      IF ( LENGI.LE.0 )  GOTO 9999
C
      DO 100 I = 1,LMEMBR
        IF ( MNAME(I).EQ.BLANK ) THEN
          IF( MLENG(I).GE.LENGI ) THEN
            LENGO = MLENG(I)
            IPOS = I
            ITOP = MADRS(I)
            GOTO 9999
          ENDIF
        ENDIF
  100 CONTINUE
C
 9999 RETURN
      END
