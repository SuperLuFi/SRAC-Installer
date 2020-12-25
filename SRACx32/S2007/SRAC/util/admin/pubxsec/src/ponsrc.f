C=======================================================================
      SUBROUTINE PONSRC( MEMBER, IPDS, IEXT, LENG, IPOS, ITOP )
C=======================================================================
C  SEARCH A MEMBER BELONG SPECIFIED DIRECTORY FROM VPDS
C                                      BY K.OKUMURA(JAERI),OCT/1999
C
C  I   MEMBER : NAME OF SEARCHING MEMBER
C  I   IPDS   : DIRECTORY NUMBER CONTATINING THE MEMBER
C  O   IEXT   : =0 MEMBER NOT EXISTING
C               >0 SPECIFIED MEMBER IS EXISTING IN VPDS
C
C  O   LENG   : =0 WHEN IEXT = 0
C               >0 DATA LENDTH
C  O   IPOS   : =0 WHEN IEXT = 0
C               >0 SEQUENTIAL POSITION IN MEMBER LIST OF VPDS
C  O   ITOP   : =0 WHEN IEXT = 0
C               >0 TOP ADDRESS OF MEMBER IN VFR( USED IN READ MODE)
C
C  NOTE : GHOST MEMBER HAS NON-ZERO LENG, IPOS, ITOP
C=======================================================================
C
      INCLUDE 'INCPDS'
      COMMON  /VPDSI1/ LMEMBR, NMEMBR, LASTAD, LOCUPY, ISTATS, NFAIL
      COMMON  /VPDSI2/ MLENG(MXMBR), MATTR(MXMBR), MADRS(MXMBR)
      COMMON  /VPDSC1/ MNAME(MXMBR), MDACS(MXPDS)
      CHARACTER*8  MEMBER, MNAME
      CHARACTER*4  MDACS
C
C=======================================================================
      IEXT = 0
      LENG = 0
      IPOS = 0
      ITOP = 0
      IF ( LMEMBR.LE.0) GOTO 9999
C
      DO 100 I = 1,LMEMBR
         IF ( MEMBER .EQ. MNAME(I) ) THEN
           IF ( MATTR(I).EQ.IPDS ) THEN
             IEXT = 1
             LENG = MLENG(I)
             IPOS = I
             ITOP = MADRS(I)
             GOTO 9999
           ENDIF
         ENDIF
  100 CONTINUE
C
cdebug-------------------
 9999 if( IEXT.eq.1) then
        if( leng.le.0) then
           write(6,*) ' debug error (PONSRC) code=1'
        endif
        if( itop.le.0) then
           write(6,*) ' debug error (PONSRC) code=2'
        endif
      endif
cdebug-------------------
c9999 RETURN
      END
