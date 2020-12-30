C=======================================================================
      SUBROUTINE PONCMP( SUBNAM, IPRN, IOUT )
C=======================================================================
C  COMPRESS MEMORY FOR VPDS( DELETE GHOST MEMBERS)
C  DELETE GHOST MEMBERS IN VPDS, AND RECONSTRUCT MEMBER LIST
C                                      BY K.OKUMURA(JAERI),OCT/1999
C
C  I   SUBNAM : NAME OF SUBROUTINE WHICH CALLS THIS SYSTEM
C  I   IPRN   : PRINT CONTROL FOR MEMBER ACCESS INFORMATION
C  I   IOUT   : OUTPUT DEVICE NUMBER FOR ERROR OR CAUTION MESSAGES
C 
C--------1---------2---------3---------4---------5---------6---------7--
C
      INCLUDE 'INCPDS'
      COMMON  /VPDSI1/ LMEMBR, NMEMBR, LASTAD, LOCUPY, ISTATS, NFAIL
      COMMON  /VPDSI2/ MLENG(MXMBR), MATTR(MXMBR), MADRS(MXMBR)
      COMMON  /VPDSC1/ MNAME(MXMBR), MDACS(MXPDS)
      COMMON  /VPDSR1/ VRF(MXVRF)
C
      CHARACTER*4    MDACS
      CHARACTER*8    SUBNAM
      CHARACTER*8    MNAME
C
C=======================================================================
C
      IF ( LMEMBR.LE.0) GOTO 9999
C
      IF ( IPRN.GT.0 ) THEN
        WRITE(IOUT,6000)
        LASTSP = MXVRF - LASTAD
        VRAT   = FLOAT(LASTSP)/FLOAT(MXVRF)*100.0
        WRITE(IOUT,6100) VRAT
      ENDIF
      IF ( IPRN.GE.3 ) THEN
        WRITE(IOUT,6200) LMEMBR, NMEMBR, LASTAD, LOCUPY
      ENDIF
C
      LAST   = 0
      NMEM   = 0
      NCMP   = 0
      LCVD   = 0
      NGST   = 0
      LGST   = 0
      LOCUPY = 0
C
      DO 100 I=1, LMEMBR
        IF ( MNAME(I).NE.'        ') THEN
          NMEM = NMEM + 1
C
C----     CHECK VOID SPACE IS INCLUDED OR NOT
          IF(I+1.LE.LMEMBR) THEN
            INTVL = MADRS(I+1) - MADRS(I)
          ELSE
            INTVL = LASTAD+1 - MADRS(I)
          ENDIF
          LVID = INTVL - MLENG(I)
C
C----     COMPRESS IF VOID SPACE IS INCLUDED IN CURRENT MEMBER, OR 
C         ALREADY COMPRESSED, OR THERE ARE GHOSTS IN PREVIOUS POSITION
C
          IEXE = 0
          IF(LVID.GT.0) IEXE = 1
          IF(NCMP.GT.0) IEXE = 1
          IF(NGST.GT.0) IEXE = 1
C
          IF(IEXE.EQ.1) THEN
C----       COMPRESS MEMBER LIST ----
            IF(I.NE.NMEM) THEN
              MNAME(NMEM) = MNAME(I)
              MNAME(I)    = '        '
              MLENG(NMEM) = MLENG(I)
              MATTR(NMEM) = MATTR(I)
            ENDIF
C
C----       MOVE DATA TO NEW ADRRESS
C 
            JTMP = MADRS(I) - 1
            IF(LAST.NE.JTMP) THEN
              IF(LAST+MLENG(NMEM).GE.JTMP+1 ) THEN
C----         RECURSIVE OPERATION (SCALAR)
*VOCL LOOP,SCALAR
*VOPTION NOVEC
*vdir novector
CDIR$ NEXTSCALAR
                DO 200 J = 1, MLENG(NMEM)
                  VRF(LAST + J) = VRF( JTMP + J)
  200           CONTINUE
              ELSE
C----         NOT RECURSIVE OPERATION (VECTORIZED/PARALLELIZED)
*VOCL LOOP,NOVREC
*VOPTION NODEP
*vdir nodep
CDIR$ IVDEP
                DO 300 J = 1, MLENG(NMEM)
                  VRF(LAST + J) = VRF( JTMP + J)
  300           CONTINUE
              ENDIF
            ENDIF
C
            MADRS(NMEM) = LAST + 1
            LAST = LAST + MLENG(NMEM)
            NCMP = NCMP + 1
            LCVD = LCVD + LVID
          ELSE
C----       NMEM=I : NO GHOST MEMBERS AND VOID SPACE UNTIL NOW
            LAST = LAST + MLENG(I)
          ENDIF
          LOCUPY = LOCUPY + MLENG(NMEM)
        ELSE
C----     NUMBERS OF GHOSTS OR REPLACED GHOST
          NGST = NGST + 1
        ENDIF
  100 CONTINUE
C
      LMEMBR = NMEM
      NMEMBR = NMEM
      LASTAD = LAST
C
      IF ( IPRN.GT.0 ) THEN
        WRITE(IOUT,6300)
        LASTSP = MXVRF - LASTAD 
        VRAT   = FLOAT(LASTSP)/FLOAT(MXVRF)*100.0
        WRITE(IOUT,6400) VRAT
      ENDIF
      IF ( IPRN.GE.3 ) THEN
        WRITE(IOUT,6200) LMEMBR, NMEMBR, LASTAD, LOCUPY
      ENDIF
C
C--------1---------2---------3---------4---------5---------6---------7--
 6000 FORMAT(5X,'COMPRESSING OF MEMORY FOR VIRTUAL PDS START')
 6100 FORMAT(5X,'BEFORE COMPRESSING : RATIO OF AVAILABLE LAST ',
     &'SPACE(%)=',F12.3)
 6200 FORMAT(
     &5X,'-- NUMBER OF MEMBERS IN V-PDS INCLUDING DELETED ONES =',I12,/,
     &5X,'-- NUMBER OF REGISTERED MEMBERS IN V-PDS             =',I12,/,
     &5X,'-- ADDRESS OF LAST DATA (WORD)                       =',I12,/,
     &5X,'-- EFFECTIVELY USED DATA LENGTH (WORD)               =',I12)
 6300 FORMAT(/,5X,'COMPRESSING OF MEMORY FOR VIRTUAL PDS COMPLETED')
 6400 FORMAT(5X,'AFTER COMPRESSING  : RATIO OF AVAILABLE LAST ',
     &'SPACE(%)=',F12.3)
C
 9999 RETURN
      END
