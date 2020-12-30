C=======================================================================
      SUBROUTINE PONWTV( SUBNAM, IPDS, MEMBER, WORK, LENG, IPRN, IOUT )
C=======================================================================
C  ADD OR OVER-WRITE A MEMBER ONLY IN A VPDS.
C  (USE FOR WORKING MEMBERS FREQUENTLY MODIFIED AND ACCESSED)
C  WHEN VPDS IS NOT AVAILABLE, THE MEMBER WILL BE DELETED FROM VPDS AND 
C  WILL BE WRITTEN IN PDS TO AVOID INCONSISTENCY OF SAME MEMBERS IN 
C  VPDS AND PDS.
C  IN ORDER TO KEEP THE MEMBER IN PDS, THE MEMBER MUST BE FINALLY 
C  WRITTEN WITH 'WRTP' OR 'CLRV' OR 'CLRA'.
C                                      BY K.OKUMURA(JAERI),OCT/1999
C
C  I   SUBNAM : NAME OF SUBROUTINE WHICH CALLS THIS SYSTEM
C  I   IPDS   : DIRECTORY NUMBER
C  I   MEMBER : NAME OF MEMBER(A8)
C  I   WORK   : DATA ARRAY TO BE WRITTEN IN THE MEMBER
C  I   LENG   : DATA LENGTH
C  I   IPRN   : PRINT CONTROL FOR MEMBER ACCESS INFORMATION
C  I   IOUT   : OUTPUT DEVICE NUMBER FOR ERROR OR CAUTION MESSAGES
C
C--------1---------2---------3---------4---------5---------6---------7--
C
      INCLUDE 'INCPDS'
      COMMON  /USPDSC/ PDSDIR(MXPDS), MOACS(MXPDS), MOSTY(MXPDS)
      COMMON  /VPDSI1/ LMEMBR, NMEMBR, LASTAD, LOCUPY, ISTATS, NFAIL
      COMMON  /VPDSI2/ MLENG(MXMBR), MATTR(MXMBR), MADRS(MXMBR)
      COMMON  /VPDSC1/ MNAME(MXMBR), MDACS(MXPDS)
      COMMON  /VPDSR1/ VRF(MXVRF)
C
      CHARACTER*4    MOACS, MOSTY, MDACS
      CHARACTER*8    SUBNAM
      CHARACTER*8    MEMBER, MNAME
      CHARACTER*120  PDSDIR, DIRNAM
      DIMENSION WORK(*)
C
C=======================================================================
C
      DIRNAM = PDSDIR(IPDS)
      LENGKP = LENG
      IF(MDACS(IPDS).NE.'CORE') GOTO 1000
C
C---- 1) SEARCH SAME MEMBER IN VPDS
C       *IF THE MEMBER FOUND, REPLACE ONLY DATA AND LENGTH
      CALL PONSRC( MEMBER, IPDS, IEXT, LENG, IPOS, ITOP )
      IF ( IEXT.EQ.1 ) THEN
        IF( LENG.GE.LENGKP ) THEN
          IT = ITOP-1
          DO 100 I=1,LENGKP
            VRF(IT+I) = WORK(I)
  100     CONTINUE
          LOCUPY = LOCUPY - MLENG(IPOS) + LENGKP
          MLENG(IPOS) = LENGKP
          IF(IPRN.GE.2) THEN
            WRITE(IOUT,6000) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &                       LENGKP,' WAS STORED IN CORE ',DIRNAM
          ENDIF
          GOTO 9999
        ELSE
C       *IF THE MEMBER FOUND, BUT LENGTH IS NOT ENOUGH, DELETE THE
C        MEMBER FROM VPDS
          IPRN2 = 0
          IDEL = 1
          CALL PONDEL( SUBNAM, IPDS, MEMBER, IDEL, IPRN2, IOUT )
        ENDIF
      ENDIF
C
C---- 2) IF THE MEMBER NOT FOUND IN VPDS, OR LEMGTH IS NOT ENOUGH
C        SEARCH A FIRST GHOST MEMBER WITH ENOUGH LENGTH
C       *IF A GHOST MEMBER WITH ENOUGH SPACE IS FOUND, WRITE THE MEMBER
      CALL PONGST( LENGKP, LENGO, IPOS, ITOP )
      IF( LENGO.GT.0 ) THEN
        IT = ITOP-1
        DO 200 I=1,LENGKP
          VRF(IT+I) = WORK(I)
  200   CONTINUE
        MNAME(IPOS) = MEMBER
        MATTR(IPOS) = IPDS
        MLENG(IPOS) = LENGKP
        MADRS(IPOS) = ITOP
        NMEMBR = NMEMBR + 1
        LOCUPY = LOCUPY + LENGKP
        IF(IPRN.GE.2) THEN
          WRITE(IOUT,6000) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &                     LENGKP,' WAS STORED IN CORE ',DIRNAM
        ENDIF
        GOTO 9999
      ENDIF
C
C---- 3) JUMP TO PDS MODE, IF MEMBER LIST & LAST SPACE IS NOT ENOUGH
C       *IF MEMBER IS FULL -> WRITE IN PDS (NO VPDS SEARCH)
      IF(LMEMBR.GE.MXMBR) THEN
        NFAIL = NFAIL + 1
        GOTO 2000
      ENDIF
C
C       *IF LAST SPACE IS NOT ENOUGH -> WRITE IN PDS (NO VPDS SEARCH)
      LASTSP = MXVRF - LASTAD
      IF(LENGKP.GT.LASTSP) THEN
        NFAIL = NFAIL + 1
        GOTO 2000
      ENDIF
C
C---- 4) ADD THE MEMBER IN VPDS, IF MEMBER LIST & LAST SPACE IS ENOUGH
      DO 300 I=1,LENGKP
        VRF(LASTAD+I) = WORK(I)
  300 CONTINUE
      LMEMBR = LMEMBR + 1
      NMEMBR = NMEMBR + 1
      MNAME(LMEMBR) = MEMBER
      MATTR(LMEMBR) = IPDS
      MLENG(LMEMBR) = LENGKP
      MADRS(LMEMBR) = LASTAD + 1
      LOCUPY = LOCUPY + LENGKP
      LASTAD = LASTAD + LENGKP
      IF(IPRN.GE.2) THEN
        WRITE(IOUT,6000) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &                   LENGKP,' WAS STORED IN CORE ',DIRNAM
      ENDIF
      GOTO 9999
C
C===== OVER-WRITE IN PDS MODE ============
C
 1000 CONTINUE
C---- 1) SEARCH MEMBER IN VPDS
C     2) IF EXISTING IN VPDS, DELETE THE MEMBER IN VPDS
C     3) OVER-WRITE THE MEMBER IN PDS
      CALL PONSRC( MEMBER, IPDS, IEXT, LENG, IPOS, ITOP )
      IF ( IEXT.EQ.1 ) THEN
        IPRN2 = 0
        IDEL = 1
        CALL PONDEL( SUBNAM, IPDS, MEMBER, IDEL, IPRN2, IOUT )
      ENDIF
 2000 LENG = LENGKP
      IMD  = 1
      CALL PDSOT(DIRNAM,MEMBER,WORK,LENG,IMD,IOUT,IPRN,IRC)
      CALL PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
C
C=======================================================================
C
 6000 FORMAT(A,A,A,I7,A,/,5X,A)
 9999 LENG = LENGKP
      RETURN
      END