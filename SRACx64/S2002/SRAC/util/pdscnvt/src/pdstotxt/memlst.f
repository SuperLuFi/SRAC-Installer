C-----------------------------------------------------------------------
C     PARAMETER (MAXME=3000)
C     DIMENSION MEMNAM(MAXME)
C     CHARACTER MEMNAM*8
C     IOMLS = 11
C     CALL MEMLST(IOMLS,NMEM,MEMNAM)
C     WRITE(6,*) 'NMEM= ',NMEM
C     DO 10 I=1,NMEM
C       WRITE(6,*) MEMNAM(I)
C  10 CONTINUE
C     STOP
C     END
C-----------------------------------------------------------------------
C     MEMLST : READ LIST OF MEMBER NAME FROM SPECIAL MEMBER(LIST)
C     FORMAT IS DEPEND ON UNIX SYSTEM BUT DIVIDED BY SOME BLANKS.
C     CU050000   CU08000   CPU00000   CPU10000  CPU20000
C     CPU90000   CAM1000
C-----------------------------------------------------------------------
      SUBROUTINE MEMLST(IOMLS,NMEM,MEMNAM)
C
      PARAMETER (MAXME=3000)
      DIMENSION MEMNAM(MAXME)
      CHARACTER MEMNAM*8, LINE*255
C
      LLINE = 255
      IPOS  = 0
      NMEM  = 0
 1000 ISRC  = 0
      READ(IOMLS,'(A255)',END=9999) LINE
      DO 100 I=1,LLINE
C-- FIND NEW MEMBER NOW
        IF(ISRC.EQ.0.AND.LINE(I:I).NE.' ') THEN
          LMEM = 1
          ISRC = 1
C-- ALREADY FOUND
        ELSEIF(ISRC.NE.0.AND.LINE(I:I).NE.' ') THEN
          LMEM = LMEM + 1
        ENDIF
C-- END OF MEMBER
        IF(ISRC.NE.0.AND.LINE(I:I).EQ.' ') THEN
          NMEM = NMEM + 1
          IS = I-LMEM
          IE = I-1
          MEMNAM(NMEM) = LINE(IS:IE)
          ISRC = 0
        ENDIF
  100 CONTINUE
      GOTO 1000
C
 9999 RETURN
      END
