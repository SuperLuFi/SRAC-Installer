C-----------------------------------------------------------------------
C     DIMENSION WORK(6),IWORK(1),CWORK(1)
C     EQUIVALENCE (WORK(1),IWORK(1),CWORK(1))
C     CHARACTER LINE*72, CWORK*4
C     LINE = ' 1.00000E+02 CASE               125 -3.00000E-12'
C     LD = 4
C     KPOS = 100
C     CALL SETDA1(LINE,LD,WORK,KPOS)
C     WRITE(6,*) (WORK(I),I=1,LD)
C     WRITE(6,*) (IWORK(I),I=1,LD)
C     WRITE(6,*) (CWORK(I),I=1,LD)
C     STOP
C     END
C-----------------------------------------------------------------------
C     SETDA1 : SET DATA FROM TEXT LINE(A72) INCLUDING INTEGER OR
C              FLOATING NUMBER OR CHRACTER DATA IN MEMBER ----DN-T
C              SAMPLE:
C               1.00000E+02 CASE               125 -3.00000E-12 .......
C-----------------------------------------------------------------------
      SUBROUTINE SETDA1(LINE,LD,DATA,KPOS)
C     KPOS : SEQUENTIAL POSITION OF PDS-DATA FOR THE TOP DATA IN A LINE
C
      CHARACTER LINE*72,ADATA*12,CWORK*4
      COMMON /SETDT/ NTNUC1,NTNUC2,NZON2,NZON3
      DIMENSION DATA(6),WORK(6),IWORK(1),CWORK(1),ADATA(6)
      EQUIVALENCE (WORK(1),IWORK(1),CWORK(1))
C
      DO 100 I=1,LD
        LPOS = KPOS+I-1
        IS = 12*(I-1)+1
        IE = IS + 12 -1
        ADATA(I) = LINE(IS:IE)
C----- CASE(A4),STDNUC(A4)
        IIS = 11
        IIE = 12
        IF( (LPOS.GE.IIS).AND.(LPOS.LE.IIE) ) THEN
          CWORK(I) = ADATA(I)(2:5)
          GOTO 100
        ENDIF
C----- NUCLID(NTNUC*A4)
        IF(NTNUC1.EQ.0) GOTO 110
        IIS = 15
        IIE = IIS+NTNUC1-1
        IF( (LPOS.GE.IIS).AND.(LPOS.LE.IIE) ) THEN
          CWORK(I) = ADATA(I)(2:5)
          GOTO 100
        ENDIF
C----------------------
  110   CALL TXTCHK(ADATA(I),ICODE)
        IF(ICODE.EQ.0) THEN
          READ(ADATA(I),'(1PE12.5)') WORK(I)
C         WRITE(6,*) I,WORK(I)
        ELSE
          CALL CNVINT(ADATA(I),IWORK(I))
          IF(LPOS.EQ.2) NTNUC1=IWORK(I)
        ENDIF
  100 CONTINUE
      DO 200 I=1,LD
        DATA(I) = WORK(I)
  200 CONTINUE
      RETURN
      END
