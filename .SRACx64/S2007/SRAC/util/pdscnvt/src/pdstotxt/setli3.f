C-----------------------------------------------------------------------
C     SETLI3 : SET DATA INCLUDING INTEGER OR FLOATING NUMBER OR
C              CHARACTER INTO A TEXT LINE(A72)
C              FOR THE MEMBER ----REST IN MACRO/MACROWRK
C              SAMPLE:
C               1.00000E+02-2.00000E-02 SRAC       -3.00000E-12 .......
C-----------------------------------------------------------------------
      SUBROUTINE SETLI3(LINE,LD,DATA,KPOS)
C     KPOS : SEQUENTIAL POSITION OF PDS-DATA FOR THE TOP DATA IN A LINE
C
      CHARACTER LINE*72,CWORK*4
      DIMENSION DATA(6),WORK(6),IWORK(1),CWORK(1)
      EQUIVALENCE (WORK(1),IWORK(1),CWORK(1))
      COMMON /SETDT/ NTNUC1,NTNUC2,NZON2,NZON3
C
      LINE = ' '
      DO 100 I=1,LD
        WORK(I) = DATA(I)
  100 CONTINUE
C
      DO 200 I=1,LD
        LPOS = KPOS+I-1
        IS = 12*(I-1)+1
        IE = IS + 12 -1
C----- MTNAME(NZON*2A4)
        IF(NZON3.EQ.0) GOTO 210
        IIS = 2+NZON3+1
        IIE = IIS+2*NZON3-1
        IF( (LPOS.GE.IIS).AND.(LPOS.LE.IIE) ) THEN
          LINE(IS:IE) = ' '//CWORK(I)//'       '
          GOTO 200
        ENDIF
C----------------------
  210   CALL NUMCHK(DATA(I),ICODE)
        IF(ICODE.EQ.0) THEN
          WRITE(LINE(IS:IE),'(1PE12.5)') WORK(I)
        ELSE
          WRITE(LINE(IS:IE),'(4X,I7,1X)') IWORK(I)
          IF(LPOS.EQ.2) NZON3 =IWORK(I)
        ENDIF
  200 CONTINUE
      RETURN
      END
