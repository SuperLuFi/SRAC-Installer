C***********************************************************************
C
C  UIOUNT   : SET UNFORMATED(0) OR FORMATED(1)
C             FOR EACH I/O DEVICE
C             IOFORM < 0 : NOT OPENED AT INITIALIZATION STEP, 
C             EVEN IF "SETENV FU??" IS SET.
C
C  THIS ROUTINE IS USED FOR SRAC95-EWS
C
C***********************************************************************
C
      SUBROUTINE UIOUNT(IOFORM)
      DIMENSION IOFORM(100)
      DO 100 I=1,100
        IOFORM(I) = -1
  100 CONTINUE
      IOFORM(5)  = 1
      IOFORM(6)  = 1
      IOFORM(10) = 1
      IOFORM(99) = 1
C
      IOFORM(49) = -1
      RETURN
      END
