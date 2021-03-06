C             WRFQ                LEVEL=1        DATE=81.11.14
      SUBROUTINE WRFQ (XN,IGM,IM)
C     C
      COMMON /MAINC/ DUMMY(64),NOUT2
      DIMENSION XN(IM,IGM)
      DIMENSION SOUR(2)
C     C
      REWIND 33
      WRITE(6,100)
      WRITE(NOUT2,100)
      CALL REAG(XN(1,1),IGM*IM,'FLUX',' GES')
      DO 1000 I=1,IGM
      WRITE (33) (XN(J,I),J=1,IM)
 1000 CONTINUE
      CALL REAM(SOUR,D,D,2,0,0)
      WRITE (6,110) SOUR
      WRITE (NOUT2,110) SOUR
      CALL GETLEN (SOUR,LENGTH)
      IF(LENGTH.NE.IGM*IM) STOP ' SOURCE READ ERROR '
      CALL READ(SOUR,XN,LENGTH)
      DO 1100 I=1,IGM
      WRITE (33) (XN(J,I),J=1,IM)
 1100 CONTINUE
      REWIND 33
      WRITE(6,120)
      WRITE(NOUT2,120)
      RETURN
  100 FORMAT('0 +++ FLUX GUESS AND SOURCE (IEVT=0) FILE PREPARE '
     &      ,'START +++'   )
  110 FORMAT('0    READ SOURCE IDENT ',2A4  )
  120 FORMAT('0 +++ FLUX GUESS AND SOURCE (IEVT=0) FILE IS '
     &      ,'PREPARED  ')
      END
