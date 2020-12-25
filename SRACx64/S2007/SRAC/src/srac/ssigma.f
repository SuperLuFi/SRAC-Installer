      SUBROUTINE SSIGMA(NG,MPOI,L235,L238,SIGMA,PHMVM,VM,PHXVX,VX,JERR)
C
C     READ MICRO CROSS-SECTION DATA FILE
C
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
CMOD  PARAMETER   ( MAXMAT =  30 )
      INCLUDE  'BMICRINC'
      INCLUDE  'MATDTINC'
C
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
      REAL*4      FLUXM(MAXNG,MAXMAT),VOLM(MAXMAT)
      REAL*4      FLUXX(MAXNG,MAXMAT),VOLX(MAXMAT)
      INTEGER*4   NISO(MAXMAT),MATXRG(MAXMAT)
C
      DIMENSION SIGMA(NG,4),PHMVM(NG),PHXVX(NG)
C
C *** READ X-SECTION TO FT52F001
C
      REWIND 52
      READ(52)  NMAT,NXR,NRTOT
C
      IF(MPOI.LT.1.OR.MPOI.GT.NMAT) THEN
                                    REWIND 52
                                    JERR = JERR+10
                                    GO TO 900
                                    ENDIF
C
      READ(52) (NISO(I),I=1,NMAT),((FLUXM(J,I),J=1,NRTOT),I=1,NMAT),
     @         (MATXRG(I),I=1,NMAT),(VOLM(I),I=1,NMAT),
     @         (VOLX(I),I=1,NXR ),((FLUXX(J,I),J=1,NRTOT),I=1,NXR )
      READ(52)
CDEL  ICNT    = 0
      DO 60 M = 1 , NMAT
      MMK     = NISO(M)
      MPOS    = M
      IF(NISO(M).GT.0) THEN
CDEL                   ICNT = ICNT + 1
      READ(52,END=65) (((EFFMIC(I,J,K),I=1,NRTOT),J=1,MAXMT3),K=1,MMK)
                       ENDIF
CMOD  IF(ICNT.EQ.MPOI) GO TO 70
      IF(M   .EQ.MPOI) GO TO 70
   60 CONTINUE
C
   65 REWIND 52
      JERR = JERR + 20
      RETURN
C
   70 REWIND 52
      VM        = VOLM(MPOS)
      IXPOS     = MATXRG(MPOS)
      DO 100  I = 1 , NG
      SIGMA(I,2)= EFFMIC(I,1,L235)
      SIGMA(I,1)= EFFMIC(I,2,L235)
      SIGMA(I,4)= EFFMIC(I,1,L238)
      SIGMA(I,3)= EFFMIC(I,2,L238)
      PHMVM(I)  = FLUXM(I,MPOS)
  100 CONTINUE
C
      IF(IXPOS.GT.0.AND.IXPOS.LE.NXR) THEN
              VX = VOLX(IXPOS)
              DO 110 I = 1 , NG
              PHXVX(I) = FLUXX(I,IXPOS)
  110         CONTINUE
              ELSE
              VX = VM
              DO 120 I = 1 , NG
              PHXVX(I) = PHMVM(I)
  120         CONTINUE
              ENDIF
C
  900 CONTINUE
CM    WRITE(6,*) ' ** NXR IXPOS VX VM : ',NXR,IXPOS,VX,VM
      RETURN
      END
