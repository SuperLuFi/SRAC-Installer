C             PACK                LEVEL=2        DATE=84.12.19
C     SUBROUTINE PACK(L,M,N)
C
C  ALTERNATE ROUTINE FOR 'PACK' & 'UNPACK' (FACOM SERVICE ROUTINE)
C        ( BY M.SASAKI  84/19/12 )
C
C     CHARACTER*4 L,N,W
C     W = N
C     L(M:M) = W(4:4)
C     RETURN
C
C     ENTRY UNPACK(L,M,N)
C     W = L
C     N(4:4) = W(M:M)
C     RETURN
C     END
C
C               PACK  UNPACK  PACKX   BY FORT77
C
                SUBROUTINE PACK(A,I,B)
C
                  CHARACTER *4 A,B
C
                  A(I:I)=B(4:4)
                  RETURN
C
                  ENTRY UNPACK(A,I,B)
                  B(4:4)=A(I:I)
                  RETURN
C
                  ENTRY PACKX(A,I,B,J)
                  A(I:I)=B(J:J)
                  RETURN
C
                  END
