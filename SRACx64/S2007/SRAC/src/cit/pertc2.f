C     THIS ROUTINE IS USED ONLY IN SRAC-CITATION AND NOT USED IN COREBN
C     PERTURBATION VOLUME CALCULATION
C
      SUBROUTINE PERTC2(XX    ,XX1   ,YY    ,YY1   ,ZZ    ,ZZ1   ,
     1                  PV    ,PVOL  ,NUAC5 ,JVXP1 ,IVXP1 ,KBVXP1,
     2                  J     ,I     ,KB                          )
C
      DIMENSION XX(JVXP1),YY(IVXP1),ZZ(KBVXP1),XX1(JVXP1),YY1(IVXP1),
     &          ZZ1(KBVXP1)
C
      GO TO (108,109,110,111,112,113,114,115,116,117,118,119,120,121),
     & NUAC5
  108 PV = XX(J+1)-XX(J)
      GO TO 122
  109 PV = 3.141593*(XX(J+1)**2-XX(J)**2)
      GO TO 122
  110 PV = 4.0*3.141593/3.0*(XX(J+1)**3-XX(J)**3)
      GO TO 122
  111 PV = 1.0
      GO TO 122
  112 GO TO 122
  113 PV = (YY(I+1)-YY(I))*(XX(J+1)-XX(J))
      GO TO 122
  114 PV = 3.141593*(XX(J+1)**2-XX(J)**2)*(YY(I+1)-YY(I))
      GO TO 122
  115 PV = (XX(J+1)-XX(J))*(YY(I+1)**2-YY(I)**2)/2.0
      GO TO 122
  116 PV = PVOL
      GO TO 122
  117 CONTINUE
      PV = 0.8660254040D+0*(XX(J+1)-XX(J))*(YY(I+1)-YY(I))
C     SQRT(3)/2 = 0.8660254040
      GO TO 122
  118 PV = (ZZ(KB+1)-ZZ(KB))*(YY(I+1)-YY(I))*(XX(J+1)-XX(J))
      GO TO 122
  119 PV = 0.5*(ZZ(KB+1)-ZZ(KB))*(XX(J+1)-XX(J))*(YY(I+1)**2-YY(I)**2)
      GO TO 122
  120 PV = PVOL/(ZZ1(KB+1)-ZZ1(KB))*(ZZ(KB+1)-ZZ(KB))
      GO TO 122
  121 CONTINUE
      PV = 0.8660254040D0*(XX(J+1)-XX(J))*(YY(I+1)-YY(I))*(ZZ(KB+1)-ZZ(K
     &      B))
C     SQRT(3)/2 = 0.8660254040
  122 RETURN
      END
