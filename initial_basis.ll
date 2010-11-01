; ModuleID = 'initial_basis.cc'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i386-pc-linux-gnu"

define i32 @int_add(i32 %x, i32 %y) nounwind readnone {
  %1 = add nsw i32 %y, %x                         ; <i32> [#uses=1]
  ret i32 %1
}

define i32 @int_minus(i32 %x, i32 %y) nounwind readnone {
  %1 = sub i32 %x, %y                             ; <i32> [#uses=1]
  ret i32 %1
}

define i32 @int_div(i32 %x, i32 %y) nounwind readnone {
  %1 = sdiv i32 %x, %y                            ; <i32> [#uses=1]
  ret i32 %1
}

define i32 @int_mul(i32 %x, i32 %y) nounwind readnone {
  %1 = mul i32 %y, %x                             ; <i32> [#uses=1]
  ret i32 %1
}

define double @double_int_add(double %x, i32 %y) nounwind readnone {
  %1 = sitofp i32 %y to double                    ; <double> [#uses=1]
  %2 = fadd double %1, %x                         ; <double> [#uses=1]
  ret double %2
}
