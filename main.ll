; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

$test_heap = comdat any

$mic_heap_new = comdat any

; Function Attrs: noinline nounwind optnone uwtable
define weak_odr dso_local void @test_heap(ptr noundef %p_out) #0 comdat {
entry:
  %p_out.addr = alloca ptr, align 8
  %q = alloca i32, align 4
  %heap = alloca ptr, align 8
  %p1 = alloca ptr, align 8
  %p2 = alloca ptr, align 8
  store ptr %p_out, ptr %p_out.addr, align 8
  store i32 2, ptr %q, align 4
  %call = call ptr @mic_heap_new()
  store ptr %call, ptr %heap, align 8
  %0 = load ptr, ptr %heap, align 8
  %call1 = call ptr @mic_heap_malloc(ptr noundef %0, i64 noundef 32)
  store ptr %call1, ptr %p1, align 8
  %1 = load ptr, ptr %heap, align 8
  %call2 = call ptr @mic_heap_malloc(ptr noundef %1, i64 noundef 48)
  store ptr %call2, ptr %p2, align 8
  %2 = load ptr, ptr %p1, align 8
  call void @mic_free(ptr noundef %2)
  %3 = load ptr, ptr %p2, align 8
  call void @mic_free(ptr noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local ptr @mic_heap_new() #0 comdat {
entry:
  %call = call ptr @mi_heap_new()
  ret ptr %call
}

; Function Attrs: noinline nounwind optnone uwtable
define internal ptr @mic_heap_malloc(ptr noundef %heap, i64 noundef %size) #0 {
entry:
  %heap.addr = alloca ptr, align 8
  %size.addr = alloca i64, align 8
  store ptr %heap, ptr %heap.addr, align 8
  store i64 %size, ptr %size.addr, align 8
  %0 = load ptr, ptr %heap.addr, align 8
  %1 = load i64, ptr %size.addr, align 8
  %call = call noalias ptr @mi_heap_malloc(ptr noundef %0, i64 noundef %1)
  ret ptr %call
}

; Function Attrs: noinline nounwind optnone uwtable
define internal void @mic_free(ptr noundef %ptr) #0 {
entry:
  %ptr.addr = alloca ptr, align 8
  store ptr %ptr, ptr %ptr.addr, align 8
  %0 = load ptr, ptr %ptr.addr, align 8
  call void @mi_free(ptr noundef %0)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %p = alloca i32, align 4
  %heap = alloca ptr, align 8
  store i32 0, ptr %retval, align 4
  store i32 2, ptr %p, align 4
  %call = call ptr @mic_heap_new()
  store ptr %call, ptr %heap, align 8
  %0 = load ptr, ptr %heap, align 8
  %call1 = call ptr @mic_heap_malloc(ptr noundef %0, i64 noundef 32)
  call void @test_heap(ptr noundef %call1)
  call void @mi_collect(i1 noundef zeroext true)
  call void @mi_stats_print(ptr noundef null)
  ret i32 0
}

declare void @mi_collect(i1 noundef zeroext) #1

declare void @mi_stats_print(ptr noundef) #1

declare ptr @mi_heap_new() #1

declare noalias ptr @mi_heap_malloc(ptr noundef, i64 noundef) #1

declare void @mi_free(ptr noundef) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 20.0.0git (https://github.com/llvm/llvm-project 3026ecaff54b220409ecc254b4f6209801a251b9)"}
