program test_openmp_target
   
    implicit none

    integer :: a(32)

    a = 1
    !$omp target map(tofrom:a)
    a = a + 2
    !$omp end target

    if (.not. all(a == 3)) error stop

end program test_openmp_target
