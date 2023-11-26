program main
    implicit none
    real, dimension(:), allocatable :: guesspoints
    real, dimension(:,:), allocatable :: D
    integer :: n, i

    n = 4
    allocate(guesspoints(n))
    allocate(D(n,n))

    guesspoints = [10.0, 2.0, 7.0, 13.0]
    D = 0.0
    do i = 1,n
        D(i,i) = guesspoints(i)
    end do
    
    write(*,*) shape(D)
    write(*,*) 'Diagonal elements'
    write(*, '(5F6.2)') (D(i,i), i = 1, n)
    write(*, *) "Matrix D"
    write(*, '(5F6.2)') (reshape([D(:,:)],[n,n]))

    deallocate(guesspoints, D)
    
end program main
