program main
    implicit none
    real, dimension(:), allocatable :: guesspoints
    real, dimension(:,:), allocatable :: D
    integer :: n, m, i, x

    n = 4
    allocate(guesspoints(n))
    allocate(D(n,n))
    
    D=0
    guesspoints = [10.0, 2.0, 7.0, 13.0]
    D = diagonalMatrix(guesspoints,size(guesspoints))
    
    write(*,*) shape(D)
    write(*,*) 'Diagonal elements'
    write(*, '(5F6.2)') (D(i,i), i = 1, n)
    write(*, *) "Matrix D"
    x = displayMatrix(D, n, n)

    deallocate(guesspoints, D)

    contains
        function diagonalMatrix(v,n) result(D)
            implicit none
            integer :: n
            real, intent(in) :: v(:)
            real :: D(n,n)
            integer :: i
            do i = 1, n
                D(i,i) = v(i)
            end do
          end function

        integer function displayMatrix(D, n, m)
            implicit none
            integer, intent(in) :: n, m
            integer :: i;
            real, dimension(n,m) :: D
            do i = 1,m
                write(*, '(5F6.2)') D(:,i)
            end do
            displayMatrix = 1
        end function displayMatrix
end program main
