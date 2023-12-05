program main
    implicit none 
    real, dimension(2,4) :: A
    real, dimension(4,1) :: c
    real, dimension(:), allocatable :: x0
    real, dimension(:,:), allocatable :: H
    real, dimension(:,:), allocatable :: F
    real, dimension(:,:), allocatable  :: g
    real, dimension(:), allocatable  :: b
    real, dimension(:,:), allocatable :: D
    real, dimension(:), allocatable :: w
    integer :: n, i
    n = 4
    allocate(x0(n))
    allocate(D(n,n))
    allocate(b(size(A,1)))
    !allocate(w(2))

    A = reshape([1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0], [2, 4]) 
    c = reshape([-2.0, 1.0, 0.0, 0.0],[4,1])
    x0 = [10.0, 2.0, 7.0, 13.0]

    D = 0.0
    D = diagonalMatrix(x0, size(x0))

    !write(*, *) "Matrix D"
    !Dk = displayMatrix(D, n, n)
    H = MATMUL(A, D)
    F = MATMUL(H, TRANSPOSE(H))
    g = MATMUL(H, MATMUL(D, c))
    b = 0.0
    do i = 1, size(g)
        b(i) = g(i,1)
    end do
    w = conjugateGradient(F,b)
   
    write(*,*) w
    deallocate(x0, D, b)
    
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

        integer function displayMatrix(D, m, n)
            implicit none
            integer, intent(in) :: n, m
            integer :: i, j  
            real, dimension(:,:) :: D
            do i = 1, m 
                write(*, '(F8.2," ")') (D(i,j), j = 1, n) 
            end do
            displayMatrix = 1
        end function displayMatrix

        function conjugateGradient(F, b) result(w)
            implicit none
            real, intent(in) :: F(:,:)
            real, intent(in) :: b(:)
            real, dimension(size(b)) :: w0, w, r , p, Ap
            real(4) :: tol, max_iter, alpha, beta
            integer :: i

            tol = 1.0E-6
            max_iter = 100
            w0 = 0.0
            do i = 1, size(b,1)
                w0(i) = 0.0
            end do 
          
            w = w0
            r = b - MATMUL(F, w)
            p = r
            i = 0
  
            do while (i < max_iter .and. maxval(abs(r)) > tol)
                Ap = MATMUL(F, p)
                alpha = dot_product(r, r) / dot_product(p, Ap)
                w = w + alpha * p
                r = r - alpha * Ap
                beta = dot_product(r, r) / dot_product(p, Ap)
                p = r + beta * p
                i = i + 1
            end do
        end function conjugateGradient
    
end program main 
