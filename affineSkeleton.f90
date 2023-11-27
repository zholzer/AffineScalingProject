program affineScaling
    implicit none
    ! Setup: Take input A, b, c, step size, tolerance ε, and initial guess x0>0
    real, dimension(:,:), allocatable :: A ! switch cases?
    real, dimension(:,:), allocatable :: b
    real, dimension(:,:) allocatable, :: c
    real, dimension(:,:), allocatable :: x0
    real :: stepSize, tolerance, start end ! user input?
    integer :: i

    ! to start we can test with hard coding, maybe one from a known reference
    ! using 7.1 from [6]
    allocate(A(2,4))
    allocate(b(1,2))
    allocate(c(1,4))
    allocate(x0(1,4))
    A = reshape([1 -1 1 0 0 1 0 1],[2,4])
    b = reshape([15 15], [1,2])
    c = reshape([-2 1 0 0],[1,4])
    x0 = reshape([10 2 7 13],[1,4])

    xk = x0
    call cpu_time(start)
    do i = 1,100 ! can change this later
        ! Step 1: Start an an interior feasible point
        vk = b - A*xk
        D = diag(vk,size(vk)) ! probably need to code this too
        ! Step 2: Transform to new space using affine-scaling
        ! 2a. Compute vector of dual estimates
        w = computeVectorOfDualEstimates(A,D,c)
        ! 2b. Compute vector of reduced costs
        call computeVectorOfReducedCosts(A, c, w, r)
        ! 2c. Step 4: Check optimality
        if (conditions) then 
            write(xk, wk) 
            stop
        end if

        ! Step 3: Compute steepest-descent direction
        call computeSteepestDescentDirection(D, r, d)

        ! Step 4: Check for unbounded and constant objective value
        if (condition) then 
            write(unbounded)
            stop
        else if (condition) then 
            write(xk)
        end if

        ! Step 5: Perform the translation
        xk = computeTranslation(xk, stepSize, D, r)
        if (i == 100) then
            write(to many iterations)
            stop
        end if
    end do
    call cpu_time(finish)

    contains
        function diag(v,n) result(D)
            implicit none
            integer :: n
            real, intent(in) :: v(:)
            real :: D(n,n)
            integer :: i
            do i = 1, n
                D(i,i) = v(i)
            end do
        end function

        function computeVectorOfDualEstimates(A,D,c)
            implicit none
            ! compute this
            ! will need inverse function
            stuffInverse = inverse(A, D)
        end function

        subroutine computeVectorOfReducedCosts(A, c, w,r)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:), intent(in) :: c
            real, dimension(:), intent(in) :: w
            real, dimension(size(TRANSPOSE(A), 1)), intent(out) :: r
    
            r = c - MATMUL(TRANSPOSE(A), w)
            !TRANPOSE A 4 x 2
            !w 2 x 1
            !c 4 x 1 
            write(*, *) 'Result vector r:'
            write(*, *) r
        end subroutine computeVectorOfReducedCosts

        function computeSteepestDescentDirection(D, r, d)
            implicit none
            real, dimension(:,:), intent(in) :: D
            real, dimension(:), intent(in) :: r
            real, dimension(:), intent(out) :: d
        
            d = -D * r

            write(*,*)"Steepest-descent direction: ", d
        end function

        function computeTranslation(xk, stepSize, D, r)
            implicit none
            computeTranslation = xk - stepSize*(D**2)*r/norm2((D**2)*r)
        end function

        function inverse(A,D)
            ! compute this probably with LU factorization
        end function
end program affineScaling

! use slides and reference https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2019/10/chapter7.pdf
