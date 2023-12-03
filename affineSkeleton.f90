program affineScaling
    implicit none
    ! Setup: Take input A, b, c, step size, tolerance ε, and initial guess x0>0
    real, dimension(:,:), allocatable :: A, b, D, oneVec, c, xk, dk, r, vk, w ! switch cases?
    real :: stepSize, tolerance, start, finish ! user input?
    real, dimension(1,1) :: temp
    integer :: i,j
    logical :: posdk, zerodk, posR

    ! to start we can test with hard coding, maybe one from a known reference
    ! using 7.1 from [6]
    tolerance = .00001
    allocate(A(2,4))
    allocate(b(1,2))
    allocate(c(4,1))
    allocate(xk(4,1))
    A = reshape([1, -1, 1, 0, 0, 1, 0, 1],[2,4])
    b = reshape([15, 15], [1,2])
    c = reshape([-2, 1, 0, 0], [4,1])
    xk = reshape([10, 2, 7, 13], [4,1])

    allocate(D(size(xk),size(xk)))
    allocate(dk(size(xk),1))
    allocate(r(size(xk),1))
    allocate(vk(size(xk),1))
    allocate(w(size(xk),1))
    allocate(oneVec(1,size(A, dim=2)))

    call cpu_time(start)
    do i = 1,100 ! can change this later
        ! Step 1: Start an an interior feasible point
        !vk = b - A*xk
        D = diagonalMatrix(xk,size(xk)) 
        ! Step 2: Transform to new space using affine-scaling
        ! 2a. Compute vector of dual estimates
        call computeVectorOfDualEstimates(A, D, c, w)
        ! 2b. Compute vector of reduced costs
        call computeVectorOfReducedCosts(A, c, w, r)
        ! 2c. Check optimality
        
        posR = .false.

        do j = 1,size(r)
            if (r(j,1) < 0) then
                continue
            else
                posR = .true.
            end if
        end do

        temp = MATMUL(oneVec,MATMUL(D,r))
        if ((posR .and. (temp(1,1)) < tolerance)) then 
            write(*, *) xk 
            stop
        end if

        ! Step 3: Compute steepest-descent direction
        call computeSteepestDescentDirection(D, r, dk)

        ! get the conditions
        posdk = .false.;

        do j = 1,size(dk)
            if (dk(j,1) > 0) then
                continue
            else
                posdk = .true.
            end if
        end do

        zerodk = .false.;
        do j = 1,size(dk)
            if (dk(j,1) .ne. 0) then
                continue
            else
                zerodk = .true.
            end if
        end do

        ! Step 4: Check for unbounded and constant objective value
        if (posdk) then 
            write(*,*)"This problem is unbounded"
            stop
        else if (zerodk) then 
            write(*,*)"Primal Optimal value: ",xk
            stop
        end if

        ! ^ if the above conditions are unsatisfied, go to the next step

        ! Step 5: Perform the translation
        call computeTranslation(xk, stepSize, D, r)
        if (i == 100) then
            write(*,*) 'To many iterations. Program will be terminated.'
            stop
        end if
    end do
    call cpu_time(finish)

    deallocate(A)
    deallocate(b)
    deallocate(c)
    deallocate(xk)
    deallocate(oneVec)
    deallocate(D)
    deallocate(dk)
    deallocate(r)

    contains
        subroutine computeVectorOfDualEstimates(A,D,c,w)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: c
            real, dimension(size(A, dim=1),size(D, dim=2)) :: H
            real, dimension(size(A, dim=1),size(A, dim=1)) :: F
            real, dimension(size(A, dim=1),1) :: bInv
            real, dimension(size(A, dim=1),1), intent(out) :: w
            
            H = MATMUL(A, D)
            F = MATMUL(H, TRANSPOSE(H))
            !write(*,*) F
            bInv = MATMUL(H, MATMUL(D, c))
            w = conjugateGradient(F,bInv)
        end subroutine computeVectorOfDualEstimates

        subroutine computeVectorOfReducedCosts(A, c, w, r)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:,:), intent(in) :: c
            real, dimension(:,:), intent(in) :: w
            real, dimension(size(A), 1), intent(out) :: r
            
            r = c - MATMUL(A, w)

        end subroutine computeVectorOfReducedCosts

        subroutine computeSteepestDescentDirection(D, r, dk)
            implicit none
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: r
            real, dimension(:,:), intent(out) :: dk
        
            dk = -D * r

        end subroutine computeSteepestDescentDirection

        subroutine computeTranslation(xk, stepSize, D, r)
            implicit none
            real, dimension(:,:), intent(inout) :: xk
            real, intent(in) :: stepSize
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: r

            xk = xk- stepSize*(D**2)*r/norm2((D**2)*r)

        end subroutine computeTranslation

        function diagonalMatrix(v,n) result(D)
            implicit none
            integer :: n
            real, intent(in) :: v(:,:)
            real :: D(n,n)
            integer :: i
            do i = 1, n
                D(i,i) = v(i,1)
            end do
        end function

        function conjugateGradient(F, b) result(w)
            implicit none
            real, intent(in) :: F(:,:)
            real, intent(in) :: b(:,:)
            real, dimension(size(b),1) :: w0, w, r , p, Ap
            real(4) :: tol, max_iter, alpha, beta
            integer :: i

            tol = 1.0E-6
            max_iter = 100
            w0 = 0.0
           ! do i = 1, size(b,1)
               ! w0(i) = 0.0
           ! end do 
          
            w = w0
            r = b - MATMUL(F, w)
            p = r
            i = 0
  
            do while (i < max_iter .and. maxval(abs(r)) > tol)
                Ap = MATMUL(F, p)
                alpha = dot_product(reshape(r, [1]), reshape(r, [1])) / dot_product(reshape(p, [1]), reshape(Ap, [1]))
                w = w + alpha * p
                r = r - alpha * Ap
                beta = dot_product(reshape(r, [1]), reshape(r, [1])) / dot_product(reshape(p, [1]), reshape(Ap, [1]))
                p = r + beta * p
                i = i + 1
            end do
        end function conjugateGradient

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

end program affineScaling

! use slides and reference https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2019/10/chapter7.pdf
