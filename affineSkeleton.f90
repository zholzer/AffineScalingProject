program affineScaling
    implicit none
    ! Setup: Take input A, b, c, step size, tolerance ε, and initial guess x0>0
    real, dimension(:,:), allocatable :: A, b, D, oneVec, c, xk, dk, r, vk, w ! switch cases?
    real :: stepSize, tolerance, start, finish ! user input?
    real, dimension(1,1) :: temp
    integer :: i,j, itNum, stat
    logical :: posdk, zerodk, posR
    character :: testChoice

    ! to start we can test with hard coding, maybe one from a known reference
    ! using 7.1 from [6]
    write(*,*) 'Please enter a test case as "A", "B", "C", "D", or "E": '
    read(*,*, iostat=stat) testChoice

 1   select case (testChoice)
        case ('A')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0], [2, 4]) 
            b = reshape([15.0, 15.0], [1,2])
            c = reshape([-2.0, 1.0, 0.0, 0.0],[4,1])
            xk = reshape([10.0, 2.0, 7.0, 13.0],[4,1])
            ! answer is (30 15 0 0)

        case ('B')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([0.8, 0.0, .2, 0.0, 0.0, .1, 0.0, 1.9], [2, 4]) 
            b = reshape([1.0, 2.0], [2,1])
            c = reshape([-0.8, -0.1, .2, 1.9],[4,1])
            xk = reshape([1, 1, 1, 1],[4,1])
            ! answer is (1 2 0 0)
        ! https://homepages.rpi.edu/~mitchj/handouts/interior_html/interior.html
        case ('C')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 1.0, 1.0, 0.0, 2.0, 1.0, 0.0, 1.0], [2, 4]) 
            b = reshape([40.0, 60.0], [2,1])
            c = reshape([1.0, -2.0, 0.0, 0.0],[4,1])
            xk = reshape([15.0, 15.0, 10.0, 15.0],[4,1])
            ! Do not know the answer
        !    https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2021/10/Lecture-6.pdf

        case ('D')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 2.0, 1.0, 0.0, 2.0, 1.0, 0.0, 1.0], [2, 4]) 
            b = reshape([3.0, 3.0], [2,1])
            c = reshape([-1.0, -1.0, 2.44, 1.97],[4,1])
            xk = reshape([.5, .03, 0.0, 0.0],[4,1])
        !    https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2021/10/Lecture-6.pdf

        case ('E')
            allocate(A(10,10))
            allocate(b(1,10))
            allocate(c(10,1))
            allocate(xk(10,1))
            A = reshape([1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, & 
                0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 6.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0], &    
                [10, 10]) 
                b = reshape([1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], [1,10])
                c = reshape([1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],[10,1])
                xk = reshape([1.0, .4, .2, .1, .1, .1, .1, .05, .05, .01],[10,1]) 
            ! answer is (1 0 ... 0), telescoping


        case ('F')
            allocate(A(10,10))
            allocate(b(1,10))
            allocate(c(10,1))
            allocate(xk(10,1))
            A = reshape([1.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, & 
                -1.0, 2.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, -1.0, 3.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, -1.0, 4.0, -1.0, 0.0, 0.0, 0.0, 3.0, 0.0, &
                0.0, 0.0, 0.0, -1.0, 5.0, -1.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, -1.0, 6.0, -1.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 8.0, 0.0, 0.0, -1.0, 7.0, -1.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 7.0, 0.0, 0.0, -1.0, 8.0, -1.0, -2.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 9.0, -1.0, &
                1.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, -1.0, 10.0], &    
                [10, 10]) 
            b = reshape([1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], [1,10])
            c = reshape([1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0],[10,1])
            xk = reshape([1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10],[10,1])

        case ('G')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 4.0, 3.0, 1.0, 1.0, 1.0], [3, 2]) 
            b = reshape([24.0, 21.0, 9.0], [1,3])
            c = reshape([2.0, 5.0],[2,1])
            xk = reshape([.1, .1],[2,1])

        case ('H')
            allocate(A(2,2))
            allocate(b(1,2))
            allocate(c(2,1))
            allocate(xk(2,1))
            A = reshape([1.0, 1.0, 1.0, 0.0], [2, 2]) 
            b = reshape([2.0, 1.0], [1,2])
            c = reshape([-3.0, -1.0],[2,1])
            xk = reshape([.6, .6],[2,1])

        case default
            write(*,*) 'Please enter a valid test case selection.'
            write(*,*) 'Please enter a test case as "A", "B", "C", "D", or "E": '
            read(*,*, iostat=stat) testChoice
            go to 1

    end select
    
    write(*,*) 'Please enter a step size between 0 and 1 exclusive (reccomended value is .1): '
    read(*,*, iostat=stat) stepSize
    
    do while ((stat .ne. 0) .or. (stepSize <= 0) .or. (stepSize >= 1))      
        write(*,*) 'Please input a number from 0 to 1 exclusive. Retry.'
        write(*,*) 'Please enter a step size between 0 and 1 (reccomended value is .1): '
        read(*,*, iostat=stat) stepSize
    end do

    tolerance = 10**(-9)

    allocate(D(size(xk),size(xk)))
    allocate(dk(size(xk),1))
    allocate(r(size(xk),1))
    allocate(vk(size(xk),1))
    allocate(w(size(A, dim=1),1))
    allocate(oneVec(1,size(A, dim=2)))

    call cpu_time(start)
    itNum = 1000
    do i = 1,itNum ! can change this later
        ! Step 1: Start an an interior feasible point
        !vk = b - A*xk
        D = diagonalMatrix(xk,size(xk)) 
        
        ! Step 2: Transform to new space using affine-scaling
        ! 2a. Compute vector of dual estimates
        call computeVectorOfDualEstimates(A, D, c, w)
       
        ! 2b. Compute vector of reduced costs
        call computeVectorOfReducedCosts(A, c, w, r)
        ! 2c. Check optimality
        
        posR = .true.

        do j = 1,size(r)
            if (r(j,1) > 0) then
                continue
            else
                posR = .false.
            end if
        end do

        temp = MATMUL(oneVec,MATMUL(D,r))
        
        if ((posR .and. (temp(1,1)) < tolerance)) then ! doesnt work
            write(*, *) 'Primal optimal value: ', xk 
            call cpu_time(finish)
            write(*,*) 'Code took ', finish - start, 'seconds with ', i, ' iterations.'
            stop
        end if

        ! Step 3: Compute steepest-descent direction
        call computeSteepestDescentDirection(D, r, dk)
        ! get the conditions
        posdk = .true.;

        do j = 1,size(dk)
            if (dk(j,1) < 0) then
                continue
            else
                posdk = .false.
            end if
        end do

        zerodk = .true.;
        do j = 1,size(dk)
            if (dk(j,1) <= tolerance) then
                continue
            else
                zerodk = .false.
            end if
        end do
       
        ! Step 4: Check for unbounded and constant objective value
        !if (posdk) then 
            !write(*,*)"This problem is unbounded"
            !stop
         if (zerodk) then 
            write(*,*)"Primal Optimal value: ",xk
            call cpu_time(finish)
            write(*,*) 'Code took ', finish - start, 'seconds with ', i, ' iterations.'
            stop
        end if

        ! ^ if the above conditions are unsatisfied, go to the next step

        ! Step 5: Perform the translation
        call computeTranslation(xk, stepSize, dk, D)
        if (i == itNum) then
            write(*,*) "Primal Optimal value: ", xk
            call cpu_time(finish)
            write(*,*) 'Code took ', finish - start, 'seconds with ', itNum, ' iterations.'
            stop
        end if
    end do

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
            bInv = MATMUL(H, MATMUL(D, c))
            w = conjugateGradient(F,bInv)
        end subroutine computeVectorOfDualEstimates

        subroutine computeVectorOfReducedCosts(A, c, w, r)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:,:), intent(in) :: c
            real, dimension(:,:), intent(in) :: w
            real, dimension(size(A), 1), intent(out) :: r
            
            r = c - MATMUL(transpose(A), w)

        end subroutine computeVectorOfReducedCosts

        subroutine computeSteepestDescentDirection(D, r, dk)
            implicit none
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: r
            real, dimension(:,:), intent(out) :: dk
        
            dk = MATMUL(-1*D, r)
            !write(*,*) dk
        end subroutine computeSteepestDescentDirection

        subroutine computeTranslation(xk, stepSize, dk, D)
            implicit none
            real, dimension(:,:), intent(inout) :: xk, dk
            real, intent(in) :: stepSize
            real :: beta
            real, dimension(:,:), intent(in) :: D

            beta = stepSize/maxval(-dk)
            xk = xk + beta*MATMUL(D,dk)
            !write(*,*) beta
            !xk = xk - stepSize*MATMUL(D**2,dk)/norm2(MATMUL(D,dk))

        end subroutine computeTranslation

        function diagonalMatrix(v,n) result(D)
            implicit none
            integer :: n
            real, intent(in) :: v(:,:)
            real :: D(n,n)
            integer :: i
            D = 0.0
            do i = 1, n
                D(i,i) = v(i,1)
            end do
        end function

        function conjugateGradient(F, b) result(wFinal)
            implicit none
            real, intent(in) :: F(:,:)
            real, intent(in) :: b(:,:)
            real, dimension(size(b)) :: w, r , p, Ap
            real(4) :: tol, max_iter, alpha, beta
            real, dimension(size(w),1) :: wFinal
            integer :: i
            tol = 1.0E-6
            max_iter = 100
            w = 0.0
          
            r = reshape(b, [size(b)]) - MATMUL(F, w)
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
            wFinal = reshape(w, [size(w),1])
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
