module utils
  use Stack

contains


  subroutine SolveGrid(grid)
    integer, dimension(9,9), intent(inout) :: grid

    logical, dimension(9,9,9) :: possibs
    logical :: changed

    do while(any(grid.eq.0))
       call PossibleEntries(grid,possibs)
       call FindMove(grid,possibs,changed)

       if (IsValidGrid(grid,possibs)) then
          if (.not. changed) then
             call TakeGuess(grid, possibs)
          end if
       else
          call RetryGuess(grid)
       end if
    end do

    !Clear rubbish out of the stack
    Call EmptyStack()

  end subroutine SolveGrid


  recursive subroutine RetryGuess(grid)
    integer, dimension(9,9), intent(inout) :: grid
    integer, dimension(3) :: choice, nextchoice
    logical, dimension(9,9,9) :: possibles
    integer :: i,j,n, m
    logical :: found
    integer :: stat

    !Ok our last guess failed so lets see if we can take another in that level
    call PopGrid(grid,choice,stat)

    if (stat .ne. 0) then !Cannot pop - just loop
       write(*,*) 'stuck pop'
       call writegrid(grid)
       return
    end if



    i = choice(1)
    j = choice(2)
    n = choice(3)

    !Recalculate possibles
    call PossibleEntries(grid,possibles)
    found = .false.

    !Lets find the next choice
    do m=n+1,9
       if (possibles(j,i,m)) then
          found = .true.
          nextchoice = (/ i,j,m /)
       end if
    end do

    if (.not. found) then
       call RetryGuess(grid)
       return
    end if

    call PushGrid(grid,nextchoice)
    grid(j,i) = nextchoice(3)

  end subroutine RetryGuess


  subroutine TakeGuess(grid, possibles)
    integer, dimension(9,9), intent(inout) :: grid
    logical, dimension(9,9,9), intent(inout) :: possibles
    integer, dimension(3) :: thischoice
    integer :: minchoices, mini, minj
    integer :: choices, i,j, n

    mini = 0
    minj = 0
    choices = 0
    minchoices = 100

    !Find the item with the least possible choices
    do i=1,9
       do j=1,9
          if (grid(j,i).ne.0) cycle
          choices = count(possibles(j,i,:))
          if (choices.lt.minchoices) then
             minchoices = choices
             mini = i
             minj = j
          end if
       end do
    end do

    !Pick the first choice
    do n=1,9
       if (possibles(minj,mini,n)) then
          choices = n
          exit
       end if
    end do

    !Sanity check
    if (choices .eq. 0 .or. mini .eq. 0 .or. minj .eq. 0) then
       call writegrid(grid)
       write(*,*) mini, minj, choices, minchoices
       STOP 'Something weird has happened'
    end if

    !Push the choice to the stack and do it
    thischoice = (/ mini, minj, choices /)
    call PushGrid(grid,thischoice)
    grid(minj,mini) = choices
  end subroutine TakeGuess


  logical function IsValidGrid(grid, possibles)
    integer, dimension(9,9) :: grid
    logical, dimension(9,9,9), intent(in) :: possibles
    integer :: ct
    integer :: i,j,n

    IsValidGrid = .true.

    do n=1,9
       !Check rows
       do i=1,9
          ct = count(grid(i,:) .eq. n)
          if (ct .gt. 1) then
             IsValidGrid = .false.
             return
          end if
          !Cols...
          ct = count(grid(:,i) .eq. n)
          if (ct .gt. 1) then
             IsValidGrid = .false.
             return
          end if
       end do

       !Lastly check if boxes only contain 1 of each number
       do i=1,7,3
          do j=1,7,3
             ct = count(grid(j:j+2,i:i+2).eq.n)
             if (ct .gt. 1) then
                IsValidGrid = .false.
                return
             end if
          end do
       end do
    end do

    !Finally, check each grid point is either assigned or has choices
    do i=1,9
       do j=1,9
          if (grid(j,i).eq.0) then
             ct = count(possibles(j,i,:))
             if (ct.lt.1) then
                IsValidGrid = .false.
                return
             end if
          end if
       end do
    end do

  end function IsValidGrid



  subroutine PossibleEntries(grid,possibles)
    integer, dimension(9,9), intent(in) :: grid
    logical, dimension(9,9,9), intent(inout) :: possibles
    integer :: i,j, squarex, squarey, p,q
    possibles = .true.

    do i=1,9
       do j=1,9
          if (grid(j,i).eq.0) cycle

          !Already have this one and it is fixed
          possibles(j,i,:) = .false.

          !Not zero so remove this number from every item in row, column and box
          possibles(:,i,grid(j,i)) = .false.
          possibles(j,:,grid(j,i)) = .false.

          squarex = ((i-1)/3) * 3 + 1
          squarey = ((j-1)/3) * 3 + 1

          do p=squarex,squarex+2
             do q=squarey,squarey+2
                possibles(q,p,grid(j,i)) = .false.
             end do
          end do

       end do
    end do
  end subroutine PossibleEntries

  subroutine FindMove(grid,possibles,changed)
    integer, dimension(9,9), intent(inout) :: grid
    logical, dimension(9,9,9), intent(in) :: possibles
    logical, intent(out) :: changed
    integer :: i, n, j, p,q
    changed = .false.

    !First see if any squares only have one choice anywhere
    do i=1,9
       do j=1,9
          if (count(possibles(j,i,:)).eq.1) then
             do n=1,9
                if (possibles(j,i,n)) then
                   grid(j,i) = n
                   changed = .true.
                   exit
                end if
             end do
          end if
       end do
    end do

    !Look over numbers
    do n=1,9
       !Loop columns
       do i=1,9
          if (count(possibles(:,i,n)).eq.1) then
             !Find the move
             do j=1,9
                if (possibles(j,i,n)) then
                   grid(j,i) = n
                   changed = .true.
                   exit
                end if
             end do
          end if
       end do

       !...rows
       do i=1,9
          if (count(possibles(i,:,n)).eq.1) then
             !Find the move
             do j=1,9
                if (possibles(i,j,n)) then
                   grid(i,j) = n
                   changed = .true.
                   exit
                end if
             end do
          end if
       end do

       !Finally count all in this square...
       do i=1,7,3
inner:    do j=1,7,3
             !Lets see if this square contains the number
             if (count(possibles(j:j+2,i:i+2,n)).eq.1) then
                !look over these
                do p=i,i+2
                   do q=j,j+2
                      if (possibles(q,p,n)) then
                         grid(q,p) = n
                         cycle inner
                      end if
                   end do
                end do


             end if
          end do inner
       end do
    end do
  end subroutine FindMove



  subroutine ReadGrid(filename,grid, nGrids)
    character(len=*), intent(in) :: filename
    integer, dimension(9,9,nGrids), intent(out) :: grid
    integer, intent(in) :: nGrids

    character(len=5) :: stuff
    integer :: gridno, i,j
    open(unit=10,file=trim(filename))
    do j=1,nGrids
       read(10,'(A5,I2)') stuff, gridno

       !Read 9 lines
       do i=1,9
          read(10,'(9I1)') grid(:,i,j)
       end do
    end do
    close(unit=10)
  end subroutine ReadGrid

  subroutine ReadGrid2(filename,grid, nGrids)
    character(len=*), intent(in) :: filename
    integer, dimension(9,9,nGrids), intent(out) :: grid    
    integer, intent(in) :: nGrids
    integer :: j
    open(unit=10,file=trim(filename))
    do j=1,nGrids
       read(10,'(81I1)') grid(:,:,j)
    end do
    close(unit=10)
  end subroutine ReadGrid2

  subroutine WriteGrid(grid)
   integer, dimension(9,9), intent(inout) :: grid
   integer :: i
   write(*,*) '-----------'
900 format (3I1,' ',3I1,' ', 3I1)
   do i=1,9
      write(*,900) grid(:,i)
      if (mod(i,3).eq.0) write(*,*)
   end do
 end subroutine WriteGrid



  


end module utils
