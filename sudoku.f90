program sudoku
  use Stack
  use utils
  implicit none


  !Inputs..
  character(len=100), parameter :: filename = 'sudoku.txt'
  integer, parameter :: nGrids = 50
  
!  character(len=100), parameter :: filename = 'lotsmore.txt'
!  integer, parameter :: nGrids = 36628

  !Entire set of problems
  integer, dimension(9,9,nGrids) :: allgrids

  !Working set grid and possible numbers
  integer, dimension(9,9) :: grid
  logical, dimension(9,9,9) :: possibs

  !State variables
  integer :: i


  !Count up the result as required....
  integer, dimension(3) :: sumdigits = (/ 100, 10, 1 /)
  integer :: totalsum = 0

  call ReadGrid(filename,allgrids, nGrids)

  do i=1,nGrids

     grid = allgrids(:,:,i)

     call SolveGrid(grid)
     call PossibleEntries(grid,possibs)     
     totalsum = totalsum + sum(grid(1:3,1) * sumdigits)

     !Sanity check

!     if (.not. isvalidgrid(grid,possibs)) then
        write(*,*) 'finished',i,'valid:',isvalidgrid(grid,possibs), sum(grid(1:3,1) * sumdigits)
!        call abort()
!     end if
  end do
  write(*,*) totalsum
  

 
end program sudoku
