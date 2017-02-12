module stack

  !Stuff for storing previous 'guesses'
  integer, parameter :: MaxStackSize = 100
  integer, dimension(3,MaxStackSize) :: GridChoice
  integer, dimension(9,9,MaxStackSize) :: GridStack
  integer :: GridStackNo = 0

contains

  subroutine EmptyStack()
    GridStackNo = 0
  end subroutine EmptyStack


  subroutine PushGrid(grid,gchoice)
    integer, dimension(9,9), intent(in) :: grid
    integer, dimension(3), intent(in) :: gchoice
    GridStackNo = GridStackNo + 1
    if (GridStackNo.gt.MaxStackSize) then
       write(*,*) 'Stack Overflow'
       call Abort
    end if
    GridStack(:,:,GridStackNo) = grid
    GridChoice(:,GridStackNo) = gchoice
  end subroutine PushGrid

  subroutine PopGrid(grid,gchoice,stat)
    integer, dimension(9,9), intent(out) :: grid
    integer, dimension(3), intent(out) :: gchoice
    integer, intent(out) :: stat
    stat = 0
    if (GridStackNo .eq. 0) then
!       write(*,*) 'Stack Underflow'
       stat = 1
       return
!       call Abort
    end if
    grid = GridStack(:,:,GridStackNo)
    gchoice = GridChoice(:,GridStackNo)
    GridStackNo = GridStackNo - 1
  end subroutine PopGrid

end module stack
