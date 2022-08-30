!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!
! File:   modulSampleData.f90
! Author: Truong
!
! Created on June 13, 2018, 9:35 PM
!

module SampleData
  implicit none

  private
  public :: dataReal
  public :: dataRealPtr
  public :: deallocDataReal

  ! Data is stored in dataReal
  type :: dataReal
     real :: x
  end type dataReal

  ! A container for storing dataReal pointers
  type :: dataRealPtr
     type(dataReal), pointer :: p => null()
     contains
     procedure :: print   => printDataRealPtr
     !final     :: deallocDataReal        ! not yet implemented in gfortran 4.8.4, 
                                          ! Intel® Fortran Compiler Classic 2021.6.0 failed to execute too
     ! A workaround: a normal subroutine
  end type dataRealPtr

  ! A Fortran 2003 constructor for dataRealPtr
  interface dataRealPtr
      module procedure dataRealPtrConstructor
  end interface dataRealPtr

  contains

  ! Implement the constructor
  function dataRealPtrConstructor(val)
      implicit none
      type(dataRealPtr) :: dataRealPtrConstructor
      real, intent(in)  :: val

      if (.not. associated(dataRealPtrConstructor%p)) then
          allocate(dataRealPtrConstructor%p)
          dataRealPtrConstructor%p%x = val
      endif
  end function

  ! Method print
  subroutine printDataRealPtr(this)
      implicit none
      class(dataRealPtr), intent(in) :: this

      if (.not. associated(this%p)) return

      select type (this)
          type is (dataRealPtr)
              write(6,'(f20.6)') this%p%x
              write(6,*) '**************************'
      end select
  end subroutine

  ! Destructor
  subroutine deallocDataReal(this)
      implicit none
      type(dataRealPtr) :: this

      if (associated(this%p)) then
          print*, 'Deallocating DataReal'
          deallocate(this%p)
          nullify(this%p)
      endif
  end subroutine

end module SampleData

