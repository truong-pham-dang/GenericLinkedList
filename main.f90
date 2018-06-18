!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!
! File:   main.f90
! Author: Truong
!
! Created on June 13, 2018, 9:31 PM
!

program testGenericLinkedList
  use GenericLinkedList
  use SampleData
  implicit none

  call exec


  contains

    subroutine exec
        implicit none

        type(list_node_t), pointer :: list => null()
        type(dataRealPtr) :: ptr

        ! Allocate a new data element
        ptr = dataRealPtr(2.7183)

        ! Initialize the list with the first data element
        call list_init(list, transfer(ptr, list_data))
        print *, 'Initializing list with data:'
        call ptr%print()

        ! Allocate a second data element
        ptr = dataRealPtr(0.5772)

        ! Insert the second into the list
        call list_insert(list, transfer(ptr, list_data))
        print *, 'Inserting node with data:'
        call ptr%print()

        ! Retrieve data from the second node and free memory
        ptr = transfer(list_get(list_next(list)), ptr)
        print *, 'Second node data:'
        call ptr%print()


        ! Retrieve data from the head node and free memory
        ptr = transfer(list_get(list), ptr)
        print *, 'Head node data:'
        call ptr%print()


        ! Free the list
        call list_free(list)

    end subroutine

end program testGenericLinkedList

