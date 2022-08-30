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
        type(list_node_t), pointer :: listTranversal => null()
        type(list_node_t), pointer :: elem => null()
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
        print *, 'Inserting second node with data:'
        call ptr%print()

        ! Allocate the third data element
        ptr = dataRealPtr(10.0002)
        call list_insert(list, transfer(ptr, list_data))
        print *, 'Inserting third node with data:'
        call ptr%print()


        ! Delete second node data
        listTranversal => list
        do while(associated(listTranversal))
            ptr = transfer(list_get(listTranversal), ptr)
            if (abs(ptr%p%x - 10.0002) <= 1.0d-9)then
                elem => listTranversal
                call list_delete_element_next_to(listTranversal)
            endif
            listTranversal => list_next(listTranversal)
        enddo




        ! All elements in the list
        print*, 'All elements in the list'
        listTranversal => list
        do while (associated(listTranversal))
            ptr = transfer(list_get(listTranversal), ptr)
            call ptr%print()
            listTranversal => list_next(listTranversal)
        end do


        ! Free the list
        call list_free(list)

    end subroutine

end program testGenericLinkedList

