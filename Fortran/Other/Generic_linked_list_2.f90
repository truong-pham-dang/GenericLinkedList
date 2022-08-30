! The problem is how to treat generic lists in Fortran 90. 
! Lists can be (1) homogeneous (elements all of the same type) or 
! (2) heterogeneous.
! 
! (1) Often there is a need to work with many (homogeneous) lists which
! may be of different types. Patrice Lignelet has shown how generic list
! properties may be logically treated in Fortran 90. However it still
! appears that the list operations (initialization, addition/removal of
! elements etc) must be separately defined for each list type, which
! leads to considerable duplication of code.
! 
! (2) Jean Vezina has shown how to handle a heterogeneous list by employing the 
! F90 TRANSFER() function. 
! 
! Peter McGavin at Industrial Research Limited (p.mcgavin@irl.cri.nz)
! has constructed a species of generic list for Fortran 90. The method
! is based on 2 ideas: the properties of the TRANSFER() function, and
! the fact that a pointer to a derived data type also points to the
! *first field* within the data type (and conversely). 
! 
! Since the TRANSFER() function does not accept pointer arguments the
! method requires the introduction of 2 auxilliary data types which
! contain the pointers, one in the generic list module and one for each
! list type in the calling program (the same construction is adopted
! when defining "arrays of pointers"). In spite of this complication the
! method represents a big saving, both conceptually and practically, when
! many lists of different types are involved.
! 
! To make clear the method we present a simple generic list module
! together with a calling program. The list module defines a
! uni-directional linked list with a few sample operations, but
! obviously more complicated generic lists could be substituted in its
! place (eg include back pointers).
! 
! Roger Young
! Peter McGavin
! 
! .........................................................................

MODULE Generic_List
! Defines a generic uni-directional linked list with a few sample operations

IMPLICIT NONE

PRIVATE

PUBLIC :: &
     Link_Type,        &! Put a Link_Type field first in your structure
     Link_Ptr_Type,    &! Mold this to and from your type ptr with TRANSFER
     List_Type          ! You should declare a List_Type variable

PUBLIC :: & 
     LI_Init_List,        &! Initialise the List_Type variable before use
     LI_Get_Head,         &! Returns the first Link in the list
     LI_Get_Next,         &! Return the next Link after a given one
     LI_Add_To_Head,      &! Add a Link to the head of the list
     LI_Remove_Head,      &! Remove the first Link and return it
     LI_Get_Len,          &! Compute list length
     LI_Associated,       &! Check if list member is associated
     LI_Check_List         ! Aborts program if list is invalid or corrupt

TYPE Link_Type
  PRIVATE
  TYPE(Link_Type), POINTER :: Next
END TYPE Link_Type

! Auxilliary data type required for the transfer function 
TYPE Link_Ptr_Type       ! Use TRANSFER() function to mold Link_Ptr_Type
  PRIVATE                ! to your pointer type and vice versa
  TYPE(Link_Type), POINTER :: P
END TYPE Link_Ptr_Type

TYPE List_Type             
  PRIVATE
  TYPE(Link_Type) :: Head   ! Dummy Link always at head of list
END TYPE List_Type

CONTAINS

!-----------------------------------------------------------------------
SUBROUTINE Abort(Message)
IMPLICIT NONE
CHARACTER *(*) Message

WRITE(6,*) Message
WRITE(6,*) 'Program aborted'
STOP

END SUBROUTINE Abort

!-----------------------------------------------------------------------
SUBROUTINE LI_Check_List(List,Message)
IMPLICIT NONE
TYPE(List_Type) List
CHARACTER *(*) Message

IF(.NOT.ASSOCIATED(List%Head%Next))THEN
   WRITE(6,*) Message
   CALL Abort('List is not initialised in call to LI_Check_List()')
ENDIF

END SUBROUTINE LI_Check_List

!-----------------------------------------------------------------------
SUBROUTINE LI_Init_List(List)
  IMPLICIT NONE
  TYPE(List_Type),INTENT(INOUT),TARGET :: List

  NULLIFY(List%Head%Next)

  RETURN
END SUBROUTINE LI_Init_List

!-----------------------------------------------------------------------
SUBROUTINE LI_Add_To_Head(Link,List)
  IMPLICIT NONE
  TYPE(List_Type),INTENT(INOUT)     :: List
  TYPE(Link_Ptr_Type),INTENT(INOUT) :: Link

  Link%P%Next => List%Head%Next
  List%Head%Next => Link%P

  RETURN
END SUBROUTINE LI_Add_To_Head

!-----------------------------------------------------------------------
INTEGER FUNCTION LI_Get_Len(List)
  IMPLICIT NONE
  TYPE(List_Type), INTENT(IN),TARGET :: LIST
  TYPE(Link_Ptr_Type) :: Link
  INTEGER N

  Link%P => List%Head
  N = 0
  DO WHILE(ASSOCIATED(Link%P%Next))
     Link%P => Link%P%Next
     N = N+1
  ENDDO
  LI_Get_Len = N

  RETURN
END FUNCTION LI_Get_Len

!-----------------------------------------------------------------------
FUNCTION LI_Associated(Link)
  IMPLICIT NONE
  LOGICAL :: LI_Associated
  TYPE(Link_Ptr_Type),INTENT(IN) :: Link

  LI_Associated = .FALSE.
  IF(ASSOCIATED(Link%P))LI_Associated=.TRUE.

  RETURN
END FUNCTION LI_Associated

!-----------------------------------------------------------------------
FUNCTION LI_Get_Next(Link)
  IMPLICIT NONE
  Type(Link_Ptr_Type)           :: LI_Get_Next
  TYPE(Link_Ptr_Type),INTENT(IN) :: Link

  IF(.NOT.ASSOCIATED(Link%P%Next))THEN
     NULLIFY(LI_Get_Next%P)
  ELSE   
     LI_Get_Next%P => Link%P%Next
  ENDIF

  RETURN
END FUNCTION LI_Get_Next

!-----------------------------------------------------------------------
FUNCTION LI_Get_Head(List)
  IMPLICIT NONE
  TYPE(Link_Ptr_Type)               :: LI_Get_Head
  TYPE(List_Type),INTENT(IN),TARGET :: List

  LI_Get_Head%P => List%Head%Next

  RETURN
END FUNCTION LI_Get_Head

!-----------------------------------------------------------------------
FUNCTION LI_Remove_Head(List)
  IMPLICIT NONE
  TYPE(Link_Ptr_Type)                  :: LI_Remove_Head
  TYPE(List_Type),INTENT(INOUT),TARGET :: List
  TYPE(Link_Ptr_Type) :: Link

  Link%P => List%Head%Next
  IF(ASSOCIATED(Link%P))THEN
     List%Head%Next => Link%P%Next
     NULLIFY(Link%P%Next)
  ENDIF
     LI_Remove_Head%P => Link%P

  RETURN
END FUNCTION LI_Remove_Head

!-----------------------------------------------------------------------
END MODULE Generic_List

!..........................................................................
PROGRAM MAIN

! Defines and manipulates list(s) of a user-defined type all based on
! a single generic list type. 

USE Generic_List, ONLY : Link_Ptr_Type,Link_Type,List_Type
USE Generic_List, ONLY : LI_Init_List,LI_Add_To_Head,LI_Get_Head,&
     LI_Remove_Head,LI_Get_Next,LI_Associated,LI_Get_Len

USE IFPORT !Comment it if you don't use Visual Fortran

IMPLICIT NONE

! User-defined list element
! The Link_Type field MUST be the FIRST in the user-defined list element 
! Note pointer to data so as to easily create sublists
TYPE User_Type
  TYPE(Link_Type) :: Link
  TYPE(User_Data_Type), POINTER :: Data  
END TYPE User_Type

TYPE User_Data_Type
  INTEGER :: Index
  INTEGER :: User_Stuff
END TYPE User_Data_Type

! Auxilliary data type required for the transfer function
TYPE User_Ptr_Type
  TYPE(User_Type), POINTER :: P
END TYPE User_Ptr_Type

TYPE(List_Type)      :: User_List,Sublist
TYPE(Link_Ptr_Type)  :: Link,Sublink
TYPE(User_Ptr_Type)  :: User,Subuser

INTEGER I,N


! Initialize list
CALL LI_Init_List(User_List)

! Build up list (add to stack)
N=5
DO I=1,N
   ALLOCATE(User%P); ALLOCATE(User%P%Data)
   User%P%Data%Index = I
   User%P%Data%User_Stuff = IRAND(I)
   Link = TRANSFER(User,Link)
   CALL LI_Add_To_Head(Link,User_List)
ENDDO

! Cycle through list
Link = LI_Get_Head(User_List)
DO WHILE(LI_Associated(Link))
   User = TRANSFER(Link,User)
   WRITE(6,*)User%P%Data%Index,User%P%Data%User_Stuff
   Link = LI_Get_Next(Link)
ENDDO

! Find list length
WRITE(6,*)
WRITE(6,*)'Length = ',LI_Get_Len(User_List)
WRITE(6,*)

! Make a sublist
CALL LI_Init_List(Sublist)
Link = LI_Get_Head(User_List)
DO WHILE(LI_Associated(Link)) 
   User = TRANSFER(Link,User)
   IF(User%P%Data%Index.NE.4)THEN
   ALLOCATE(Subuser%P)
   Subuser%P%Data => User%P%Data	
   Sublink = TRANSFER(Subuser,Sublink)
   CALL LI_Add_To_Head(Sublink,Sublist)
   ENDIF
   Link = LI_Get_Next(Link)
ENDDO
! Cycle through sublist
Sublink = LI_Get_Head(Sublist)
DO WHILE(LI_Associated(Sublink))
   Subuser = TRANSFER(Sublink,Subuser)
   WRITE(6,*)Subuser%P%Data%Index,Subuser%P%Data%User_Stuff
   Sublink = LI_Get_Next(Sublink)
ENDDO
! Find sublist length
WRITE(6,*)
WRITE(6,*)'Sublength = ',LI_Get_Len(Sublist)
WRITE(6,*)

! Remove from list (stack)
WRITE(6,*)
DO
   Link = LI_Remove_Head(User_List)
   IF(.NOT.LI_Associated(Link))EXIT
   User = TRANSFER(Link,User)
   WRITE(6,*)User%P%Data%Index,User%P%Data%User_Stuff
   DEALLOCATE(User%P)
ENDDO

! Find list length
WRITE(6,*)
WRITE(6,*)'Length = ',LI_Get_Len(User_List)
WRITE(6,*)

END PROGRAM MAIN
!..........................................................................

