CF         = gfortran
FFLAGS     = -O3
LD         = gfortran
LDFLAGS    = 
PREPROC    = 

OBJS =  modulGenericLinkedList.o \
        modulSampleData.o  \
	main.o \


.SUFFIXES: .o .f90 .f
.f90.o:
	$(LD) -c $(FFLAGS) $<
.f.o:
	$(LD) -c $(FFLAGS) $<

GenericLinkedList :$(OBJS) 
	$(LD) $(LDFLAGS) -o $@ $(OBJS)

clean :
	rm -f GenericLinkedList *.o core *.mod

