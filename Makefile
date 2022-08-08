ifeq ($(TARGET),intel)
        CF = ifort
        FFLAGS =
        LD = ifort
        ifeq (${OS},macos)
        	LDFLAGS = -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib
        else
        	LDFLAGS =
        endif
else
        CF = gfortran
        FFLAGS = -O3
        LD = gfortran
        LDFLAGS =
endif

PREPROC    = 

OBJS =  modulGenericLinkedList.o \
        modulSampleData.o  \
	main.o \


.SUFFIXES: .o .f90 .f
.f90.o:
	$(LD) -c $(FFLAGS) $<
.f.o:
	$(LD) -c $(FFLAGS) $<

GenericLinkedList.exe :$(OBJS) 
	$(LD) $(LDFLAGS) -o $@ $(OBJS)

clean :
	rm -f GenericLinkedList.exe *.o core *.mod

