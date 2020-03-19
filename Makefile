#FC = ifort
FC = gfortran

TARGET = bspline.exe

OBJS = pastriangle.o bspline.o


F90FLAGS =  
LIB =

$(TARGET): $(OBJS)
	$(FC) $^ $(LIB) -o $@

$(OBJS): %.o: %.f90
	$(FC) -c -C $(F90FLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -f *.mod *.o $(TARGET)
