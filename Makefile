#FC = ifort
FC = gfortran

TARGET = spline._inter.exe

OBJS = sweep_sub.o main.o


F90FLAGS =  
LIB =

$(TARGET): $(OBJS)
	$(FC) $^ $(LIB) -o $@

$(OBJS): %.o: %.f90
	$(FC) -c -C $(F90FLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -f *.mod *.o $(TARGET)
