
FC=/home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/PGI217/mpif90 -O0 -g -acc=gpu -Mcuda -Mlarge_arrays

all: main.x

abor1.o: abor1.F90
	$(FC) -c abor1.F90

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90

yomhook.o: yomhook.F90 parkind1.o
	$(FC) -c yomhook.F90

oml_mod.o: parkind1.o oml_mod.F90
	$(FC) -c oml_mod.F90

field_module.o: field_module.fypp parkind1.o oml_mod.o
	/opt/softs/anaconda3/bin/fypp -m os field_module.fypp > field_module.F90
	$(FC) -c field_module.F90

field_helper_module.o: field_helper_module.fypp parkind1.o field_module.o yomhook.o
	/opt/softs/anaconda3/bin/fypp -m os field_helper_module.fypp > field_helper_module.F90
	$(FC) -c field_helper_module.F90

main.x: main.F90 field_helper_module.o field_module.o oml_mod.o yomhook.o abor1.o
	$(FC) -o main.x main.F90 field_helper_module.o field_module.o oml_mod.o yomhook.o abor1.o

clean:
	\rm -f *.x *.o *.mod *.a
