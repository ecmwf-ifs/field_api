#!/bin/bash

#set -o xtrace

FC=nvfortran
OPT="-acc -O0"

SRC="abor1.F90 parkind1.F90 oml_mod.F90 field_module.F90"
OBJ="${SRC//F90/o}"
OUTFILE=$(mktemp)

echo "Compile field_api"
for file in $SRC
do
	cp ../$file .
done
$FC -c $OPT $SRC &> /dev/null

test_prog(){
	local FILENAME="$1"
	local BIN=${FILENAME/F90/EXE}
	echo -n "$FILENAME: "
	if ! $FC $OPT $OBJ $FILENAME -o $BIN &> $OUTFILE; then
		cat $OUTFILE
		return
	fi
	if ./$BIN &> $OUTFILE; then
		echo "OK"
		rm $BIN
	else
		echo "FAILED"
		cat $OUTFILE
	fi
}

echo "Execute tests"

echo -n "no_abstract_instantiation: "
if ! $FC $OPT no_abstract_instantiation.F90 &> $OUTFILE ; then
	echo "OK"
else
	echo "FAILED"
	cat $OUTFILE
fi

test_prog init_wrapper.F90
test_prog init_wrapper_gpu.F90
test_prog init_owner.F90
test_prog init_owner_gpu.F90
test_prog wrapper_modify_gpu.F90
test_prog final_wrapper.F90
test_prog final_owner.F90
test_prog final_wrapper_gpu.F90
test_prog get_view_bugfix1.F90
test_prog get_stats.F90

rm $SRC *.o *.mod $OUTFILE
