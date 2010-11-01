#!/bin/bash


HOSTNAME=`hostname`

case $HOSTNAME in
    orba)
	LLVMCONFIG="llvm-config-2.7"
	LLVMOCAMLLIB="/usr/lib/ocaml/llvm-2.7/"
	;;
    *)
	LLVMCONFIG="llvm-config"
	LLVMOCAMLLIB=$($LLVMCONFIG --libdir)/ocaml
	;;
esac

rm Makefile.generated
echo "LLVMCONFIG=$LLVMCONFIG" >> Makefile.generated
echo "LLVMOCAMLLIB=$LLVMOCAMLLIB" >> Makefile.generated

make clean all
