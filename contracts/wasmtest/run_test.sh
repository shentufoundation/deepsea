cd $1
../../../edsger.bc ./$1.ds ewasm > ./$1.wat
../../../wasm_init.sh $1
cd ..
./$1/$1.js ./$1/$1-r.wasm $2 $3 $4
