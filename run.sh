TEST_DIR="./tests/"
GRAPHC_EXT=".grf"
NATIVE="./graphc.native"
STDLIB="inbuilt.o"

if [[ $# < 1 ]]; then 
    echo "usage: ./run.sh test-file[.grf]"
    exit 1
fi

if [[ $# == 1 ]]; then
    TEST_PATH=${TEST_DIR}$1${GRAPHC_EXT}
    if [ -e $TEST_PATH ] 
    then
        LL_FILE=${TEST_DIR}$1.ll
        S_FILE=${TEST_DIR}$1.s
        EXEC=${TEST_DIR}$1-prog.exe

        $NATIVE $TEST_PATH > $LL_FILE
        llc -relocation-model=pic $LL_FILE > $S_FILE
        cc -o $EXEC $S_FILE $STDLIB
        rm $LL_FILE $S_FILE
        ./$EXEC
        rm $EXEC
    else 
        echo ".grf file doesn't exist!"
        exit 1
    fi
fi

