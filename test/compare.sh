#!/usr/bin/env bash

# Helper functions.
processFortran() {
    # Strip all comments and blank lines from Fortran files
    # and convert logical operators to Fortran 77 style.
    for fortranFile in 'ELFUN.f' 'RANGE.f' 'GROUP.f' 'EXTER.f'
    do
        tmpName="$$_$fortranFile"
        sed -f $homeDir/convert.sed $fortranFile > $tmpName
        mv $tmpName $fortranFile
    done
}

processData() {
    # SIFdecode produces some -0's that should not be considered a difference.
    tmpName="$$_OUTSDIF.d"
    sed -e 's/-0.00000000D+00/ 0.00000000D+00/g' OUTSDIF.d > $tmpName
    mv $tmpName OUTSDIF.d
}

compareFiles() {
    # Compare but ignore case and white space.
    cd $homeDir
    for file in 'ELFUN.f' 'RANGE.f' 'GROUP.f' 'EXTER.f' 'AUTOMAT.d' 'OUTSDIF.d'
    do
        diff -qiwB cuter/$file cutest/$file  # > /dev/null 1>&2
        [[ $? != 0 ]] && echo "$1 $file" >> $homeDir/differ.lst
    done
}

error() {
    # The line below is output in bright red.
    echo -e "\0033[0;31mError: $1 \0033[00m"
}

success() {
    # The line below is output in green.
    echo -e "\0033[0;32mSuccess: $1 \0033[00m"
}

# Check environment.
if [[ -z `which sifdecode` ]]; then
    error '`sifdecode` is not on the PATH. Check SifDec environment.'
    exit 1
fi
if [[ -z `which sifdecoder` ]]; then
    error '`sifdecoder` is not on the PATH. Check SIFdecode environment.'
    exit 1
fi
if [[ ${MASTSIF+set} != 'set' ]]; then
    error '`MASTSIF` is not set.'
    exit 2
fi

# Run tests.
homeDir=$PWD
[[ ! -d cuter ]] && mkdir cuter
[[ ! -d cutest ]] && mkdir cutest

for problem in $MASTSIF/[0-9]*.SIF
do
    echo "Processing $problem"

    cd $homeDir/cuter
    rm -rf *
    sifdecode $problem > /dev/null 2>&1
    if [[ $? != 0 ]]; then
        echo $problem >> $homeDir/cuter-failed.lst
        continue
    fi
    processFortran
    processData

    cd $homeDir/cutest
    rm -rf *
    sifdecoder $problem > /dev/null 2>&1
    if [[ $? != 0 ]]; then
        echo $problem >> $homeDir/cutest-failed.lst
        continue
    fi
    processFortran
    processData

    compareFiles $problem
done

[[ -s $homeDir/differ.lst ]] && cat $homeDir/differ.lst || success 'All tests passed!'
