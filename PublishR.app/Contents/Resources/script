#!/bin/bash

csd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
# hack app
if [[ "$csd" == *.app/Contents/Resources ]]
then
    parentdir="$(dirname "$(pwd)")"
    parentdir="$(dirname "$parentdir")"
    csd="$(dirname "$parentdir")"
else
    csd=$csd
fi
# hack app done
package=$(basename "$csd")
cd $csd


if [[ -z $(git status -s) ]]
then
    echo "Tree is clean, no need to do anything, exiting..."
    exit
else

R --vanilla -e "devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"

cd ..
R --vanilla CMD INSTALL --no-multiarch --with-keep.source $package

cd $csd
git add -A 
git commit -m 'update' 
git push origin master 

fi