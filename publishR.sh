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

# tips about git push
# 1) use https (no git ssh)
# 2) Tell Git to use osxkeychain using the global credential.helper config by running (in the project folder):
#    git config --global credential.helper osxkeychain  
#    if credential-osxkeychain not installed, see https://help.github.com/articles/caching-your-github-password-in-git/
# 3) The next time you clone an HTTPS URL that requires a password, you'll be prompted for your username and password, 
#    and to grant access to the OSX keychain. After this, the username and password are stored in your keychain and 
#    you won't be required to type them in to Git again.
