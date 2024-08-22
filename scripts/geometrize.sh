if [ "$#" -ne 2 ]; then
    echo 'This script takes two parameters: the path to a JSON file from Geometrize or a similar prorgam (web demo here: https://www.samcodes.co.uk/project/geometrize-haxe-web/) and the path to output the final gif at. Sample usage:'
    echo '$ ./geometrize.sh myJSON.json coolgif.gif'
    echo 'NOTE: change the variables set at the top of `process.awk` to match the width and height of the output Geometrize image and number of objects in the json file.'
else
    path1=$(readlink -f "$1")
    path2=$(readlink -f "$2")
    cd ../
    awk -f scripts/process.awk $path1 | cabal run fabricater -- -d $path2
fi
