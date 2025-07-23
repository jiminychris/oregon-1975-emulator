BUILD_DIR=~/Projects/build

clang main.cpp -o $BUILD_DIR/oregon_trail
clang merge.cpp -o $BUILD_DIR/oregon_trail_merge
$BUILD_DIR/oregon_trail_merge $BUILD_DIR/oregon_trail source_code.bas

