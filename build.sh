BUILD_DIR=~/Projects/build

clang main.cpp -o $BUILD_DIR/basic
$BUILD_DIR/basic -x oregon_trail.bas -o $BUILD_DIR/oregon_trail
$BUILD_DIR/basic -x hello_world.bas -o $BUILD_DIR/hello_world
