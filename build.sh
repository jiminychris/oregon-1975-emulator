BUILD_DIR=~/Projects/build
PROJECT_DIR=$PWD

clang main.cpp -o $BUILD_DIR/basic
cd $BUILD_DIR
./basic -x $PROJECT_DIR/oregon_trail.bas
./basic -x $PROJECT_DIR/hello_world.bas
