set -e

BUILD_DIR=~/Projects/build

clang -Oz basic_apple_silicon.cpp -o $BUILD_DIR/basic -fno-rtti -fno-exceptions #-nostdlib -nostdlib++ -nodefaultlibs -ffreestanding
printf "CBAS\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" >> $BUILD_DIR/basic
$BUILD_DIR/basic -x oregon_trail.bas -o $BUILD_DIR/oregon_trail
$BUILD_DIR/basic -x hello_world.bas -o $BUILD_DIR/hello_world
