/*
namespace: Compile
expectation: Pass
input_file: input/dummy.in
*/

@test 
function fake_test() {}

function main(y: bool) -> bool {
    return y == true;
}

// use std import to test import order
// import std.unstable.blake2s.Blake2s;
// TODO @gluax add once we have a non pre included import.

circuit Foo {}
