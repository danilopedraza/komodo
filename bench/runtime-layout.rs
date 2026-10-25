//! Run with `nix run .#runtime-layout`.
use komodo::{
    ir::{Block, Instruction, Op},
    object::Object,
    typed_ast::Type,
};
use std::mem::size_of;

fn main() {
    println!("Block: {} bytes", size_of::<Block>());
    println!("Instruction: {} bytes", size_of::<Instruction>());
    println!("Op: {} bytes", size_of::<Op>());
    println!("Type: {} bytes", size_of::<Type>());
    println!("Object: {} bytes", size_of::<Object>());
}
