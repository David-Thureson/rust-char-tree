#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

extern crate char_node;
mod char_node;

use std::fs;
use std::fs::File;
use std::io::Read;
use std::io;
use std::io::prelude::*;
use std::time::Instant;
use std::time::Duration;

// use char_trie::char_node::*;
use typename::TypeName;

#[macro_use]
extern crate typename;

#[macro_use]
extern crate util;

fn main() {
    println!("\nChar Trie\n");	
	simple_tree_trie();
	// simple_tree_map();
	// simple_tree_ext();
	// large_tree_trie();
	// large_tree_map();
	// large_tree_ext()
}

fn simple_tree_trie() {
	let t = &mut CharTrieNode::make_root();
	simple_tree(t);
	println!("{:#?}", t);
}

/*	
fn simple_tree_map() {
	let t = &mut CharMapNode::make_root();
	simple_tree(t);
	println!("{:#?}", t);
}

fn simple_tree_ext() {
	let t = &mut CharExtNode::make_root();
	simple_tree(t);
	println!("{:#?}", t);
}
*/

fn simple_tree(t: &mut CharNode) {
	t.add_word("creature");
	//rintln!("{:#?}", t);
	t.add_word("cross");	
	//rintln!("{:#?}", t);
	t.add_word("a");
	t.add_word("and");
	t.add_word("an");	
	t.add_word("creative");
	t.add_word("creator");
	t.add_word("crease");
	t.add_word("azure");
}

/*
fn get_words_large() -> Vec<&'static str> {
	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	/*
	let mut file = try!(File::open(filename));
    let mut words = Vec::new();
    try!(file.read_to_end(&mut words));
	*/
	
	let start_read_file = Instant::now();
	let content = fs::read_to_string(filename).expect("Error reading file.");
	println!("\nRead file = {:?}", start_read_file.elapsed());

	let start_vector = Instant::now();
	let mut words: Vec<&str> = content.split('\n').collect();
	println!("\nMake vector = {:?}", start_vector.elapsed());

	println!("\nWord count = {}", words.len());

	words
}
*/

/*
fn large_tree_trie() {
	let t = &mut CharTrieNode::make_root();
	large_tree(t);

	let start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
}

fn large_tree_map() {
	let t = &mut CharMapNode::make_root();
	large_tree(t);

	let mut start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
	
	let start_freeze = Instant::now();
	t.freeze();
	println!("\nFreeze = {:?}", start_print.elapsed());
	
	start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());

	// 561 ms for 1,000 debug prints.
	// start_print = Instant::now();
	// for _ in (0..1_000) {
	// 	println!("\n{:#?}", t);
	// }
	//println!("\nPrint = {:?}", start_print.elapsed());

	t.unfreeze();
	t.add_word("asouaouhaoseaosuntahou");

	start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
}

fn large_tree_ext() {
	let t = &mut CharExtNode::make_root();
	large_tree(t);

	let mut start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
	
	let start_freeze = Instant::now();
	t.freeze();
	println!("\nFreeze = {:?}", start_print.elapsed());
	
	start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());

	// 561 ms for 1,000 debug prints.
	// start_print = Instant::now();
	// for _ in (0..1_000) {
	// 	println!("\n{:#?}", t);
	// }
	//println!("\nPrint = {:?}", start_print.elapsed());

	t.unfreeze();
	t.add_word("asouaouhaoseaosuntahou");

	start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
}

fn large_tree(t: &mut CharNode) {

	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	/*
	let mut file = try!(File::open(filename));
    let mut words = Vec::new();
    try!(file.read_to_end(&mut words));
	*/
	
	let start_read_file = Instant::now();
	let content = fs::read_to_string(filename).expect("Error reading file.");
	println!("\nRead file = {:?}", start_read_file.elapsed());

	let start_vector = Instant::now();
	let mut words: Vec<&str> = content.split('\n').collect();
	// let mut words: Vec<String> = content.split('\n').map(|x| x.to_string()).collect();
	println!("\nMake vector = {:?}", start_vector.elapsed());

	println!("\nWord count = {}", words.len());
	
	let start_tree = Instant::now();
	for word in words {
		//rintln!("{}", word);
		t.add_word(word);
	}	
	println!("\nMake tree = {:?}", start_tree.elapsed());

		
}
*/
