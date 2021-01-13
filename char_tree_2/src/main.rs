#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::{BufRead, BufReader};
use std::io;
use std::io::prelude::*;
use std::time::Instant;
use std::time::Duration;
use std::thread;
use std::sync::mpsc;

use char_tree_2::*;
use typename::TypeName;

#[macro_use]
extern crate typename;

#[macro_use]
extern crate util;

fn main() {
    println!("\nChar Trie 2\n");	
	// simple_tree_trie();
	// simple_tree_map();
	simple_tree_ext();
	// large_tree_trie();
	// large_tree_map();
	
	// large_tree_ext();
	// large_tree_btree();
	// large_from_vec_ext();
	// large_from_vec_ext_parallel();
	// large_ext_stream();
	// large_ext_stream_parallel();
	// large_btree_stream_parallel();
	// try_find();
	// try_iterator();
}

fn simple_tree_trie() {
	let t = &mut CharTrieNode::new();
	simple_tree(t);
	println!("{:#?}", t);
}
	
fn simple_tree_map() {
	let t = &mut CharMapNode::new();
	simple_tree(t);
	println!("{:#?}", t);
}

fn simple_tree_ext() {
	let t = &mut CharExtNode::new();
	simple_tree(t);
	println!("{:#?}", t);
}

fn simple_tree(t: &mut dyn CharNode) {
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

fn large_tree_trie() {
	let t = &mut CharTrieNode::new();
	large_tree(t);

	let start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
}

fn large_tree_map() {
	let t = &mut CharMapNode::new();
	large_tree(t);

	let mut start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
	
	let start_freeze = Instant::now();
	t.freeze();
	println!("\nFreeze = {:?}", start_freeze.elapsed());
	
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

fn print_elapsed<F>(fn_name: &str, label: &str, mut f: F, )
	where F: FnMut() -> ()
{
	let start = Instant::now();
	f();
	println!("\n{}: {} = {:?}", fn_name, label, start.elapsed());
}

fn large_tree_ext() {

	let fn_name = "large_tree_ext()";

	let t = &mut CharExtNode::new();
	print_elapsed(fn_name, "overall build", || { large_tree(t); });
	
	/*
	let mut start_print = Instant::now();
	println!("\n{:?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());
	*/
	
	// let start_freeze = Instant::now();
	// t.freeze();
	// println!("\nFreeze = {:?}", start_freeze.elapsed());
	
	print_elapsed(fn_name, "t.freeze()", || { t.freeze(); });
	
	/*
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
	*/
	
	/*
	let start_print = Instant::now();
	println!("\n{:?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());

	t.print_words(20);
	*/
}

fn large_tree_btree() {

	let fn_name = "large_tree_btree()";

	let t = &mut CharBTreeNode::new();
	print_elapsed(fn_name, "overall build", || { large_tree(t); });
	print_elapsed(fn_name, "t.freeze()", || { t.freeze(); });
}	

fn large_tree(t: &mut dyn CharNode) {

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

	println!("{:?}", t.to_fixed_char_node());
	
		
}

fn large_from_vec_ext() {

	let fn_name = "large_from_vec_ext()";

	let overall_build_start = Instant::now();

	let v = get_vec_vec_char_large();

	let t = &mut CharExtNode::new();

	let start_tree = Instant::now();
	for vec_char in v {
		let v_len = vec_char.len();
		t.add_from_vec_chars(vec_char, v_len, 0);
	}
	//rintln!("\nMake tree = {:?}", start_tree.elapsed());
	println!("\n{}: Overall Build = {:?}", fn_name, overall_build_start.elapsed());

	/*
	let mut start_print = Instant::now();
	println!("\n{:?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());	
	
	t.print_words(20);
	*/
}

#[derive(Debug)]
struct TreeBuildThreadResult {
	tree: CharExtNode,
	// duration: Duration,
}

fn large_from_vec_ext_parallel() {

	let fn_name = "large_from_vec_ext_parallel()";

	let overall_build_start = Instant::now();

	let v = get_vec_vec_char_large();

	let tree_start = Instant::now();

	let (tx, rx) = mpsc::channel();
	
	let mut thread_count = 0;
	let mut prev_c = ' ';
	let mut this_vec: Vec<Vec<char>> = vec![];
	for vec_char in v {
		let this_c = vec_char[0];
		if this_c != prev_c {
			if process_part_of_vec(this_vec, mpsc::Sender::clone(&tx)) {
				thread_count += 1;
			}
			this_vec = vec![];
			prev_c = this_c;
		}
		this_vec.push(vec_char.clone());
	}
	if process_part_of_vec(this_vec, mpsc::Sender::clone(&tx)) {
		thread_count += 1;
	}
	
	
	let t = &mut CharExtNode::new();

	let mut received_count = 0;
	for received in rx {
		//rintln!("\nReceived {:?}", received_t);
		received_count += 1;
		
		t.merge(received.tree);
		
		if received_count == thread_count {
			break;
		}
	}

	// println!("\nTrees = {:?}", tree_start.elapsed());	
	println!("\n{}: Overall Build = {:?}", fn_name, overall_build_start.elapsed());

	/*
	let mut start_print = Instant::now();
	println!("\n{:?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());	

	let mut start_print = Instant::now();
	println!("\n{:#?}", t);
	println!("\nPrint = {:?}", start_print.elapsed());	
	
	// t.print_prefixes(30);
	t.print_words(20);
	*/
}

fn large_ext_stream_parallel() {

	let fn_name = "large_ext_stream_parallel()";

	let overall_build_start = Instant::now();

	let (tx, rx) = mpsc::channel();

	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	let file = File::open(filename).unwrap();

	let mut thread_count = 0;
	let mut prev_c = ' ';
	let mut this_vec: Vec<Vec<char>> = vec![];
    for line in BufReader::new(file).lines() {
		let line = line.unwrap();
		let vec_char: Vec<char> = line.to_lowercase().chars().collect();
		let this_c = vec_char[0];
		if this_c != prev_c {
			if process_part_of_vec(this_vec, mpsc::Sender::clone(&tx)) {
				thread_count += 1;
			}
			this_vec = vec![];
			prev_c = this_c;
		}
		this_vec.push(vec_char.clone());
    }

	if process_part_of_vec(this_vec, mpsc::Sender::clone(&tx)) {
		thread_count += 1;
	}
		
	let t = &mut CharExtNode::new();

	let mut received_count = 0;
	for received in rx {
		//rintln!("\nReceived {:?}", received_t);
		received_count += 1;
		
		t.merge(received.tree);
		
		if received_count == thread_count {
			break;
		}
	}

	println!("\n{}: Overall Build = {:?}", fn_name, overall_build_start.elapsed());
	//rintln!("\n{:?}", t);

}

fn large_btree_stream_parallel() {

	let fn_name = "large_btree_stream_parallel()";

	let overall_build_start = Instant::now();

	let (tx, rx) = mpsc::channel();

	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	let file = File::open(filename).unwrap();

	let mut thread_count = 0;
	let mut prev_c = ' ';
	let mut this_vec: Vec<Vec<char>> = vec![];
    for line in BufReader::new(file).lines() {
		let line = line.unwrap();
		let vec_char: Vec<char> = line.to_lowercase().chars().collect();
		let this_c = vec_char[0];
		if this_c != prev_c {
			if process_part_of_vec_btree(this_vec, mpsc::Sender::clone(&tx)) {
				thread_count += 1;
			}
			this_vec = vec![];
			prev_c = this_c;
		}
		this_vec.push(vec_char.clone());
    }

	if process_part_of_vec_btree(this_vec, mpsc::Sender::clone(&tx)) {
		thread_count += 1;
	}
		
	let t = &mut CharBTreeNode::new();

	let mut received_count = 0;
	for received in rx {
		//rintln!("\nReceived {:?}", received_t);
		received_count += 1;
		
		t.merge(received);
		
		if received_count == thread_count {
			break;
		}
	}

	println!("\n{}: Overall Build = {:?}", fn_name, overall_build_start.elapsed());
	//rintln!("\n{:?}", t);

}

fn process_part_of_vec(v: Vec<Vec<char>>, tx: mpsc::Sender<TreeBuildThreadResult>) -> bool {
	//rintln!("process_part_of_vec(): v.len() = {}", v.len());
	if v.len() > 0 {
		thread::spawn(move || {
			// let start = Instant::now();
			// let t = &mut CharExtNode::new();
			let mut t = CharExtNode::new();
			// let mut i = 0;
			for vec_char in v {
				// if i < 3 { println!("\t{:?}", vec_char); }
				// i += 1;
				let v_len = vec_char.len();
				t.add_from_vec_chars(vec_char, v_len, 0);
			}
			//rintln!("\t{:?}", t);
			tx.send(TreeBuildThreadResult {
				tree: t,
				// duration: start.elapsed(),
			}).unwrap();
		});
		true
	} else {
		false
	}
}

fn process_part_of_vec_btree(v: Vec<Vec<char>>, tx: mpsc::Sender<CharBTreeNode>) -> bool {
	//rintln!("process_part_of_vec(): v.len() = {}", v.len());
	if v.len() > 0 {
		thread::spawn(move || {
			// let start = Instant::now();
			// let t = &mut CharExtNode::new();
			let mut t = CharBTreeNode::new();
			// let mut i = 0;
			for vec_char in v {
				// if i < 3 { println!("\t{:?}", vec_char); }
				// i += 1;
				let v_len = vec_char.len();
				t.add_from_vec_chars(vec_char, v_len, 0);
			}
			//rintln!("\t{:?}", t);
			tx.send(t).unwrap();
		});
		true
	} else {
		false
	}
}

fn large_ext_stream() {

	let fn_name = "large_ext_stream()";

	let overall_build_start = Instant::now();

	let t = &mut CharExtNode::new();

	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	let file = File::open(filename).unwrap();

    for line in BufReader::new(file).lines() {
		let line = line.unwrap();
		let vec_char: Vec<char> = line.to_lowercase().chars().collect();
		let v_len = vec_char.len();
		t.add_from_vec_chars(vec_char, v_len, 0);
    }
	
	println!("\n{}: Overall Build = {:?}", fn_name, overall_build_start.elapsed());

	//rintln!("\n{:?}", t);
	
	// t.print_words(20);
}

fn get_vec_vec_char_large() -> Vec<Vec<char>> {

	let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";

	let start_read_file = Instant::now();
	let content = fs::read_to_string(filename).expect("Error reading file.");
	println!("\nRead file = {:?}", start_read_file.elapsed());

	let start_vector = Instant::now();
	// let v: Vec<Vec<char>> = content.split('\n').map(|x| x.to_lowercase().chars()).collect();
	let mut v: Vec<Vec<char>> = vec![];
	for line in content.split('\n') {
		let vec_char: Vec<char> = line.to_lowercase().chars().collect();
		v.push(vec_char);
	}
	println!("\nMake vector = {:?}", start_vector.elapsed());

	println!("\nWord count = {}", v.len());

	v
}

fn try_find() {
	let t = &mut CharExtNode::new();
	t.add_word("creature");
	t.add_word("cross");	
	t.add_word("and");
	println!("{:#?}", t);
	let mut found_cross = t.find("cross");
	// let mut found_river = t.find("river");
	altvals!(t.to_fixed_char_node(), t.find("cross"), t.find("creatu"), t.find("an"), t.find("c"));
}

fn try_iterator() {
	let t = &mut CharBTreeNode::new();
	simple_tree(t);
	println!("{:#?}", t);
	for node in t.iter_breadth_first() {
		println!("{:?}", node);
	}
}
	
