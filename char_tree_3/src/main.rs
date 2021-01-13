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
use std::thread;
use std::sync::mpsc;

use char_tree::*;

use typename::TypeName;

#[macro_use]
extern crate typename;

#[macro_use]
extern crate util;

fn main() {
    println!("\nChar Tree 3\n");

	let all_sizes: Vec<DatasetSize> = DatasetSize::iter().collect();
	let all_methods: Vec<LoadMethod> = LoadMethod::iter().collect();
	let all_types: Vec<CharTreeType> = CharTreeType::iter().collect();
	let merge_methods: vec![LoadMethod::ContinuousParallel];
	let merge_types = vec![CharTreeType::Hash, CharTreeType::BTree];
	
	// Single-threaded load methods.
	try_combinations(&all_sizes, &all_methods, &all_types);
	
	// Parallel load method(s).
	try_combinations(&all_sizes, &merge_methods, &merge_types);
}

fn try_combinations(
	sizes: &Vec<DatasetSize>,
	methods: &Vec<LoadMethod>,
	types: &Vec<CharTreeType>,
{
	for one_size in sizes {
		for one_method in methods {
			for one_type in types {
				try_one_combination(&one_size, &one_method, &one_type);
			}
		}
	}
}

fn try_one_combination(
	dataset_size: &DatasetSize,
	load_method: &LoadMethod,
	char_tree_type: &CharTreeType)
{
	let filename = size.filename()
	
	if let Some(mut t) = char_tree_type.new() {
		let opt = Display_Detail_Options {
			print_overall_time: true,
			print_step_time: true,
			object_detail_level: if dataset_size == DatasetSize::Small { 2 } else { 1 },
			label: get_test_label(&dataset_size, &load_method, &char_tree_type),
		};
		if (char_tree_type == CharTreeType::BTreeMapCharTree) {
			let t = BTreeMapCharTree::new();
			t.load(&filename, &load_method, &opt);
		}
	}
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
	
