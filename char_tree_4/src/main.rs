#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]
// #![allow(unused_mut)]

use rand::seq::SliceRandom;
use std::io::Write;                                                                                                                                                                  
use std::fs; 
use std::fs::File; 
use char_tree::*;

extern crate typename;

extern crate util;

fn main() {
    println!("\nChar Tree 4\n");


	let all_sizes = vec![DatasetSize::Small, DatasetSize::Medium, DatasetSize::Large];
	let all_methods = vec![LoadMethod::ReadVecFill, LoadMethod::VecFill, LoadMethod::Continuous, LoadMethod::ContinuousParallel];

	try_combinations(&all_sizes, &all_methods);	
	display_small_tree();
	
	create_all_shuffled_files(&all_sizes);
}

fn display_small_tree() {
	let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
	println!("{:#?}", &t);
}

fn try_combinations(sizes: &Vec<DatasetSize>, methods: &Vec<LoadMethod>) {
	for one_size in sizes {
		for one_method in methods {
			try_one_combination(&one_size, &one_method);
		}
	}
}

fn try_one_combination(dataset_size: &DatasetSize, load_method: &LoadMethod) {
	let filename = dataset_size.filename();
	let opt = DisplayDetailOptions::make_overall_time(dataset_size, load_method);
	// let opt = DisplayDetailOptions::make_moderate(dataset_size, load_method);
	let t = CharTree::new();
	t.load(&filename, &load_method, &opt);
}

fn create_all_shuffled_files(sizes: &Vec<DatasetSize>) {
	for one_size in sizes {
		let source_filename = one_size.filename();
		let target_filename = source_filename.replace(".txt", "_shuffled.txt");
		create_shuffled_file(&source_filename, &target_filename);
	}
}

fn create_shuffled_file(source_filename: &str, target_filename: &str) {
	let content = fs::read_to_string(source_filename).expect("Error reading file.");
	let mut v: Vec<&str> = content.split('\n').collect();
    v.shuffle(&mut rand::thread_rng());
	let mut file = File::create(target_filename).expect("Error creating target file.");
	for s in v {
		writeln!(file, "{}", s).expect("Error writing a line.");
	}
}

/*
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
*/
