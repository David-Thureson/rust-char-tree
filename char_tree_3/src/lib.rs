#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

// pub mod dir_vec_char_tree;
// pub mod dir_hash_map_char_tree;
// pub mod hash_map_char_tree;
pub mod btree_map_char_tree;

// pub use dir_vec_char_tree::DirVecCharTree;
// pub use dir_hash_map_char_tree::DirHashMapCharTree;
// pub use hash_map_char_tree::HashMapCharTree;
pub use btree_map_char_tree::BTreeMapCharTree;

// #![feature(test)]
// extern crate test;

#[macro_use]
extern crate util;
use util::*;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::cell::RefCell;
// use std::borrow::borrow_mut;
use std::fmt::{self, Debug};
use std::cmp;
// use std::ops::Deref;
// use std::sync::{Arc, Weak};
// Don't use rc::Weak because we'll have code later that needs sync::Weak.
use std::rc;
use std::rc::Rc;
use std::fs;
use std::fs::File;
use std::io::Read;
// use std::io::prelude::*;
use std::time::Instant;


// use typename::TypeName;
// #[macro_use]
// extern crate typename;
// #[macro_use]
// extern crate util;
// use util::*;

const DEBUG_TREE_MAX_DEPTH: usize = 3;
const DEBUG_TREE_MAX_CHILDREN: usize = 3;
const LABEL_STEP_OVERALL: &str = "overall load";
const LABEL_STEP_READ_FILE: &str = "read file";
const LABEL_STEP_MAKE_VECTOR: &str = "make_vector";
const LABEL_STEP_READ_AND_VECTOR: &str = "make vector from file";
const LABEL_STEP_LOAD_FROM_VEC: &str = "load from vector";

pub enum DatasetSize {
	Small,
	Medium,
	Large,
}

impl DatasetSize {

	pub fn filename(&self) -> &str {
		match self {
			Small => "C:\\Data\\Text\\words_9.txt",
			Medium => "C:\\Data\\Text\\dictionary 10_000.txt",
			Large => "C:\\Data\\Text\\dictionary-zingarelli2005.txt"
		}
	}

}

pub enum LoadMethod {
	ReadVecFill,
	VecFill,
	Continuous,
	ContinuousParallel,
}

impl LoadMethod {

	pub fn requires_merge(&self) -> bool {
		match self {
			ContinuousParallel => true,
			_ => false,
		}
	}

}	

pub enum CharTreeType {
	DirVec,
	DirHash,
	Hash,
	BTree,
}

impl CharTreeType {	

	pub fn allows_merge(&self) -> bool {
		match self {
			DirVec | DirHash => false,
			Hash | BTree => true,
		}
	}
}

pub struct DisplayDetailOptions {
	print_overall_time: bool,
	print_step_time: bool,
	object_detail_level: usize,
	label: String,
}

pub fn get_test_label (
	dataset_size: &DatasetSize,
	load_method: &LoadMethod,
	char_tree_type: &CharTreeType) -> String
{
	format!("{:?}; {:?}; {:?}", dataset_size, load_method, char_tree_type).to_owned();
}



#[derive(Debug)]
#[derive(PartialEq)]
pub struct FixedCharNode {
	type: String,
    c: char,
	prefix: Option<String>,
	depth: usize,
	is_word: bool,
	child_count: usize,
	node_count: usize,
	word_count: usize,
	height: usize,
	is_frozen = bool,
}

pub trait CharTree {

	fn get_child(&self, &c: char) -> Option<& dyn CharTree>;

	fn child_nodes(&self, 

	fn data(&self) -> &CharTreeData;

	fn data_mut(&self) -> &mut CharTreeData;
	
	fn is_frozen(&self) -> {
		self.data.is_frozen;
	}

	fn assert_not_frozen(&self) -> {
		assert!(!self.data.is_frozen);
	}

	fn add_word(&mut self, s: &str) {
		self.assert_not_frozen();
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}

	pub fn add_from_vec_chars(
			&self,
			v: Vec<char>,
			v_len: usize,
			char_index: usize);

	pub fn node_count(&self) -> usize {
		let data = self.data();
		if let Some(found_count) = data.node_count {
			found_count
		} else {
			let mut calc_count = 1;
			for child_node in self.child_nodes() {
				calc_count += child_node.node_count();
			}
			calc_count
		}
	}

	pub fn word_count(&self) -> usize {
		let data = self.data();
		if let Some(found_count) = data.word_count {
			found_count
		} else {
			let mut count = if mut.is_word { 1 } else { 0 };
			for child_node in self.child_nodes() {
				count += child_node.word_count();
			}
			count
		}
	}

	pub fn height(&self) -> usize {
		let data = self.data();
		if let Some(found_height) = data.height {
			found_height
		} else {
			let mut max_child_height = 0;
			for child_node in self.child_nodes() {
				let max_child_height = cmp::max(max_child_height, child_node.height());
			}
			max_child_height + 1
		}
	}
	
	pub fn freeze(&mut self) {
		if !self.is_frozen() {
			let mut node_count = 1;
			let mut word_count = if self.is_word { 1 } else { 0 };
			let mut max_child_height = 0;
			for mut child_node in self.child_nodes_mut() {
				child_node.freeze();
				let child_data = child_node.data();
				node_count += child_data.node_count.unwrap();
				word_count += child_data.word_count.unwrap();
				max_child_height = cmp::max(max_child_height, child_data.height.unwrap());
			}
			let data = self.data_mut();
			data.node_count = Some(node_count);
			data.word_count = Some(word_count);
			data.height = Some(max_child_height + 1);
			data.is_frozen = true;
		}
	}

	pub fn unfreeze(&mut self) {
		if self.is_frozen() {
			for mut child_node in self.child_nodes_mut() {
				child_node.unfreeze();
			}
			let data = self.data_mut();
			data.node_count = None;
			data.word_count = None;
			data.height = None;
			data.is_frozen = false;
		}
	}

	fn find(&self, prefix: &str) -> Option<FixedCharNode> {
		let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
		let prefix_len = prefix.len();
		self.find_child(prefix, prefix_len, 0)
	}
	
	fn find_child(&self, prefix: Vec<char>, prefix_len: usize, prefix_index: usize) -> Option<FixedCharNode> {
		if prefix_index >= prefix_len {
			None
		} else {
			let c = prefix[prefix_index];
			if let Some(child_node) = self.get_child(&c) {
				if prefix_index == prefix_len - 1 {
					// We've found the node.
					let fixed_char_node = child_node.to_fixed_char_node();
					if fixed_char_node.prefix == None {
						fixed_char_node.prefix = prefix[..prefix_index].to_owned();
					}
					fixed_char_node
				} else {
					child_node.find_child(prefix, prefix_len, prefix_index + 1)
				}
			} else {
				None
			}
		}
	}

 	fn to_fixed_char_node(&self) -> FixedCharNode {
		FixedCharNode {
			type: self.type_name(),
			c: self.c(),
			prefix: self.prefix(),
			depth: self.depth(),
			is_word: self.is_word(),
			child_count: self.child_count(),
			node_count: self.node_count(),
			word_count: self.word_count(),
			height: self.height(),
			is_frozen = self.is_frozen(),
		}
	}

	fn print(&self, detail_level: usize) {
		if detail_level > 0 {
			let format = if detail_level == 1 { "{:?}" } else { "{:#?}" };
			println!(format, self.to_fixed_char_node());
		}
	}
	   
	fn describe_one_line(&self) -> String {
		let fc = self.to_fixed_char_node();
		let prefix_desc = if let Some(prefix) = self.prefix() { format!("\"{}\"", prefix) } else { "" };
		let is_frozen_desc = if self.is_frozen() { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word() { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth());
		let height_desc = format!("; height = {}", self.height());
        format!("CharNode: {:?}{}{}{}{}{}{}{}{}", fc.type, fc.c, prefix_desc, is_frozen_desc, is_word_desc,
			node_count_desc, word_count_desc, depth_desc, height_desc)
    }

    pub fn describe_deep(&self, s: &mut String, depth: usize) {
        s.push_str(&format!(
            "{}\n",
            format_indent(depth, &(self.describe_one_line()))
        ));
        if depth < DEBUG_TREE_MAX_DEPTH {
            for child_node in self.ordered_child_nodes().take(DEBUG_TREE_MAX_CHILDREN) {
				child_node.describe_deep(s, depth + 1);
            }
        }
    }

	fn load($self) (
		filename: &str,
		load_method: &LoadMethod,
		opt: &DisplayDetailOptions)
	{
		print_elapsed(opt.print_overall_time, opt.label, LABEL_STEP_OVERALL, || { 
			match load_method {
				ReadVecFill => self.load_read_vec_fill(),
				VecFill => self.load_vec_fill(),
				Continuous => self.load_continuous(),
				ContinuousParalllel => self.load_continuous_parallel(),
			};
		});
	}
	
	fn load_read_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
		let mut start = Instant::now();
		let content = fs::read_to_string(filename).expect("Error reading file.");
		print_elapsed_from_start(opt.print_overall_time, &opt.label, LABEL_STEP_READ_FILE, start); 
		
		let start = Instant::now();
		let words: Vec<&str> = content.split('\n').collect();
		print_elapsed_from_start(opt.print_overall_time, &opt.label, LABEL_STEP_MAKE_VECTOR, start); 
		
		if opt.object_detail_level >= 1 {
			println!("\nWord count = {}", words.len());
		}
		
		print_elapsed(opt.print_overall_time, &opt.label, LABEL_STEP_LOAD_FROM_VEC, || { 
			for word in words {
				self.add_word(word);
			}
		});

		self.print(opt.object_detail_level);
	}

	fn load_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
		let v = make_vec_char(filename, opt);
		
		print_elapsed(opt.print_overall_time, &opt.label, LABEL_STEP_LOAD_FROM_VEC, || { 
			for vec_char in v {
				let v_len = vec_char.len();
				self.add_from_vec_chars(vec_char, v_len, 0);
			}
		});

		self.print(opt.object_detail_level);
	}

	fn load_continuous(&self, filename: &str, opt: &DisplayDetailOptions) {
	}

	fn load_continuous_parallel(&self, filename: &str, opt: &DisplayDetailOptions) {
	}

}

fn make_vec_char(filename: &str, opt: &DisplayDetailOptions) -> Vec<Vec<char>> {
	let start = Instant::now();
	let file = File::open(filename).unwrap();
	let mut v: Vec<Vec<char>> = vec![];
	for line in BufReader::new(file).lines() {
		let line = line.unwrap();
		let vec_char: Vec<char> = line.to_lowercase().chars().collect();
		v.push(vec_char);
	}
	print_elapsed_from_start(opt.print_overall_time, &opt.label, LABEL_STEP_READ_AND_VECTOR, start);

	if opt.object_detail_level >= 1 {
		println!("\nWord count = {}", v.len());
	}
	
	v
}


/*
pub trait MergeCharTree<T: CharTree> {

	fn merge(&self, other: T);

}
*/

struct CharTreeData {
    c: char,
	depth: usize,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
}

impl Default for CharTreeData {
	fn default() -> Self {
		c: ' ',
		depth: 0,
		is_word: false,
		is_frozen: false,
		node_count: None,
		word_count: None,
		height: None
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use test::Bencher;

	/*
    #[test]
    fn ext_small_root() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_root(&t)
	}
	
    #[test]
    fn ext_small_cross() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_cross(&t)
	}
	
    #[test]
    fn ext_small_creatu() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_creatu(&t)
	}
	
    #[test]
    fn ext_small_an() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_an(&t)
	}
	
    #[test]
    fn ext_small_c() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_c(&t)
	}
	
    #[test]
    fn ext_small_not_found() {
		let mut t = CharExtNode::new();
		add_words_small(&mut t);
		assert_small_not_found(&t)
	}
	
    #[test]
    fn trie_large_root() {
		let mut t = CharTrieNode::new();
		add_words_large(&mut t);
		assert_large_root(&t)
	}
	
    #[test]
    fn map_large_root() {
		let mut t = CharMapNode::new();
		add_words_large(&mut t);
		assert_large_root(&t)
	}
	
    #[test]
    fn ext_large_root() {
		let mut t = CharExtNode::new();
		add_words_large(&mut t);
		assert_large_root(&t)
	}
	
	/*
	#[bench]
	fn bench_ext_small(b: &mut Bencher) {
		b.iter(|| {
			let mut t = CharExtNode::new();
			t.add_word("creature");
			t.add_word("cross");	
			t.add_word("and");
		});
	}
	*/
	
	#[bench]
	fn bench_trie_10_000(b: &mut Bencher) {
		let filename = "C:\\Data\\Text\\dictionary 10_000.txt";
		let content = fs::read_to_string(filename).expect("Error reading file.");
		let words: Vec<&str> = content.split('\n').collect();
		b.iter(|| {
			let mut t = CharTrieNode::new();
			for word in &words {
				t.add_word(word);
			}	
		});
	}
	
	#[bench]
	fn bench_map_10_000(b: &mut Bencher) {
		let filename = "C:\\Data\\Text\\dictionary 10_000.txt";
		let content = fs::read_to_string(filename).expect("Error reading file.");
		let words: Vec<&str> = content.split('\n').collect();
		b.iter(|| {
			let mut t = CharMapNode::new();
			for word in &words {
				t.add_word(word);
			}	
		});
	}
	
	#[bench]
	fn bench_ext_10_000(b: &mut Bencher) {
		let filename = "C:\\Data\\Text\\dictionary 10_000.txt";
		let content = fs::read_to_string(filename).expect("Error reading file.");
		let words: Vec<&str> = content.split('\n').collect();
		b.iter(|| {
			let mut t = CharExtNode::new();
			for word in &words {
				t.add_word(word);
			}	
		});
	}
	
	fn add_words_small(t: &mut dyn CharNode) {
		t.add_word("creature");
		t.add_word("cross");	
		t.add_word("and");
	}
	
	fn add_words_large(t: &mut dyn CharNode) {
		let filename = "C:\\Data\\Text\\dictionary-zingarelli2005.txt";
		let content = fs::read_to_string(filename).expect("Error reading file.");
		let words: Vec<&str> = content.split('\n').collect();
		for word in words {
			t.add_word(word);
		}	
	}
	
	fn assert_small_root(t: &dyn CharNode) {
		assert_eq!(
			t.to_fixed_char_node(),
			FixedCharNode {
				c: ' ',
				depth: 0,
				is_word: false,
				child_count: 2,
				node_count: 15,
				word_count: 3,
				height: 9,
			}
		);
	}
	
	fn assert_small_cross(t: &dyn CharNode) {
		assert_eq!(
			t.find("cross"),
			Some(FixedCharNode {
				c: 's',
				depth: 5,
				is_word: true,
				child_count: 0,
				node_count: 1,
				word_count: 1,
				height: 1,
			})
		);
	}
	
	fn assert_small_creatu(t: &dyn CharNode) {	
		assert_eq!(
			t.find("creatu"),
			Some(FixedCharNode {
				c: 'u',
				depth: 6,
				is_word: false,
				child_count: 1,
				node_count: 3,
				word_count: 1,
				height: 3,
			})
		);
	}

	fn assert_small_an(t: &dyn CharNode) {
		assert_eq!(
			t.find("an"),
			Some(FixedCharNode {
				c: 'n',
				depth: 2,
				is_word: false,
				child_count: 1,
				node_count: 2,
				word_count: 1,
				height: 2,
			})
		);
	}
		
	fn assert_small_c(t: &dyn CharNode) {
		assert_eq!(
			t.find("c"),
			Some(FixedCharNode {
				c: 'c',
				depth: 1,
				is_word: false,
				child_count: 1,
				node_count: 11,
				word_count: 2,
				height: 8,
			})
		);		
	}
	
	fn assert_small_not_found(t: &dyn CharNode) {
		assert_eq!(t.find("casoun"), None);
	}
	
	fn assert_large_root(t: &dyn CharNode) {
		assert_eq!(
			t.to_fixed_char_node(),
			FixedCharNode {
				c: ' ',
				depth: 0,
				is_word: false,
				child_count: 26,
				node_count: 1_143_413,
				word_count: 584_978,
				height: 16,
			}
		);
	}
	*/
}

