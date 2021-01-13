#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

#![feature(test)]
extern crate test;

extern crate util;
use util::*;

use std::collections::BTreeMap;
use std::cell::RefCell;
use std::fmt::{self, Debug};
use std::cmp;
use std::rc;
use std::rc::Rc;
use std::fs;
use std::fs::File;
use std::time::Instant;
use std::io::{BufRead, BufReader};
use std::thread;
use std::sync::mpsc;

const DATA_PATH: &str = "C:\\Data\\Text";
const FILENAME_SMALL: &str = "words_9.txt";
const FILENAME_MEDIUM: &str = "words_10_000.txt";
const FILENAME_LARGE: &str = "words_584_983.txt";
const USE_SHUFFLED_FILES: bool = false;

// const DEBUG_TREE_MAX_DEPTH: usize = 3;
// const DEBUG_TREE_MAX_CHILDREN: usize = 3;
const DEBUG_TREE_MAX_DEPTH: usize = 1000;
const DEBUG_TREE_MAX_CHILDREN: usize = 1000;

const LABEL_STEP_OVERALL: &str = "overall load";
const LABEL_STEP_READ_FILE: &str = "read file";
const LABEL_STEP_MAKE_VECTOR: &str = "make_vector";
const LABEL_STEP_READ_AND_VECTOR: &str = "make vector from file";
const LABEL_STEP_LOAD_FROM_VEC: &str = "load from vector";

/// Enum used to choose the number of words to load in the character tree.
#[derive(Debug)]
pub enum DatasetSize {
	/// Small file with nine words.
	Small,
	/// Medium file with 10,000 words.
	Medium,
	/// Large file with 584, 983 words.
	Large,
}

impl DatasetSize {

/// Get the path to a file with a set of words for testing.
///
/// # Examples
///
/// Get the path to a file that has 10,000 words.
///
/// ```
/// let filename = char_tree::DatasetSize::Medium.filename();
/// ```
	pub fn filename(&self) -> String {
		let mut filename = match self {
			DatasetSize::Small => FILENAME_SMALL,
			DatasetSize::Medium => FILENAME_MEDIUM,
			DatasetSize::Large => FILENAME_LARGE,
		}.to_owned();
		filename = format!("{}\\{}", DATA_PATH, &filename);
		if USE_SHUFFLED_FILES {
			filename = filename.replace(".txt", "_shuffled.txt");
		}
		filename
	}

}

/// The method the CharTree will use to load words from a text file.
#[derive(Debug)]
pub enum LoadMethod {
	ReadVecFill,
	VecFill,
	Continuous,
	ContinuousParallel,
}

impl LoadMethod {

	pub fn requires_merge(&self) -> bool {
		match self {
			LoadMethod::ContinuousParallel => true,
			_ => false,
		}
	}

}	

pub struct DisplayDetailOptions {
	pub print_overall_time: bool,
	pub print_step_time: bool,
	pub object_detail_level: usize,
	pub label: String,
}

impl DisplayDetailOptions {

	pub fn make_no_display() -> Self {
		Self {
			print_overall_time: false,
			print_step_time: false,
			object_detail_level: 0,
			label: "".to_owned(),
		}
	}

	pub fn make_overall_time(dataset_size: &DatasetSize, load_method: &LoadMethod) -> Self {
		Self {
			print_overall_time: true,
			print_step_time: false,
			object_detail_level: 0,
			label: get_test_label(&dataset_size, &load_method),
		}
	}

	pub fn make_moderate(dataset_size: &DatasetSize, load_method: &LoadMethod) -> Self {
		Self {
			print_overall_time: true,
			print_step_time: true,
			object_detail_level: match dataset_size {
				DatasetSize::Small => 2,
				_ => 1,
			},
			label: get_test_label(&dataset_size, &load_method),
		}
	}

}

pub fn get_test_label (
	dataset_size: &DatasetSize,
	load_method: &LoadMethod) -> String
{
	format!("{:?}; {:?}", dataset_size, load_method).to_owned()
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct FixedCharNode {
    c: char,
	prefix: String,
	depth: usize,
	is_word: bool,
	child_count: usize,
	node_count: usize,
	word_count: usize,
	height: usize,
}

type RcRefCharNode = Rc<RefCell<CharNode>>;
type WeakRefCharNode = rc::Weak<RefCell<CharNode>>;

pub struct CharTree {
	node: RcRefCharNode,
}

impl CharTree {

	pub fn new() -> CharTree {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		let node = CharTree::make_node_rc(c, parent, depth, is_word);
		CharTree { node }
	}
	
	fn make_node_rc(c: char, parent: Option<WeakRefCharNode>, depth: usize, is_word: bool) -> RcRefCharNode {
		let children = BTreeMap::new();
		Rc::new(RefCell::new(CharNode {
			c,
			depth,
			parent,
			children,
			is_word,
			is_frozen: false,
			node_count: None,
			word_count: None,
			height: None,
		}))
	}
	
	pub fn from_file(filename: &str, load_method: &LoadMethod) -> Self {
		let t = Self::new();
		let opt = DisplayDetailOptions::make_no_display();
		t.load(filename, load_method, &opt);
		t
	}
	
	fn add_word(&self, s: &str) {
		let s = s.trim();
		if s.len() > 0 {
			self.assert_not_frozen();
			let v: Vec<char> = s.to_lowercase().chars().collect();
			let v_len = v.len();
			self.add_from_vec_chars(&v, v_len, 0);
		}
	}

	pub fn add_from_vec_chars(&self, v: &Vec<char>, v_len: usize, char_index: usize)
	{
		if v_len > 0 {
			self.assert_not_frozen();
			CharTree::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
		}
	}
	
	fn add_from_vec_chars_rc(rc: &RcRefCharNode, v: &Vec<char>, v_len: usize, char_index: usize)
	{
		if char_index < v_len {
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let mut node = rc.borrow_mut();
			let child_node_opt = node.children.get(&c);
			if let Some(child_node_rc) = child_node_opt {
				if is_word {
					let mut child_node = child_node_rc.borrow_mut();
					child_node.is_word = true;
				}
				CharTree::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				let parent: WeakRefCharNode = Rc::downgrade(&rc);
				let new_child_rc: RcRefCharNode = CharTree::make_node_rc(c, Some(parent), node.depth + 1, is_word);
				CharTree::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
				node.children.insert(c, new_child_rc);
			}
		}
	}
	
	pub fn merge(&self, other: CharTree) {
		let mut this_node = self.node.borrow_mut();
		for other_child_node_rc in other.node.borrow().children.values() {
			// other_child_node_rc is an RcRefCharNode.
			let mut other_child_node = other_child_node_rc.borrow_mut();
			let parent: WeakRefCharNode = Rc::downgrade(&self.node);
			other_child_node.parent = Some(parent);
			let c = other_child_node.c;
			this_node.children.insert(c, Rc::clone(other_child_node_rc));
		}
	}

	pub fn print_prefixes(&self, prefix_count: usize) -> usize {
		self.node.borrow().print_prefixes(prefix_count)
	}

	pub fn get_words(&self, word_count: usize) -> Vec<String> {
		let mut v: Vec<String> = vec![];
		self.node.borrow().get_words(&mut v, word_count);
		v
	}

	pub fn print_words(&self, word_count: usize) {
		let v = self.get_words(word_count);
		for word in v {
			println!("{}", word);
		}
	}

	fn assert_not_frozen(&self) {
		self.node.borrow().assert_not_frozen();
	}

	pub fn iter_breadth_first(&self) -> CharTreeIteratorBreadthFirst {
		CharTreeIteratorBreadthFirst {
			stack: vec![Rc::clone(&self.node)],
		}
	}

	pub fn freeze(&mut self) {
		self.node.borrow_mut().freeze();
	}

	pub fn unfreeze(&mut self) {
		self.node.borrow_mut().unfreeze();
	}

	pub fn find(&self, prefix: &str) -> Option<FixedCharNode> {
		let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
		let prefix_len = prefix.len();
		self.node.borrow().find_child(prefix, prefix_len, 0)
	}
	
	fn to_fixed_char_node(&self) -> FixedCharNode {
		self.node.borrow().to_fixed_char_node()
	}

	fn print(&self, detail_level: usize) {
		match detail_level {
			1 => println!("{:?}", self.to_fixed_char_node()),
			2 => println!("{:#?}", self.to_fixed_char_node()),
			_ => (),
		}
	}
	
	pub fn load(&self, filename: &str, load_method: &LoadMethod, opt: &DisplayDetailOptions) {
		print_elapsed(opt.print_overall_time, &opt.label, LABEL_STEP_OVERALL, || { 
			match load_method {
				LoadMethod::ReadVecFill => self.load_read_vec_fill(filename, opt),
				LoadMethod::VecFill => self.load_vec_fill(filename, opt),
				LoadMethod::Continuous => self.load_continuous(filename),
				LoadMethod::ContinuousParallel => self.load_continuous_parallel(filename),
			};
		});
	}
	
	fn load_read_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
		let start = Instant::now();
		let content = fs::read_to_string(filename).expect("Error reading file.");
		print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_READ_FILE, start); 
		
		let start = Instant::now();
		let words: Vec<&str> = content.split('\n').collect();
		print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_MAKE_VECTOR, start); 
		
		if opt.object_detail_level >= 1 {
			println!("\nWord count = {}", words.len());
		}

		let start = Instant::now();
		for word in words {
			self.add_word(word);
		}
		print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_LOAD_FROM_VEC, start); 

		self.print(opt.object_detail_level);
	}

	fn load_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
		let start = Instant::now();
		let v = make_vec_char(filename, opt);
		for vec_char in v {
			let v_len = vec_char.len();
			self.add_from_vec_chars(&vec_char, v_len, 0);
		}
		print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_LOAD_FROM_VEC, start);
		self.print(opt.object_detail_level);
	}

	fn load_continuous(&self, filename: &str) {
		let file = File::open(filename).unwrap();
		for line in BufReader::new(file).lines() {
			let line = line.unwrap();
			let line = line.trim();
			if line.len() > 0 {
				let vec_char: Vec<char> = line.to_lowercase().chars().collect();
				let v_len = vec_char.len();
				self.add_from_vec_chars(&vec_char, v_len, 0);
			}
		}
	}

	fn load_continuous_parallel(&self, filename: &str) {
		let (tx, rx) = mpsc::channel();

		let file = File::open(filename).unwrap();

		let mut thread_count = 0;
		let mut prev_c = ' ';
		let mut this_vec: Vec<Vec<char>> = vec![];
		for line in BufReader::new(file).lines() {
			let line = line.unwrap();
			let line = line.trim();
			if line.len() > 0 {
				let vec_char: Vec<char> = line.to_lowercase().chars().collect();
				let this_c = vec_char[0];
				if this_c != prev_c {
					thread_count += Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));
					this_vec = vec![];
					prev_c = this_c;
				}
				this_vec.push(vec_char.clone());
			}
		}

		thread_count += Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));
			
		let mut received_count = 0;
		for received in rx {
			//rintln!("\nReceived {:?}", received_t);
			received_count += 1;
			
			self.merge(received);
			
			if received_count == thread_count {
				break;
			}
		}
	}
	
	// Returns the number of threads spawned, which will be 1 if there are items in the vector, otherwise 0.
	fn create_thread_for_part_of_vec(v: Vec<Vec<char>>, tx: mpsc::Sender<CharTree>) -> usize {
		if v.len() > 0 {
			thread::spawn(move || {
				let t = CharTree::new();
				for vec_char in v {
					let v_len = vec_char.len();
					t.add_from_vec_chars(&vec_char, v_len, 0);
				}
				tx.send(t).unwrap();
			});
			1
		} else {
			0
		}
	}

}

impl Debug for CharTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.node.borrow().fmt(f)
    }
}

unsafe impl Send for CharTree {}

pub struct CharTreeIteratorBreadthFirst {
	stack: Vec<RcRefCharNode>,
}

impl Iterator for CharTreeIteratorBreadthFirst {
	type Item = FixedCharNode;
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.stack.len() == 0 {
			None
		} else {
			let this_rc = self.stack.remove(0);
			let this_node = this_rc.borrow();
			let fixed_char_node = this_node.to_fixed_char_node();
			for (_, child_node_rc) in this_node.children.iter() {
				self.stack.push(Rc::clone(&child_node_rc));
			}
			Some(fixed_char_node)
		}
	}
}

struct CharNode {
    c: char,
	depth: usize,
    parent: Option<WeakRefCharNode>,
	children: BTreeMap<char, RcRefCharNode>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
}

impl CharNode {

	pub fn node_count(&self) -> usize {
		if self.is_frozen {
			self.node_count.unwrap()
		} else {
			let mut calc_count = 1;
			for child_node in self.children.values().map(|x| x.borrow()) {
				calc_count += child_node.node_count();
			}
			calc_count
		}
	}

	pub fn word_count(&self) -> usize {
		if self.is_frozen {
			self.word_count.unwrap()
		} else {
			let mut count = if self.is_word { 1 } else { 0 };
			for child_node in self.children.values().map(|x| x.borrow()) {
				count += child_node.word_count();
			}
			count
		}
	}

	pub fn height(&self) -> usize {
		if self.is_frozen {
			self.height.unwrap()
		} else {
			let mut max_child_height = 0;
			for child_node in self.children.values().map(|x| x.borrow()) {
				let child_height = child_node.height();
				if child_height > max_child_height {
					max_child_height = child_height;
				}
			}
			max_child_height + 1
		}
	}
	
	pub fn freeze(&mut self) {
		if !self.is_frozen {
			let mut node_count = 1;
			let mut word_count = if self.is_word { 1 } else { 0 };
			let mut max_child_height = 0;
			for mut child_node in self.children.values().map(|x| x.borrow_mut()) {
				child_node.freeze();
				node_count += child_node.node_count.unwrap();
				word_count += child_node.word_count.unwrap();
				max_child_height = cmp::max(max_child_height, child_node.height.unwrap());
			}
			self.node_count = Some(node_count);
			self.word_count = Some(word_count);
			self.height = Some(max_child_height + 1);
			self.is_frozen = true;
		}
	}

	pub fn unfreeze(&mut self) {
		if self.is_frozen {
			for mut child_node in self.children.values().map(|x| x.borrow_mut()) {
				child_node.unfreeze();
			}
			self.node_count = None;
			self.word_count = None;
			self.height = None;
			self.is_frozen = false;
		}
	}

	fn find_child(&self, prefix: Vec<char>, prefix_len: usize, prefix_index: usize) -> Option<FixedCharNode> {
		if prefix_index >= prefix_len {
			None
		} else {
			let c = prefix[prefix_index];
			if let Some(child_rc) = self.children.get(&c) {
				let child_node = child_rc.borrow();
				if prefix_index == prefix_len - 1 {
					// We've found the node.
					Some(child_node.to_fixed_char_node())
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
			c: self.c,
			prefix: self.prefix(),
			depth: self.depth,
			is_word: self.is_word,
			child_count: self.children.len(),
			node_count: self.node_count(),
			word_count: self.word_count(),
			height: self.height(),
		}
	}

    pub fn describe_one_line(&self) -> String {
		let prefix_desc = format!(" \"{}\"", self.prefix());
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharNode: {:?}{}{}{}{}{}{}{}", self.c, prefix_desc, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
    }

    pub fn describe_deep(&self, s: &mut String, depth: usize) {
        s.push_str(&format!(
            "{}\n",
            format_indent(depth, &(self.describe_one_line()))
        ));
        if depth < DEBUG_TREE_MAX_DEPTH {
            for child_node in self.children.values().map(|x| x.borrow()).take(DEBUG_TREE_MAX_CHILDREN) {
				child_node.describe_deep(s, depth + 1);
            }
        }
    }

    pub fn prefix(&self) -> String {
		if let Some(parent_weak) = &self.parent {
			if let Some(parent_rc) = parent_weak.upgrade() {
				let parent_prefix = parent_rc.borrow().prefix();
				return format!("{}{}", parent_prefix, self.c)
			}
		}
		String::from("")
    }

	pub fn print_prefixes(&self, prefix_count: usize) -> usize {
		let mut remaining_prefix_count = prefix_count;
		let mut prefixes_printed = 0;
		for child_node_rc in self.children.values() {
			let child_node = child_node_rc.borrow();
			println!("{}", child_node.prefix());
			remaining_prefix_count -= 1;
			if remaining_prefix_count > 0 {
				let one_prefixes_printed = child_node.print_prefixes(remaining_prefix_count);
				remaining_prefix_count -= one_prefixes_printed;
				prefixes_printed += one_prefixes_printed;
			} else {
				break;
			}
		}
		prefixes_printed
	}

	pub fn get_words(&self, v: &mut Vec<String>, word_count: usize) {
		if v.len() >= word_count {
			return;
		}
		if self.is_word {
			v.push(self.prefix());
		}
		if self.children.len() > 0 {
			for (_, child_node_rc) in self.children.iter() {
				child_node_rc.borrow().get_words(v, word_count);
			}
		}		
	}

	fn assert_not_frozen(&self) {
		assert!(!self.is_frozen);
	}

}

impl Debug for CharNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let mut s = String::new();
            self.describe_deep(&mut s, 0);
            write!(f, "{}", s)
        } else {
            let s = self.describe_one_line();
            write!(f, "{}", s)
        }
    }
}

fn make_vec_char(filename: &str, opt: &DisplayDetailOptions) -> Vec<Vec<char>> {
	let start = Instant::now();
	let file = File::open(filename).unwrap();
	let mut v: Vec<Vec<char>> = vec![];
	for line in BufReader::new(file).lines() {
		let line = line.unwrap();
		let line = line.trim();
		if line.len() > 0 {
			let vec_char: Vec<char> = line.to_lowercase().chars().collect();
			v.push(vec_char);
		}
	}
	print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_READ_AND_VECTOR, start);

	if opt.object_detail_level >= 1 {
		println!("\nWord count = {}", v.len());
	}
	
	v
}

#[cfg(test)]
mod tests {
	use super::*;
	use test::Bencher;

    #[test]
    fn small_root() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(
			t.to_fixed_char_node(),
			FixedCharNode {
				c: ' ',
				prefix: "".to_owned(),
				depth: 0,
				is_word: false,
				child_count: 2,
				node_count: 26,
				word_count: 9,
				height: 9,
			}
		);
	}
	
    #[test]
    fn small_prefix_cross() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(
			t.find("cross"),
			Some(FixedCharNode {
				c: 's',
				prefix: "cross".to_owned(),
				depth: 5,
				is_word: true,
				child_count: 0,
				node_count: 1,
				word_count: 1,
				height: 1,
			})
		);
	}
	
    #[test]
    fn small_prefix_creatu() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(
			t.find("creatu"),
			Some(FixedCharNode {
				c: 'u',
				prefix: "creatu".to_owned(),
				depth: 6,
				is_word: false,
				child_count: 1,
				node_count: 3,
				word_count: 1,
				height: 3,
			})
		);
	}
	
    #[test]
    fn small_prefix_an() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(
			t.find("an"),
			Some(FixedCharNode {
				c: 'n',
				prefix: "an".to_owned(),
				depth: 2,
				is_word: true,
				child_count: 1,
				node_count: 2,
				word_count: 2,
				height: 2,
			})
		);
	}
	
    #[test]
    fn small_prefix_c() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(
			t.find("c"),
			Some(FixedCharNode {
				c: 'c',
				prefix: "c".to_owned(),
				depth: 1,
				is_word: false,
				child_count: 1,
				node_count: 18,
				word_count: 5,
				height: 8,
			})
		);		
	}
	
    #[test]
    fn small_prefix_not_found() {
		let t = CharTree::from_file(&DatasetSize::Small.filename(), &LoadMethod::Continuous);
		assert_eq!(t.find("casoun"), None);
	}
	
    #[test]
    fn large_read_vec_fill_root() {
		let filename = DatasetSize::Large.filename();
		let t = CharTree::from_file(&filename, &LoadMethod::ReadVecFill);
		assert_large_root(&t)
	}
	
    #[test]
    fn large_vec_fill_root() {
		let filename = DatasetSize::Large.filename();
		let t = CharTree::from_file(&filename, &LoadMethod::VecFill);
		assert_large_root(&t)
	}
	
    #[test]
    fn large_continuous_root() {
		let filename = DatasetSize::Large.filename();
		let t = CharTree::from_file(&filename, &LoadMethod::Continuous);
		assert_large_root(&t)
	}
	
    #[test]
    fn large_continuous_parallel_root() {
		if !USE_SHUFFLED_FILES {
			let filename = DatasetSize::Large.filename();
			let t = CharTree::from_file(&filename, &LoadMethod::ContinuousParallel);
			assert_large_root(&t)
		}
	}

	#[bench]
	fn bench_load_read_vec_fill(b: &mut Bencher) {
		b.iter(|| {
			let filename = DatasetSize::Medium.filename();
			CharTree::from_file(&filename, &LoadMethod::ReadVecFill);
		});
	}
	
	#[bench]
	fn bench_load_vec_fill(b: &mut Bencher) {
		b.iter(|| {
			let filename = DatasetSize::Medium.filename();
			CharTree::from_file(&filename, &LoadMethod::VecFill);
		});
	}
	
	#[bench]
	fn bench_load_continuous(b: &mut Bencher) {
		b.iter(|| {
			let filename = DatasetSize::Medium.filename();
			CharTree::from_file(&filename, &LoadMethod::Continuous);
		});
	}

	#[bench]
	fn bench_load_continuous_parallel(b: &mut Bencher) {
		b.iter(|| {
			let filename = DatasetSize::Medium.filename();
			CharTree::from_file(&filename, &LoadMethod::ContinuousParallel);
		});
	}

	fn assert_large_root(t: &CharTree) {
		assert_eq!(
			t.to_fixed_char_node(),
			FixedCharNode {
				c: ' ',
				prefix: "".to_owned(),
				depth: 0,
				is_word: false,
				child_count: 26,
				node_count: 1_143_413,
				word_count: 584_978,
				height: 16,
			}
		);
	}
}

