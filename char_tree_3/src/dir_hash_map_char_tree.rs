/*
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

pub mod dir_vec_char_tree;
pub mod dir_hash_map_char_tree;
pub mod hash_map_char_tree;
pub mod btree_map_char_tree;

#![feature(test)]
extern crate test;

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
//  use std::io::Read;
// use std::io::prelude::*;

pub struct DirHashMapCharTree {
	data: CharTreeData,
	children: HashMap<char, DirHashMapCharTree>,
}

impl DirHashMapCharTree {

	pub fn new() -> DirHashMapCharTree {
		let c = ' ';
		let depth = 0;
		// let parent = RefCell::new(rc::Weak::new());
		let is_word = false;
		DirHashMapCharTree::make(c, depth, is_word)
	}
	
	// fn make(c: char, parent: RefCell<rc::Weak<DirHashMapCharTree>>, is_word: bool) -> DirHashMapCharTree {
	fn make(c: char, depth: usize, is_word: bool) -> DirHashMapCharTree {
		let children = HashMap::new();
		DirHashMapCharTree {
			c,
			depth,
			children,
			is_word,
			is_frozen: false,
			node_count: None,
			word_count: None,
			height: None,
		}
	}
	
	fn add_from_vec_chars(&mut self, v: Vec<char>, v_len: usize, char_index: usize) {
		assert!(!self.is_frozen);
		if char_index < v_len {
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let child_node_opt = self.children.get_mut(&c);
			if let Some(child_node) = child_node_opt {
				if is_word {
					child_node.is_word = true;
				}
				child_node.add_from_vec_chars(v, v_len, char_index + 1);
			} else {
				let mut new_child_node = DirHashMapCharTree::make(c, self.depth + 1, is_word);
				new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
				self.children.insert(c, new_child_node);
			}
		}
	}

	pub fn node_count(&self) -> usize {
		if self.is_frozen {
			self.node_count.unwrap()
		} else {
			let mut calc_count = 1;
			for child_node in self.children.values() {
				calc_count += child_node.node_count();
			}
			calc_count
		}
	}

	pub fn height(&self) -> usize {
		if self.is_frozen {
			self.height.unwrap()
		} else {
			let mut max_child_height = 0;
			for child_node in self.children.values() {
				let child_height = child_node.height();
				if child_height > max_child_height {
					max_child_height = child_height;
				}
			}
			max_child_height + 1
		}
	}
	
	pub fn word_count(&self) -> usize {
		if self.is_frozen {
			self.word_count.unwrap()
		} else {
			let mut count = if self.is_word { 1 } else { 0 };
			for child_node in self.children.values() {
				count += child_node.word_count();
			}
			count
		}
	}

}

impl CharNode for DirHashMapCharTree {

	fn freeze(&mut self) {
		if !self.is_frozen {
			let mut node_count = 1;
			let mut word_count = if self.is_word { 1 } else { 0 };
			let mut max_child_height = 0;
			for child_node in self.children.values_mut() {
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

	fn unfreeze(&mut self) {
		if self.is_frozen {
			for child_node in self.children.values_mut() {
				child_node.unfreeze();
			}
			self.node_count = None;
			self.word_count = None;
			self.height = None;
			self.is_frozen = false;
		}
	}
	
}

*/