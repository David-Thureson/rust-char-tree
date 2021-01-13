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

type RcRefCharNode = Rc<RefCell<CharNode>>;
type WeakRefCharNode = rc::Weak<RefCell<CharNode>>;

pub struct HashMapCharTree {
	data: CharTreeData,
	node: RcRefCharNode,
}

impl HashMapCharTree {

	pub fn new() -> Self {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		Self::make_rc_node(c, parent, depth, is_word);
	}

	fn make(c: char, parent: Option<WeakRefCharNode>, depth: usize, is_word: bool) -> Self {
		let data = CharTreeData {
			c,
			depth,
			is_word,
			..Default::default()
		}
		let children = BTreeMap::new();
		let node = Rc::new(RefCell::new(CharNode {
			parent,
			children,
		}));
		Self {
			data,
			node,
		}
	}

	pub fn add_from_vec_chars(
			&self, v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		HashMapCharTree::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
	}
	
	fn add_from_vec_chars_rc(
			rc: &RcRefCharNode,
			v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		let mut node = rc.borrow_mut();
		node.assert_not_frozen();
		if char_index < v_len {
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let child_node_opt = node.children.get(&c);
			if let Some(child_node_rc) = child_node_opt {
				if is_word {
					let mut child_node = child_node_rc.borrow_mut();
					child_node.is_word = true;
				}
				HashMapCharTree::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				// let parent: RcRefCharNode = rc.clone();
				let parent: WeakRefCharNode = Rc::downgrade(&rc);
				let new_child: RcRefCharNode = HashMapCharTree::make(c, Some(parent), node.depth + 1, is_word);
				HashMapCharTree::add_from_vec_chars_rc(&new_child.node, v, v_len, char_index + 1);
				node.children.insert(c, &new_child.node);
			}
		}
	}

    pub fn prefix(&self) -> String {
		if let Some(parent_weak) = &self.node.borrow().parent {
			if let Some(parent_rc) = parent_weak.upgrade() {
				let parent_prefix = parent_rc.borrow().prefix();
				return format!("{}{}", parent_prefix, self.c)
			}
		}
		String::from("")
    }

	/*
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
	*/
}

impl CharNode for HashMapCharTree {

}

unsafe impl Send for HashMapCharTree {}

impl MergeCharTree for HashMapCharTree {

	fn merge(&self, other: HashMapCharTree) {
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

} 

struct CharNode {
    parent: Option<WeakRefCharNode>,
	children: HashMap<char, RcRefCharNode>,
}

impl CharNode {

// for child_node in self.children.values().map(|x| x.borrow()) {

/*
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
			let mut keys: Vec<char> = self.children.keys().map(|x| x.clone()).collect();
			keys.sort();
			for key in keys {
				let child_node_rc = self.children.get(&key).unwrap();
				child_node_rc.borrow().get_words(v, word_count);
			}
		}		
	}

*/
}

*/