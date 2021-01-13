#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

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

// use char_tree::*;
use crate::*;

type RcRefCharNode = Rc<RefCell<CharNode>>;
type WeakRefCharNode = rc::Weak<RefCell<CharNode>>;

pub struct BTreeCharTree {
	node: RcRefCharNode,
}

impl BTreeCharTree {

	pub fn new() -> Self {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		Self::make(c, parent, depth, is_word)
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
			data,
			parent,
			children,
		}));
		Self {
			node,
		}
	}
	
	pub fn add_from_vec_chars(
			&self,
			v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		assert_not_frozen();
		Self::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
	}
	
	fn add_from_vec_chars_rc(
			rc: &RcRefCharNode,
			v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		if char_index < v_len {
			let mut node = rc.borrow_mut();
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let child_node_opt = node.children.get(&c);
			if let Some(child_node_rc) = child_node_opt {
				if is_word {
					let mut child_node = child_node_rc.borrow_mut();
					child_node.data.is_word = true;
				}
				Self::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				// let parent: RcRefCharNode = rc.clone();
				let parent: WeakRefCharNode = Rc::downgrade(&rc);
				let new_child = Self::make(c, Some(parent), node.data.depth + 1, is_word);
				Self::add_from_vec_chars_rc(&new_child.node, v, v_len, char_index + 1);
				node.children.insert(c, new_child.node);
			}
		}
	}

	fn merge(&mut self, other: BTreeCharTree) {
		let mut this_node = self.node.borrow_mut();
		for other_child_node_rc in other.node.borrow().children.values() {
			// other_child_node_rc is an RcRefCharNode.
			let mut other_child_node = other_child_node_rc.borrow_mut();
			let parent: WeakRefCharNode = Rc::downgrade(&self.node);
			other_child_node.parent = Some(parent);
			let c = other_child_node.data.c;
			this_node.children.insert(c, Rc::clone(other_child_node_rc));
		}
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
	
	pub fn iter_breadth_first(&self) -> BTreeCharTreeIteratorBreadthFirst {
		BTreeCharTreeIteratorBreadthFirst {
			stack: vec![Rc::clone(&self.node)],
		}
	}
	*/

}

impl CharTree for BTreeCharTree {

    pub fn prefix(&self) -> String {
		self.node.borrow().prefix()
    }

}

// impl MergeCharTree for BTreeCharTree {



/*
pub struct BTreeMapCharTreeIteratorBreadthFirst {
	stack: Vec<RcRefCharNode>,
}

impl Iterator for BTreeMapCharTreeIteratorBreadthFirst {
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
*/

impl Debug for BTreeCharTree {
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

unsafe impl Send for BTreeCharTree {}

struct CharNode {
	data: CharTreeData,
    parent: Option<WeakRefCharNode>,
	children: BTreeMap<char, RcRefCharNode>,
}

impl CharNode {

    pub fn prefix(&self) -> String {
		if let Some(parent_weak) = &self.parent {
			if let Some(parent_rc) = parent_weak.upgrade() {
				let parent_prefix = parent_rc.borrow().prefix();
				return format!("{}{}", parent_prefix, self.data.c)
			}
		}
		String::from("")
    }

    // pub fn describe_deep(&self, s: &mut String, depth: usize) {
	//	...
    //        for child_node in self.children.values().map(|x| x.borrow()).take(DEBUG_TREE_MAX_CHILDREN) {

	// Getting child nodes for a calculation like node_count:
	// children.values().map(|x| x.borrow()) {

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
			for (_, child_node_rc) in self.children.iter() {
				child_node_rc.borrow().get_words(v, word_count);
			}
		}		
	}
	*/
}


