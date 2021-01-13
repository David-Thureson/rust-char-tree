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

pub struct DirVecCharTree {
	data: CharTreeData,
	children: Vec<Option<DirVecCharTree>>,
}

impl DirVecCharTree {

	pub fn new() -> DirVecCharTree {
		let c = ' ';
		let depth = 0;
		// let parent = RefCell::new(rc::Weak::new());
		let is_word = false;
		DirVecCharTree::make(c, depth, is_word)
	}
	
	// fn new(c: char, parent: RefCell<rc::Weak<DirVecCharTree>>, is_word: bool) -> DirVecCharTree {
	fn make(c: char, depth: usize, is_word: bool) -> DirVecCharTree {
		let mut children: Vec<Option<DirVecCharTree>> = Vec::with_capacity(26);
		for _ in 0..26 {
			children.push(None);
		}
		DirVecCharTree {
			c,
			depth,
			children,
			is_word,
		}
	}
	
	fn add_from_vec_chars(&mut self, v: Vec<char>, v_len: usize, char_index: usize) {
		if char_index < v_len {
			let c = v[char_index];
			let index_26 = (c as usize) - 97;
			let is_word = char_index == v_len - 1;
			//rintln!("v = {:?}; char_index = {}; c = {}; index_26 = {}", v, char_index, c, index_26);
			if let Some(node) = &mut self.children[index_26] {
				if is_word {
					node.is_word = true;
				}
				node.add_from_vec_chars(v, v_len, char_index + 1);
			} else {
				// let rc_parent = Rc::new(self);
				// let parent = RefCell::new(Rc::downgrade(&(Rc::new(self))));
				// let parent = Some(weak_parent);
				// let parent = Rc::downgrade(&(Rc::new(&self)));
				// let parent = Rc::downgrade(&(Rc::new(self)));
				// let parent = Rc::downgrade(&self.clone());
				// let mut new_child_node = DirVecCharTree::new(c, parent, is_word);
				let mut new_child_node = DirVecCharTree::make(c, self.depth + 1, is_word);
				new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
				self.children[index_26] = Some(new_child_node);
			}
		}
	}

	/*
	pub fn parent(&self) -> Option<DirVecCharTree> {
        Some(try_opt!(try_opt!(self.0.borrow().parent.as_ref()).upgrade()))
    }
	*/

	/*
	pub fn node_count(&mut self) -> usize {
		if let Some(count) = self.node_count {
			count
		}
		else {
			// let calc_count = 1 + self.children.iter().map(|x| x.node_count()).sum();
			// let calc_count = 1 + self.children.into_iter().flat_map(|x| x.unwrap().node_count()).sum();
			let mut calc_count = 1;
			for mut opt in &self.children {
				if let Some(mut child_node) = opt {
					calc_count += child_node.node_count();
				}
			}
			self.node_count = Some(calc_count);
			calc_count
		}
	}
	*/

	pub fn node_count(&self) -> usize {
		let mut calc_count = 1;
		for opt in &self.children {
			if let Some(child_node) = opt {
				calc_count += child_node.node_count();
			}
		}
		calc_count
	}

	pub fn height(&self) -> usize {
		// let max_child_height = self.children.map(|x| x.into_iter()).flatten()
		// let child_nodes: Vec<DirVecCharTree> = self.children.iter().map(|x| x.iter()).flatten().collect();
		// let max_child_height = child_nodes.iter().map(|x| x.height()).max().unwrap();
		// let max_child_height = self.children.iter().map(|x| x.iter()).flatten().collect().iter().map(|x| x.height()).max();
		// let max_child_height = self.children.iter().map(|x| x.map_or(|y| y.height(), 0)).collect().iter().max();
		let max_child_height = self.children.iter()
			// .map(|x| x.map_or(0, |y| y.height()))
			.map(|x| {
				if let Some(child_node) = x {
					child_node.height()
				} else {
					0
				}
			})
			.max()
			.unwrap();				
		max_child_height + 1
	}

	pub fn word_count(&self) -> usize {
		let mut count = if self.is_word { 1 } else { 0 };
		for child_node_opt in self.children.iter() {
			if let Some(child_node) = child_node_opt {
				count += child_node.word_count();
			}
		}
		count
		/*
		let child_count: usize = self.children.iter()
			.map(|x| {
				if let Some(child_node) = x {
					child_node.word_count()
				} else {
					0
				}
			})
			.sum()
			.unwrap();
		this_count + child_count
		*/
	}

	pub fn child_count(&self) -> usize {
		self.children.iter().filter(|x| x.is_some()).count()
	}

    // pub fn describe_deep(&self, s: &mut String, depth: usize) {
	// ...
    //        for child_node_option in self.children.iter().filter(|x| x.is_some()).take(DEBUG_TREE_MAX_CHILDREN) {
	//			if let Some(child_node) = child_node_option {

}

impl CharNode for DirVecCharTree {

}	

*/