use typename::TypeName;
#[macro_use]
extern crate typename;
#[macro_use]
extern crate util;
use util::*;

use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt::{self, Debug, Display};
use std::cmp;
// Don't use rc::Weak because we'll have code later that needs sync::Weak.
use std::rc::{self, Rc};

const DEBUG_TREE_MAX_DEPTH: usize = 3;
const DEBUG_TREE_MAX_CHILDREN: usize = 3;

pub mod char_trie_node {

	pub struct CharTrieNode {
		pub c: char,
		pub depth: usize,
		// pub parent: Option<rc::Weak<RefCell<CharTrieNode>>>,
		// pub parent: RefCell<rc::Weak<CharTrieNode>>,
		children: Vec<Option<CharTrieNode>>,
		is_word: bool,
	}

	impl CharTrieNode {

		pub fn make_root() -> CharTrieNode {
			let c = ' ';
			let depth = 0;
			// let parent = RefCell::new(rc::Weak::new());
			let is_word = false;
			CharTrieNode::new(c, depth, is_word)
		}
		
		// fn new(c: char, parent: RefCell<rc::Weak<CharTrieNode>>, is_word: bool) -> CharTrieNode {
		fn new(c: char, depth: usize, is_word: bool) -> CharTrieNode {
			let mut children: Vec<Option<CharTrieNode>> = Vec::with_capacity(26);
			for _ in 0..26 {
				children.push(None);
			}
			CharTrieNode {
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
					// let mut new_child_node = CharTrieNode::new(c, parent, is_word);
					let mut new_child_node = CharTrieNode::new(c, self.depth + 1, is_word);
					new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
					self.children[index_26] = Some(new_child_node);
				}
			}
		}

		/*
		pub fn parent(&self) -> Option<CharTrieNode> {
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
			// let child_nodes: Vec<CharTrieNode> = self.children.iter().map(|x| x.iter()).flatten().collect();
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

		pub fn describe_one_line(&self) -> String {
			let is_word_desc = if self.is_word { " (word)" } else { "" };
			let node_count_desc = format!("; node count = {}", self.node_count());
			let word_count_desc = format!("; word count = {}", self.word_count());
			let depth_desc = format!("; depth = {}", self.depth);
			let height_desc = format!("; height = {}", self.height());
			format!("CharTrieNode: {:?}{}{}{}{}{}", self.c, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
		}

		pub fn describe_deep(&self, s: &mut String, depth: usize) {
			s.push_str(&format!(
				"{}\n",
				format_indent(depth, &self.describe_one_line())
			));
			if depth < DEBUG_TREE_MAX_DEPTH {
				for child_node_option in self.children.iter().filter(|x| x.is_some()).take(DEBUG_TREE_MAX_CHILDREN) {
					if let Some(child_node) = child_node_option {
						child_node.describe_deep(s, depth + 1);
					}
				}
			}
		}

	}

	impl CharNode for CharTrieNode {

		fn add_word(&mut self, s: &str) {
			let v: Vec<char> = s.to_lowercase().chars().collect();
			let v_len = v.len();
			self.add_from_vec_chars(v, v_len, 0);
		}

		fn freeze(&mut self) {
		}
		
		fn unfreeze(&mut self) {
		}

	}	

	impl Debug for CharTrieNode {
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

}