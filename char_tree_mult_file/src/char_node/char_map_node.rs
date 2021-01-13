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

pub mod char_map_node {

const DEBUG_TREE_MAX_DEPTH: usize = 3;
const DEBUG_TREE_MAX_CHILDREN: usize = 3;

pub struct CharMapNode {
    pub c: char,
	pub depth: usize,
    // pub parent: Option<rc::Weak<RefCell<CharMapNode>>>,
    // pub parent: RefCell<rc::Weak<CharMapNode>>,
	children: HashMap<char, CharMapNode>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
}

impl CharMapNode {

	pub fn make_root() -> CharMapNode {
		let c = ' ';
		let depth = 0;
		// let parent = RefCell::new(rc::Weak::new());
		let is_word = false;
		CharMapNode::new(c, depth, is_word)
	}
	
	// fn new(c: char, parent: RefCell<rc::Weak<CharMapNode>>, is_word: bool) -> CharMapNode {
	fn new(c: char, depth: usize, is_word: bool) -> CharMapNode {
		let children = HashMap::new();
		CharMapNode {
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
				let mut new_child_node = CharMapNode::new(c, self.depth + 1, is_word);
				new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
				self.children.insert(c, new_child_node);
			}
		}
	}

	/*
	pub fn parent(&self) -> Option<CharMapNode> {
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
			for child_node in self.children.values_mut() {
				calc_count += (*child_node).node_count();
			}
			self.node_count = Some(calc_count);
			calc_count
		}
	}
	*/
	
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

	pub fn freeze(&mut self) {
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

	pub fn unfreeze(&mut self) {
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
	
    pub fn describe_one_line(&self) -> String {
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharMapNode: {:?}{}{}{}{}{}{}", self.c, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
    }

    pub fn describe_deep(&self, s: &mut String, depth: usize) {
        s.push_str(&format!(
            "{}\n",
            format_indent(depth, &(self.describe_one_line()))
        ));
        if depth < DEBUG_TREE_MAX_DEPTH {
            for child_node in self.children.values().take(DEBUG_TREE_MAX_CHILDREN) {
				child_node.describe_deep(s, depth + 1);
            }
        }
    }

}

impl CharNode for CharMapNode {

	fn add_word(&mut self, s: &str) {
		assert!(!self.is_frozen);
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
}

impl Debug for CharMapNode {
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