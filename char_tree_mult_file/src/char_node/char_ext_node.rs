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

pub mod char_ext_node {

const DEBUG_TREE_MAX_DEPTH: usize = 3;
const DEBUG_TREE_MAX_CHILDREN: usize = 3;

struct CharWrappedNode {
    c: char,
	depth: usize,
    parent: Option<RcRefCharWrappedNode>,
	children: HashMap<char, RcRefCharWrappedNode>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
}

impl CharWrappedNode {

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

    pub fn describe_one_line(&self) -> String {
		let parent_desc = if let Some(parent) = self.parent {
			
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharWrappedNode: {:?}{}{}{}{}{}{}", self.c, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
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

	fn assert_not_frozen(&self) {
		assert!(!self.is_frozen);
	}

}

impl Debug for CharWrappedNode {
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

type RcRefCharWrappedNode = Rc<RefCell<CharWrappedNode>>;
type OptionRcRefCharWrappedNode = Option<RcRefCharWrappedNode>;

pub struct CharExtNode {
	node: RcRefCharWrappedNode,
}

impl CharExtNode {

	pub fn make_root() -> CharExtNode {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		let node = CharExtNode::new(c, parent, depth, is_word);
		CharExtNode { node }
	}
	
	fn new(c: char, parent: Option<RcRefCharWrappedNode>, depth: usize, is_word: bool) -> RcRefCharWrappedNode {
		let children = HashMap::new();
		Rc::new(RefCell::new(CharWrappedNode {
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
	
	fn add_from_vec_chars(&self, v: Vec<char>, v_len: usize, char_index: usize) {
		CharExtNode::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
	}
	
	fn add_from_vec_chars_rc(rc: &RcRefCharWrappedNode, v: Vec<char>, v_len: usize, char_index: usize) {
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
				CharExtNode::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				let parent: RcRefCharWrappedNode = rc.clone();
				let new_child_rc: RcRefCharWrappedNode = CharExtNode::new(c, Some(parent), node.depth + 1, is_word);
				CharExtNode::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
				node.children.insert(c, new_child_rc);
			}
		}
	}
	
	pub fn freeze(&self) {
		self.node.borrow_mut().freeze();
	}

	pub fn unfreeze(&self) {
		self.node.borrow_mut().unfreeze();
	}

    fn describe_one_line(&self) -> String {
		self.node.borrow().describe_one_line()
    }

	/*
    fn describe_deep(&self, s: &mut String, depth: usize) {
		describe_deep(self.node.borrow(), s, depth)
    }
	*/

	fn assert_not_frozen(&self) {
		self.node.borrow().assert_not_frozen();
	}

}

impl CharNode for CharExtNode {

	fn add_word(&mut self, s: &str) {
		self.assert_not_frozen();
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
}

impl Debug for CharExtNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.node.borrow().fmt(f)
    }
}

}
