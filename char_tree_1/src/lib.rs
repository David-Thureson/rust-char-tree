#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

use std::collections::HashMap;
use std::cell::RefCell;
// use std::borrow::borrow_mut;
use std::fmt::{self, Debug, Display};
use std::cmp;
// use std::ops::Deref;
// use std::sync::{Arc, Weak};
// Don't use rc::Weak because we'll have code later that needs sync::Weak.
use std::rc::{self, Rc};

use typename::TypeName;
#[macro_use]
extern crate typename;
#[macro_use]
extern crate util;
use util::*;

const DEBUG_TREE_MAX_DEPTH: usize = 3;
const DEBUG_TREE_MAX_CHILDREN: usize = 3;

/*
macro_rules! try_opt {
    ($expr: expr) => {
        match $expr {
            Some(value) => value,
            None => return None
        }
    }
}
*/

pub trait CharNode {

	fn add_word(&mut self, s: &str);
	
}

/// A TreeNode holds a reference to an item with the Entry trait. A TreeNode is the top of a tree if it has no parent.
///
/// # Examples
///
/// ```
/// let s = letter_tree::StringEntry::new(String::from("abc"));
/// let t = letter_tree::TreeNode::new(&s);
/// ```
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




/*

pub struct CharRcNode<'a> {
    c: char,
	depth: usize,
    parent: Option<Rc<RefCell<CharRcNode<'a>>>>,
	children: HashMap<char, Rc<RefCell<CharRcNode<'a>>>>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
	// self_rc: Option<Rc<RefCell<CharRcNode>>>,
}

impl <'a> CharRcNode<'a> {

	pub fn make_root() -> CharRcNode<'a> {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		CharRcNode::new(c, parent, depth, is_word)
	}
	
	// fn new(c: char, parent: RefCell<rc::Weak<CharRcNode>>, is_word: bool) -> CharRcNode {
	fn new(c: char, parent: Option<Rc<RefCell<CharRcNode<'a>>>>, depth: usize, is_word: bool) -> CharRcNode<'a> {
		let children = HashMap::new();
		let node = CharRcNode {
			c,
			depth,
			parent,
			children,
			is_word,
			is_frozen: false,
			node_count: None,
			word_count: None,
			height: None,
			// self_rc: None,
		};
		// let self_rc = Some(Rc::new(RefCell::new(node)));
		// self_rc.unwrap().borrow_mut().self_rc = self_rc.clone();
		// node.self_rc = Some(Rc::new(RefCell::new(node)));
		// let self_rc = Some(Rc::new(RefCell::new(node)));
		// self_rc.unwrap().borrow_mut().self_rc = self_rc.clone();
		node
	}
	
	fn add_from_vec_chars(&mut self, v: Vec<char>, v_len: usize, char_index: usize) {
		assert!(!self.is_frozen);
		if char_index < v_len {
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let child_node_opt = self.children.get(&c);
			if let Some(child_node_rc) = child_node_opt {
				let mut child_node = child_node_rc.borrow_mut();
				if is_word {
					child_node.is_word = true;
				}
				child_node.add_from_vec_chars(v, v_len, char_index + 1);
			} else {
				let self_as_parent_rc: Rc<RefCell<CharRcNode>>;
				if let Some(self_parent_rc) = &self.parent {
					// The node has a parent, so use the parent to get the Rc for the current node and clone that.
					self_as_parent_rc = self_parent_rc.borrow().children.get(&(self.c)).unwrap().clone();
				} else {
					self_as_parent_rc = Rc::new(RefCell::new(*self));
				}
				let mut new_child_node = CharRcNode::new(c, Some(self_as_parent_rc), self.depth + 1, is_word);
				new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
				self.children.insert(c, Rc::new(RefCell::new(new_child_node)));
			}
		}
	}
	
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
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharRcNode: {:?}{}{}{}{}{}{}", self.c, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
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

}

impl CharNode for CharRcNode<'_> {

	fn add_word(&mut self, s: &str) {
		assert!(!self.is_frozen);
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
}

impl Debug for CharRcNode<'_> {
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
*/





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
