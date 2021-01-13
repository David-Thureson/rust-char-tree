
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

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


// use typename::TypeName;
// #[macro_use]
// extern crate typename;
// #[macro_use]
// extern crate util;
use util::*;

// const DEBUG_TREE_MAX_DEPTH: usize = 3;
// const DEBUG_TREE_MAX_CHILDREN: usize = 3;
const DEBUG_TREE_MAX_DEPTH: usize = 100;
const DEBUG_TREE_MAX_CHILDREN: usize = 100;

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

/*
pub trait TreeNode<T> {
	
	fn next_pre_order(&self) -> Option<T> {
	}

	fn iter_pre_order
}
*/	

pub trait CharNode {

	fn add_word(&mut self, s: &str);
	
	fn freeze(&mut self) {
	}
	
	fn unfreeze(&mut self) {
	}
		
	fn find(&self, _prefix: &str) -> Option<FixedCharNode> {
		None
	}

	fn to_fixed_char_node(&self) -> FixedCharNode;
	
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct FixedCharNode {
    c: char,
	prefix: Option<String>,
	depth: usize,
	is_word: bool,
	child_count: usize,
	node_count: usize,
	word_count: usize,
	height: usize,
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct CharTrieNode {
    pub c: char,
	pub depth: usize,
    // pub parent: Option<rc::Weak<RefCell<CharTrieNode>>>,
    // pub parent: RefCell<rc::Weak<CharTrieNode>>,
	children: Vec<Option<CharTrieNode>>,
	is_word: bool,
}

impl CharTrieNode {

	pub fn new() -> CharTrieNode {
		let c = ' ';
		let depth = 0;
		// let parent = RefCell::new(rc::Weak::new());
		let is_word = false;
		CharTrieNode::make(c, depth, is_word)
	}
	
	// fn new(c: char, parent: RefCell<rc::Weak<CharTrieNode>>, is_word: bool) -> CharTrieNode {
	fn make(c: char, depth: usize, is_word: bool) -> CharTrieNode {
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
				let mut new_child_node = CharTrieNode::make(c, self.depth + 1, is_word);
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

	pub fn child_count(&self) -> usize {
		self.children.iter().filter(|x| x.is_some()).count()
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

	fn to_fixed_char_node(&self) -> FixedCharNode {
		FixedCharNode {
			c: self.c,
			prefix: None,
			depth: self.depth,
			is_word: self.is_word,
			child_count: self.child_count(),
			node_count: self.node_count(),
			word_count: self.word_count(),
			height: self.height(),
		}
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

	pub fn new() -> CharMapNode {
		let c = ' ';
		let depth = 0;
		// let parent = RefCell::new(rc::Weak::new());
		let is_word = false;
		CharMapNode::make(c, depth, is_word)
	}
	
	// fn make(c: char, parent: RefCell<rc::Weak<CharMapNode>>, is_word: bool) -> CharMapNode {
	fn make(c: char, depth: usize, is_word: bool) -> CharMapNode {
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
				let mut new_child_node = CharMapNode::make(c, self.depth + 1, is_word);
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
	
	fn to_fixed_char_node(&self) -> FixedCharNode {
		FixedCharNode {
			c: self.c,
			prefix: None,
			depth: self.depth,
			is_word: self.is_word,
			child_count: self.children.len(),
			node_count: self.node_count(),
			word_count: self.word_count(),
			height: self.height(),
		}
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct CharWrappedNode {
    c: char,
	depth: usize,
    parent: Option<WeakRefCharWrappedNode>,
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
			prefix: Some(self.prefix()),
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
		let mut parent_desc = String::from("");
		if let Some(parent_weak) = &self.parent {
			if let Some(parent_rc) = parent_weak.upgrade() {
				let parent_c = parent_rc.borrow().c;
				if parent_c != ' ' {
					parent_desc = format!(" ^{}", parent_c);
				}
			}
		}
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharWrappedNode: {:?}{}{}{}{}{}{}{}{}", self.c, parent_desc, prefix_desc, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
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
			let mut keys: Vec<char> = self.children.keys().map(|x| x.clone()).collect();
			keys.sort();
			for key in keys {
				let child_node_rc = self.children.get(&key).unwrap();
				child_node_rc.borrow().get_words(v, word_count);
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
type WeakRefCharWrappedNode = rc::Weak<RefCell<CharWrappedNode>>;
// type OptionRcRefCharWrappedNode = Option<RcRefCharWrappedNode>;

pub struct CharExtNode {
	node: RcRefCharWrappedNode,
}

impl CharExtNode {

	pub fn new() -> CharExtNode {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		let node = CharExtNode::make(c, parent, depth, is_word);
		CharExtNode { node }
	}
	
	fn make(c: char, parent: Option<WeakRefCharWrappedNode>, depth: usize, is_word: bool) -> RcRefCharWrappedNode {
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
	
	pub fn add_from_vec_chars(
			&self, v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		CharExtNode::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
	}
	
	fn add_from_vec_chars_rc(
			rc: &RcRefCharWrappedNode,
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
				CharExtNode::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				// let parent: RcRefCharWrappedNode = rc.clone();
				let parent: WeakRefCharWrappedNode = Rc::downgrade(&rc);
				let new_child_rc: RcRefCharWrappedNode = CharExtNode::make(c, Some(parent), node.depth + 1, is_word);
				CharExtNode::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
				node.children.insert(c, new_child_rc);
			}
		}
	}
	
	pub fn merge(&self, other: CharExtNode) {
		let mut this_node = self.node.borrow_mut();
		for other_child_node_rc in other.node.borrow().children.values() {
			// other_child_node_rc is an RcRefCharWrappedNode.
			let mut other_child_node = other_child_node_rc.borrow_mut();
			let parent: WeakRefCharWrappedNode = Rc::downgrade(&self.node);
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

	/*
    fn describe_one_line(&self) -> String {
		self.node.borrow().describe_one_line()
    }
	*/
	/*
    fn describe_deep(&self, s: &mut String, depth: usize) {
		describe_deep(self.node.borrow(), s, depth)
    }
	*/

	fn assert_not_frozen(&self) {
		self.node.borrow().assert_not_frozen();
	}

	/*
	fn next_pre_order(rc: RcRefCharWrappedNode) -> Option<RcRefCharWrappedNode> {
		let node = rc.borrow();
		if (node.children.len() > 0) {
		
	}
	*/

}

impl CharNode for CharExtNode {

	fn add_word(&mut self, s: &str) {
		self.assert_not_frozen();
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
	fn freeze(&mut self) {
		self.node.borrow_mut().freeze();
	}

	fn unfreeze(&mut self) {
		self.node.borrow_mut().unfreeze();
	}

	fn find(&self, prefix: &str) -> Option<FixedCharNode> {
		let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
		let prefix_len = prefix.len();
		self.node.borrow().find_child(prefix, prefix_len, 0)
	}
	
	fn to_fixed_char_node(&self) -> FixedCharNode {
		self.node.borrow().to_fixed_char_node()
	}

}

impl Debug for CharExtNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.node.borrow().fmt(f)
    }
}

unsafe impl Send for CharExtNode {}


/*
struct CharExtNodeIteratorPreOrder {
	rc: RcRefCharWrappedNode,
}

impl IntoIterator for CharExtNode {
	type Item = FixedCharNode;
	type IntoIter = CharExtNodeIteratorPreOrder;
	
	fn into_iter(self) -> Self::IntoIter {
		CharExtNodeIteratorPreOrder {
			rc: Rc::clone(&self.node),
		}
	}
}

impl Iterator for CharExtNodeIteratorPreOrder {
	type Item = FixedCharNode;
	
	fn next(&mut self) -> Option<Self::Item> {
		if let Some(next_rc) = CharExtNode::next_pre_order(self.rc) {
			self.rc = next_rc;
			let nextFixedCharNode = next_rc.borrow().to_fixed_char_node();
			Some(nextFixedCharNode)
		} else {
			None
		}
	}
}
*/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct CharWrappedBTreeNode {
    c: char,
	depth: usize,
    parent: Option<WeakRefCharWrappedBTreeNode>,
	children: BTreeMap<char, RcRefCharWrappedBTreeNode>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
}

impl CharWrappedBTreeNode {

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
			prefix: Some(self.prefix()),
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
		let mut parent_desc = String::from("");
		if let Some(parent_weak) = &self.parent {
			if let Some(parent_rc) = parent_weak.upgrade() {
				let parent_c = parent_rc.borrow().c;
				if parent_c != ' ' {
					parent_desc = format!(" ^{}", parent_c);
				}
			}
		}
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharWrappedBTreeNode: {:?}{}{}{}{}{}{}{}{}", self.c, parent_desc, prefix_desc, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
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

impl Debug for CharWrappedBTreeNode {
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

type RcRefCharWrappedBTreeNode = Rc<RefCell<CharWrappedBTreeNode>>;
type WeakRefCharWrappedBTreeNode = rc::Weak<RefCell<CharWrappedBTreeNode>>;
// type OptionRcRefCharWrappedBTreeNode = Option<RcRefCharWrappedBTreeNode>;

pub struct CharBTreeNode {
	node: RcRefCharWrappedBTreeNode,
}

impl CharBTreeNode {

	pub fn new() -> CharBTreeNode {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		let node = CharBTreeNode::make(c, parent, depth, is_word);
		CharBTreeNode { node }
	}
	
	fn make(c: char, parent: Option<WeakRefCharWrappedBTreeNode>, depth: usize, is_word: bool) -> RcRefCharWrappedBTreeNode {
		let children = BTreeMap::new();
		Rc::new(RefCell::new(CharWrappedBTreeNode {
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
	
	pub fn add_from_vec_chars(
			&self, v: Vec<char>,
			v_len: usize,
			char_index: usize)
	{
		CharBTreeNode::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
	}
	
	fn add_from_vec_chars_rc(
			rc: &RcRefCharWrappedBTreeNode,
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
				CharBTreeNode::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
			} else {
				// let parent: RcRefCharWrappedBTreeNode = rc.clone();
				let parent: WeakRefCharWrappedBTreeNode = Rc::downgrade(&rc);
				let new_child_rc: RcRefCharWrappedBTreeNode = CharBTreeNode::make(c, Some(parent), node.depth + 1, is_word);
				CharBTreeNode::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
				node.children.insert(c, new_child_rc);
			}
		}
	}
	
	pub fn merge(&self, other: CharBTreeNode) {
		let mut this_node = self.node.borrow_mut();
		for other_child_node_rc in other.node.borrow().children.values() {
			// other_child_node_rc is an RcRefCharWrappedBTreeNode.
			let mut other_child_node = other_child_node_rc.borrow_mut();
			let parent: WeakRefCharWrappedBTreeNode = Rc::downgrade(&self.node);
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

	/*
    fn describe_one_line(&self) -> String {
		self.node.borrow().describe_one_line()
    }
	*/
	/*
    fn describe_deep(&self, s: &mut String, depth: usize) {
		describe_deep(self.node.borrow(), s, depth)
    }
	*/

	fn assert_not_frozen(&self) {
		self.node.borrow().assert_not_frozen();
	}

	
	// fn next_pre_order(rc: &RcRefCharWrappedBTreeNode) -> Option<RcRefCharWrappedBTreeNode> {
	// 	None
		// let node = rc.borrow();
		// if (node.children.len() > 0) {
		//	let (_, first_child_rc) = node.children.iter().take(1);
			
	// }

	pub fn iter_breadth_first(&self) -> CharBTreeNodeIteratorBreadthFirst {
		CharBTreeNodeIteratorBreadthFirst {
			stack: vec![Rc::clone(&self.node)],
		}
	}

}

impl CharNode for CharBTreeNode {

	fn add_word(&mut self, s: &str) {
		self.assert_not_frozen();
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
	fn freeze(&mut self) {
		self.node.borrow_mut().freeze();
	}

	fn unfreeze(&mut self) {
		self.node.borrow_mut().unfreeze();
	}

	fn find(&self, prefix: &str) -> Option<FixedCharNode> {
		let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
		let prefix_len = prefix.len();
		self.node.borrow().find_child(prefix, prefix_len, 0)
	}
	
	fn to_fixed_char_node(&self) -> FixedCharNode {
		self.node.borrow().to_fixed_char_node()
	}

}

impl Debug for CharBTreeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.node.borrow().fmt(f)
    }
}

unsafe impl Send for CharBTreeNode {}

pub struct CharBTreeNodeIteratorBreadthFirst {
	stack: Vec<RcRefCharWrappedBTreeNode>,
}

/*
impl IntoIterator for CharBTreeNode {
	type Item = FixedCharNode;
	type IntoIter = CharBTreeNodeIteratorPreOrder;
	
	fn into_iter(&self) -> Self::IntoIter {
		CharBTreeNodeIteratorPreOrder {
			stack: vec![Rc::clone(&self.node)],
		}
	}
}
*/



impl Iterator for CharBTreeNodeIteratorBreadthFirst {
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


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
	use super::*;
	use test::Bencher;

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
				prefix: None,
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
				prefix: None,
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
				prefix: None,
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
				prefix: None,
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
				prefix: None,
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
				prefix: None,
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

