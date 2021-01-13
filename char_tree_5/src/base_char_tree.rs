#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

extern crate test;

extern crate util;
use util::*;

use std::cell::Ref;
use std::cell::RefCell;
use std::cmp;
use std::collections::BTreeMap;
use std::fmt::{self, Debug};
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc;
use std::rc::Rc;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;
use std::mem;
use std::collections::HashSet;

use crate::*;

type RcRefCharNode = Rc<RefCell<CharNode>>;
type WeakRefCharNode = rc::Weak<RefCell<CharNode>>;

pub struct BaseCharTree {
    node: RcRefCharNode,
}

impl BaseCharTree {
    pub fn new() -> BaseCharTree {
        let c = ' ';
        let depth = 0;
        let parent = None;
        let is_word = false;
        let node = BaseCharTree::make_node_rc(c, parent, depth, is_word);
		//rintln!("size_of_val() = {}", mem::size_of_val(&node));
		//rintln!("size_of<BaseCharTree>() = {:?}", mem::size_of::<BaseCharTree>());
		//rintln!("size_of<CharNode>() = {:?}", mem::size_of::<CharNode>());
        BaseCharTree { node }
    }

    fn make_node_rc(
        c: char,
        parent: Option<WeakRefCharNode>,
        depth: usize,
        is_word: bool,
    ) -> RcRefCharNode {
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

    fn add_word(&self, s: &str) {
        let s = s.trim();
        if !s.is_empty() {
            debug_assert!(!self.is_frozen());
            let v: Vec<char> = s.to_lowercase().chars().collect();
            let v_len = v.len();
            self.add_from_vec_chars(&v, v_len, 0);
        }
    }

    // This is called once for every word, and should be called only on the root.
    pub fn add_from_vec_chars(&self, v: &[char], v_len: usize, char_index: usize) {
		debug_assert!(!self.is_frozen());
		debug_assert!(self.node.borrow().c == ' ');
        if v_len > 0 {
            BaseCharTree::add_from_vec_chars_rc(&self.node, v, v_len, char_index);
            // BaseCharTree::add_from_vec_chars_rc_loop(&self.node, v, v_len, char_index);
        }
    }

    // This is called once for every character in every word.
    fn add_from_vec_chars_rc(rc: &RcRefCharNode, v: &[char], v_len: usize, char_index: usize) {
        if char_index < v_len {
            let c = v[char_index];
            let is_word = char_index == v_len - 1;
            let mut node = rc.borrow_mut();
            let child_node_opt = node.children.get(&c);

            if USE_CHAR_GET_COUNTER {
                CharGetCounter::record(child_node_opt.is_some());
            }

            if let Some(child_node_rc) = child_node_opt {
                if is_word {
                    let mut child_node = child_node_rc.borrow_mut();
                    child_node.is_word = true;
                }
                BaseCharTree::add_from_vec_chars_rc(&child_node_rc, v, v_len, char_index + 1);
            } else {
                let parent: WeakRefCharNode = Rc::downgrade(&rc);
                let new_child_rc: RcRefCharNode =
                    BaseCharTree::make_node_rc(c, Some(parent), node.depth + 1, is_word);
                BaseCharTree::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
                node.children.insert(c, new_child_rc);
            }
        }
    }

	/*
    fn add_from_vec_chars_rc_loop(rc: &RcRefCharNode, v: &Vec<char>, v_len: usize, char_index: usize) {
		let mut rc = Rc::clone(rc);
		let mut char_index = char_index;
		//let mut new_child_rc: RcRefCharNode;
		while char_index < v_len {
            let c = v[char_index];
            let is_word = char_index == v_len - 1;
			// let rc_borrow = rc.borrow();
			// let child_node_opt = rc_borrow.children.get(&c);

            // if USE_CHAR_GET_COUNTER {
            //     CharGetCounter::record(child_node_opt.is_some());
            // }

			/*
            if child_node_opt.is_some() {
				let child_node_rc = child_node_opt.unwrap();
				// rc = &Rc::clone(&child_node_opt.unwrap());
                if is_word {
                    child_node_rc.borrow_mut().is_word = true;
                }
				rc = Rc::clone(&child_node_rc);
            } else {
			*/
                let parent = Rc::downgrade(&rc);
                let mut new_child_rc = BaseCharTree::make_node_rc(c, Some(parent), rc.borrow().depth + 1, is_word);
				let mut node = rc.borrow_mut();
				mem::swap(&mut rc, &mut new_child_rc);
				node.children.insert(c, new_child_rc);
            // }
			char_index += 1;
        }
    }
	*/
	
	/*
    fn add_from_vec_chars_rc_loop(rc: &RcRefCharNode, v: &Vec<char>, v_len: usize, char_index: usize) {
		let mut rc = rc;
		let mut char_index = char_index;
		let mut node: &CharNode;
		let mut child_node_opt: Option<&RcRefCharNode>;
		let mut child_node_rc: &RcRefCharNode;
		let mut new_child_rc: RcRefCharNode;
		let mut parent: WeakRefCharNode;
		while char_index < v_len {
            let c = v[char_index];
            let is_word = char_index == v_len - 1;
            // node = &rc.borrow();
            // child_node_opt = node.children.get(&c);
			child_node_opt = rc.borrow().children.get(&c);

            if USE_CHAR_GET_COUNTER {
                CharGetCounter::record(child_node_opt.is_some());
            }

            if child_node_opt.is_some() {
				child_node_rc = child_node_opt.unwrap();
                if is_word {
                    child_node_rc.borrow_mut().is_word = true;
                }
				rc = child_node_rc;
            } else {
                parent = Rc::downgrade(&rc);
                new_child_rc = BaseCharTree::make_node_rc(c, Some(parent), rc.borrow().depth + 1, is_word);
				rc = &Rc::clone(&new_child_rc);
                BaseCharTree::add_from_vec_chars_rc(&new_child_rc, v, v_len, char_index + 1);
                node.children.insert(c, new_child_rc);
            }
			char_index += 1;
        }
    }
	*/

	/*
    fn add_from_vec_chars_rc_loop(rc: &RcRefCharNode, v: &Vec<char>, v_len: usize, char_index: usize) {
		let mut rc = Rc::clone(rc);
		let mut char_index = char_index;
		let mut new_child_rc: RcRefCharNode;
		while char_index < v_len {
            let c = v[char_index];
            let is_word = char_index == v_len - 1;
			let rc_borrow = rc.borrow();
			let child_node_opt = rc_borrow.children.get(&c);

            // if USE_CHAR_GET_COUNTER {
            //     CharGetCounter::record(child_node_opt.is_some());
            // }

            if child_node_opt.is_some() {
				let child_node_rc = child_node_opt.unwrap();
				// rc = &Rc::clone(&child_node_opt.unwrap());
                if is_word {
                    child_node_rc.borrow_mut().is_word = true;
                }
				rc = Rc::clone(&child_node_rc);
            } else {
                let parent = Rc::downgrade(&rc);
                new_child_rc = BaseCharTree::make_node_rc(c, Some(parent), rc.borrow().depth + 1, is_word);
				let mut node = rc.borrow_mut();
				rc = Rc::clone(&new_child_rc);
				node.children.insert(c, new_child_rc);
            }
			char_index += 1;
        }
    }
	*/
	
    pub fn merge(&self, other: BaseCharTree) {
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

    /*
    fn assert_not_frozen(&self) {
        self.node.borrow().assert_not_frozen();
    }
    */

    fn is_frozen(&self) -> bool {
        self.node.borrow().is_frozen
    }

    pub fn iter_breadth_first(&self) -> BaseCharTreeIteratorBreadthFirst {
        BaseCharTreeIteratorBreadthFirst {
            stack: vec![Rc::clone(&self.node)],
        }
    }

	pub fn iter_prefix(&self, prefix: &str) -> BaseCharTreeIteratorPrefix {
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
		BaseCharTreeIteratorPrefix {
			prefix,
			prefix_len,
			prefix_index: 0,
			rc: Rc::clone(&self.node),
		}
	}

    pub fn freeze(&mut self) {
        self.node.borrow_mut().freeze();
    }

    pub fn unfreeze(&mut self) {
        self.node.borrow_mut().unfreeze();
    }

    fn print(&self, detail_level: usize) {
        match detail_level {
            1 => println!("{:?}", self.to_fixed_char_node()),
            2 => println!("{:#?}", self.to_fixed_char_node()),
            _ => (),
        }
    }

    fn load_read_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
        println!("{}", filename);
        let start = Instant::now();
        let content = fs::read_to_string(filename).expect("Error reading file.");
        print_elapsed_from_start(opt.print_step_time, &opt.label, LABEL_STEP_READ_FILE, start);

        let start = Instant::now();
        let words: Vec<&str> = content.split('\n').collect();
        print_elapsed_from_start(
            opt.print_step_time,
            &opt.label,
            LABEL_STEP_MAKE_VECTOR,
            start,
        );

        if opt.object_detail_level >= 1 {
            println!("\nWord count = {}", words.len());
        }

        let start = Instant::now();
        for word in words {
            self.add_word(word);
        }
        print_elapsed_from_start(
            opt.print_step_time,
            &opt.label,
            LABEL_STEP_LOAD_FROM_VEC,
            start,
        );

        self.print(opt.object_detail_level);
    }

    fn load_vec_fill(&self, filename: &str, opt: &DisplayDetailOptions) {
        let start = Instant::now();
        let v = make_vec_char(filename, opt);
        for vec_char in v {
            let v_len = vec_char.len();
            self.add_from_vec_chars(&vec_char, v_len, 0);
        }
        print_elapsed_from_start(
            opt.print_step_time,
            &opt.label,
            LABEL_STEP_LOAD_FROM_VEC,
            start,
        );
        self.print(opt.object_detail_level);
    }

    fn load_continuous(&self, filename: &str) {
        let file = File::open(filename).unwrap();
        for line in BufReader::new(file).lines() {
            let line = line.unwrap();
            let line = line.trim();
            if !line.is_empty() {
                let vec_char: Vec<char> = line.to_lowercase().chars().collect();
                let v_len = vec_char.len();
                self.add_from_vec_chars(&vec_char, v_len, 0);
            }
        }
    }

    fn load_continuous_parallel_sorted(&self, filename: &str) {
        let (tx, rx) = mpsc::channel();

        let file = File::open(filename).unwrap();

        let mut thread_count = 0;
        let mut prev_c = ' ';
        let mut this_vec: Vec<Vec<char>> = vec![];
        for line in BufReader::new(file).lines() {
            let line = line.unwrap();
            let line = line.trim();
            if !line.is_empty() {
                let vec_char: Vec<char> = line.to_lowercase().chars().collect();
                let this_c = vec_char[0];
                if this_c != prev_c {
                    thread_count +=
                        Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));
                    this_vec = vec![];
                    prev_c = this_c;
                }
                this_vec.push(vec_char.clone());
            }
        }

        thread_count += Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));

        for (received_index, received) in rx.iter().enumerate() {
            self.merge(received);
            if received_index == thread_count - 1 {
                break;
            }
        }
    }

    fn load_parallel_unsorted(&self, filename: &str, opt: &DisplayDetailOptions) {
        let mut v = make_vec_char(filename, opt);

        print_elapsed(
            opt.print_step_time,
            &opt.label,
            LABEL_STEP_SORT_VECTOR,
            || v.sort_unstable_by(|a, b| a[0].cmp(&b[0])),
        );

        let (tx, rx) = mpsc::channel();

        let mut thread_count = 0;
        let mut prev_c = ' ';
        let mut this_vec: Vec<Vec<char>> = vec![];
        for vec_char in v {
            let this_c = vec_char[0];
            if this_c != prev_c {
                thread_count +=
                    Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));
                this_vec = vec![];
                prev_c = this_c;
            }
            this_vec.push(vec_char.clone());
        }

        thread_count += Self::create_thread_for_part_of_vec(this_vec, mpsc::Sender::clone(&tx));

        for (received_index, received) in rx.iter().enumerate() {
            self.merge(received);
            if received_index == thread_count - 1 {
                break;
            }
        }
    }

    // Returns the number of threads spawned, which will be 1 if there are items in the vector, otherwise 0.
    fn create_thread_for_part_of_vec(v: Vec<Vec<char>>, tx: mpsc::Sender<BaseCharTree>) -> usize {
        if !v.is_empty() {
            thread::spawn(move || {
                let t = BaseCharTree::new();
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
	
    pub fn find_loop(&self, prefix: &str) -> Option<FixedCharNode> {
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
		
        // Self::find_child_loop(&self.node, prefix, prefix_len, 0)
		None
    }

	// pub fn get_child_node_rc(rc: &RcRefCharNode, c: char) -> Option<&RcRefCharNode> {
		// rc.borrow().children.get(&c)
		// rc.borrow().children.get(&c).take()
		// rc.borrow().children.get(&c)
			//.map(|x| Some((&x).copy()))
		// rc.into_inner().children.get(&c)
	//}
		
	// fn get_child_node_rc(rc: &RcRefCharNode, c: char) -> Option<Ref<RcRefCharNode>> {
	/*
	fn get_child_node_rc(rc: &RcRefCharNode, c: char) -> Ref<Option<RcRefCharNode>> {
		// Something like this example from https://rust-unofficial.github.io/too-many-lists/fourth-peek.html:
		//   pub fn peek_front(&self) -> Option<Ref<T>> {
		//     self.head.as_ref().map(|node| {
        //       Ref::map(node.borrow(), |node| &node.elem)
		//     })
		//   }
		// Ref::map(rc.borrow(), |node| node.children.get(&c))
		None
	}
	*/
	
	// pub fn get_child_node_rc(rc: &mut RcRefCharNode, c: char) -> Option<&RcRefCharNode> {
		
	// }

	/*
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// See https://rust-unofficial.github.io/too-many-lists/second-peek.html.
		// Do I want something like Option::as_ref()?
		let mut prefix_index = prefix_index;
		let mut rc = rc;
		let mut rc_borrow: CharNode;
		let mut get_result: Option<&RcRefCharNode> = None;
		while prefix_index < prefix_len {
			let c = prefix[prefix_index];
			rc_borrow = *rc.borrow();
			get_result = rc_borrow.children.get(&c);
			if get_result.is_some() {
				rc = &get_result.unwrap();
				if prefix_index == prefix_len - 1 {
					// We've found the node.
					return Some(rc.borrow().to_fixed_char_node());
				}
			} else {
				return None;
			}
			prefix_index += 1;
		}
		None
	}
	*/

	/*
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// See https://rust-unofficial.github.io/too-many-lists/second-peek.html.
		// Do I want a mem::swap, mem::replace, or something like Option::as_ref();
		let mut prefix_index = prefix_index;
		let mut rc = &Rc::clone(rc);
		let mut refs: Vec<&RcRefCharNode> = vec![];
		let mut rc_borrow: CharNode;
		let mut get_result: Option<&RcRefCharNode> = None;
		while prefix_index < prefix_len {
			refs.push(rc);
			let c = prefix[prefix_index];
			get_result = rc.borrow().children.get(&c);
			if get_result.is_some() {
				if prefix_index == prefix_len - 1 {
					// We've found the node.
					return Some(rc.borrow().to_fixed_char_node());
				} else {
					rc = &Rc::clone(&get_result.unwrap());
				}
			} else {
				return None;
			}
			prefix_index += 1;
		}
		None
	}
	*/
	
	/*
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// See https://rust-unofficial.github.io/too-many-lists/second-peek.html.
		// Do I want a mem::swap, mem::replace, or something like Option::as_ref();
		// let mut prefix_index = prefix_index;
		let mut rc: Option<&RcRefCharNode> = Some(rc);
		let mut prefix_index = prefix_index;
		while prefix_index < prefix_len {
			if rc.is_none() {
				return None;
			}
			if prefix_index == prefix_len - 1 {
				// We've found the node that matches the prefix. It may or may not represent a word.
				return match rc.unwrap().borrow().is_word {
					true => Some(rc.unwrap().borrow().to_fixed_char_node()),
					false => None,
				}
			}
			let c = prefix[prefix_index];
			// let get_result: Option<&RcRefCharNode> = rc.borrow().children.get(&c);
			// let rc_opt = rc.borrow().children.get(&c).as_ref().map(|rc| &**rc);
			rc = rc.unwrap().as_ref().children.get(&c).as_ref().map(|x| &**x);
			prefix_index += 1;
		}
		None
	}
	*/

	/*
	// In this variation the main variable for the loop is an Option<&CharNode> rather than an &RcRefCharNode.
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// See https://rust-unofficial.github.io/too-many-lists/second-peek.html.
		// Do I want a mem::swap, mem::replace, or something like Option::as_ref();
		let mut node: Option<&CharNode> = Some(&rc.borrow());
		let mut prefix_index = prefix_index;
		let mut node_unwrap: &CharNode;
		let mut get_c: Option<&RcRefCharNode>;
		let mut get_c_borrow: Ref<CharNode>;
		let mut ref_to_reference: &CharNode;
		while prefix_index < prefix_len {
			let c = prefix[prefix_index];
			// .get(&c) returns an Option<&RcRefCharNode>.
			// node = node.unwrap().children.get(&c).map(|x| Ref::map(x.borrow(), |x| &x));
			// Break down the above to work out each type.
			node_unwrap: &CharNode = node.unwrap();
			get_c: Option<&RcRefCharNode> = node_unwrap.children.get(&c);
			get_c_borrow: Ref<CharNode> = get_c.unwrap().borrow();
			ref_to_reference: &CharNode = &Ref::map(get_c_borrow, |x| &x);
		}
		None
	}
	*/

	/*
	// This variation is based on lists_book::third::try_loop();
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        mut prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// let opt_rc: Option<RcRefCharNode> = Some(rc);
		// let mut next: Option<&CharNode> = opt_rc.as_ref().map(|node| &**node);
		// let mut next_opt_ref_cell: Option<&RefCell<CharNode>> = opt_rc.as_ref().map(|node| &**node);
		// let next: Option<&CharNode> = 
		let node = rc.borrow();
		// let mut next: Option<Ref<CharNode>> = Some(node);
		let mut next: Option<&RcRefCharNode> = Some(rc);
		while let Some(node_ref) = next {
			let c = prefix[prefix_index];
			// next = next.as_ref().map(|node| &**node);
			let get_c_opt: Option<&RcRefCharNode> = node_ref.borrow().children.get(&c);
			if let Some(get_c) = get_c_opt {
				// get_c is the &RcRefCharNode for the child node that matched the next character.
				// next = Some(get_c.as_ref());
				// next = Some(get_c.as_ref().map(|x| &**x));
				// next = Some(&Ref::map(get_c.borrow(), |x| &x));
				next = Some(get_c);
				// next = get_c.unwrap().map(|node| &**node);
			}
		}
		None

	// Inspiration:
	/*
		// self.head is a Link<T> which is an Option<Rc<Node<T>>>.
		let mut next: Option<&Node<T>> = self.head.as_ref().map(|node| &**node);
		while next.is_some() {
			let opt = next.map(|node| {
				next = node.next.as_ref().map(|node| &**node);
				&node.elem
			});
			if let Some(node) = next {
				println!("try_loop(): Node({:?})", (*node).elem);
			} else {
				println!("try_loop(): None");
			}
        }
	*/	
	}
	*/
	
	// This variation is based on BaseCharTreeIteratorBreadthFirst::next() below.
	/*
	fn find_child_loop(
        rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        mut prefix_index: usize,
    ) -> Option<FixedCharNode> {
		let rc = Some(Rc::clone(&rc));
		loop {
			let c = prefix[prefix_index];
			let rc = rc.unwrap().borrow().children.get(&c).map(|x| Rc::clone(&x));
			
			
			prefix_index += 1;
		}
		
		// Inspiration:
		/*
		pub struct BaseCharTreeIteratorBreadthFirst {
			stack: Vec<RcRefCharNode>,
		}

		impl Iterator for BaseCharTreeIteratorBreadthFirst {
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
	}
	*/
	
	/*
    pub fn find_loop_unsafe(&self, prefix: &str) -> Option<FixedCharNode> {
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
        Self::find_child_loop_unsafe(&self.node, prefix, prefix_len, 0)
    }
	
	fn find_child_loop_unsafe (
        mut rc: &RcRefCharNode,
        prefix: Vec<char>,
        prefix_len: usize,
        mut prefix_index: usize,
    ) -> Option<FixedCharNode> {
		// let mut rc_opt: Option<&RcRefCharNode> = Some(rc);
		let rc_raw: * _ = &*rc;
		// while let Some(rc_some) = rc_opt {
			// rc_opt = rc_some.
		// }
		None
	}
	*/
	
	pub fn find_loop_from_iterator(&self, prefix: &str) -> Option<FixedCharNode> {
		let mut iter = self.iter_prefix(prefix);
		loop {
			if let Some(next) = iter.next() {
				println!("find_loop_from_iterator():\n{:#?}", next);
				if next.is_word && next.prefix.len() == prefix.len() {
					return Some(next);
				}
			} else {
				println!("find_loop_from_iterator(): None");
				return None;
			}
		}
	}

	/*
	pub fn find_loop_like_iterator(&self, prefix: &str) -> Option<FixedCharNode> {
		// Reproduce the logic of find_loop_from_iterator but as a loop self-contained in this function.
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
		let mut prefix_index = 0;
		let mut rc = Rc::clone(&self.node);
		loop {
			if prefix_index > prefix_len {
				return None;
			} else {
				let fixed_char_node = rc.borrow().to_fixed_char_node();
				if prefix_index == prefix_len {
					return if fixed_char_node.is_word {
						Some(fixed_char_node)
					} else {
						None
					};
				} else {
					let c = prefix[prefix_index];
					let rc_opt = rc.borrow().children.get(&c).map(|x| Rc::clone(x));
					if let Some(rc_next) = rc_opt {
						rc = rc_next;
						prefix_index += 1;
					} else {
						return None;
					}
				}
			}
		}
	}
	*/
	
	pub fn find_loop_like_iterator(&self, prefix: &str) -> Option<FixedCharNode> {
		// Reproduce the logic of find_loop_from_iterator but as a loop self-contained in this function.
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
		let mut prefix_index = 0;
		let mut rc = Rc::clone(&self.node);
		loop {
			if prefix_index > prefix_len {
				return None;
			} else {
				if prefix_index == prefix_len {
					return if rc.borrow().is_word {
						Some(rc.borrow().to_fixed_char_node())
					} else {
						None
					};
				}
				let c = prefix[prefix_index];
				let rc_opt = rc.borrow().children.get(&c).map(|x| Rc::clone(x));
				if let Some(rc_next) = rc_opt {
					rc = rc_next;
					prefix_index += 1;
				} else {
					return None;
				}
			}
		}
	}

    fn is_word_recursive(&self, prefix: &str) -> bool {
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
        self.node.borrow().is_word_child(prefix, prefix_len, 0)
    }
	
	pub fn is_word_loop(&self, prefix: &str) -> bool {
        let prefix: Vec<char> = prefix.to_lowercase().chars().collect();
        let prefix_len = prefix.len();
		let mut prefix_index = 0;
		let mut rc = Rc::clone(&self.node);
		loop {
			if prefix_index > prefix_len {
				return false;
			} else {
				if prefix_index == prefix_len {
					return rc.borrow().is_word;
				}
				let c = prefix[prefix_index];
				let rc_opt = rc.borrow().children.get(&c).map(|x| Rc::clone(x));
				if let Some(rc_next) = rc_opt {
					rc = rc_next;
					prefix_index += 1;
				} else {
					return false;
				}
			}
		}
	}

}

impl CharTree for BaseCharTree {
    fn from_file(filename: &str, is_sorted: bool, load_method: &LoadMethod) -> Self {
        let opt = DisplayDetailOptions::make_no_display();
        Self::from_file_test(filename, is_sorted, load_method, &opt)
    }

    fn from_file_test(
        filename: &str,
        is_sorted: bool,
        load_method: &LoadMethod,
        opt: &DisplayDetailOptions,
    ) -> Self {
        let t = Self::new();
        print_elapsed(
            opt.print_overall_time,
            &opt.label,
            LABEL_STEP_OVERALL,
            || {
                match load_method {
                    LoadMethod::ReadVecFill => {
                        t.load_read_vec_fill(filename, opt);
                    }
                    LoadMethod::VecFill => {
                        t.load_vec_fill(filename, opt);
                    }
                    LoadMethod::Continuous => {
                        t.load_continuous(filename);
                    }
                    LoadMethod::ContinuousParallel => {
                        if is_sorted {
                            t.load_continuous_parallel_sorted(filename);
                        } else {
                            t.load_parallel_unsorted(filename, opt);
                        }
                    }
                };
            },
        );
        t
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

impl Debug for BaseCharTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.borrow().fmt(f)
    }
}

unsafe impl Send for BaseCharTree {}

pub struct BaseCharTreeIteratorBreadthFirst {
    stack: Vec<RcRefCharNode>,
}

impl Iterator for BaseCharTreeIteratorBreadthFirst {
    type Item = FixedCharNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
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

pub struct BaseCharTreeIteratorPrefix {
    prefix: Vec<char>,
	prefix_len: usize,
	prefix_index: usize,
	rc: RcRefCharNode,
}

impl Iterator for BaseCharTreeIteratorPrefix {
    type Item = FixedCharNode;

    fn next(&mut self) -> Option<Self::Item> {
		println!("BaseCharTreeIteratorPrefix.next():\n{:#?}", self);
        if self.prefix_index > self.prefix_len {
            None
        } else {
			let fixed_char_node = self.rc.borrow().to_fixed_char_node();
			if self.prefix_index == self.prefix_len {
				self.prefix_index += 1;
				Some(fixed_char_node)
			} else {
				let c = self.prefix[self.prefix_index];
				let rc_opt = self.rc.borrow().children.get(&c).map(|x| Rc::clone(x));
				if let Some(rc_next) = rc_opt {
					self.rc = rc_next;
					self.prefix_index += 1;
					Some(fixed_char_node)
				} else {
					None
				}
			}
        }
    }
}

impl Debug for BaseCharTreeIteratorPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let rc_string = self.rc.borrow().describe_one_line();
        if f.alternate() {
            write!(f, "BaseCharTreeIteratorPrefix:\n\tprefix_len = {}\n\tprefix_index = {}\n\trc = {}",
				self.prefix_len,
				self.prefix_index,
				&rc_string)
        } else {
            write!(f, "BaseCharTreeIteratorPrefix: prefix_len = {}, prefix_index = {}, rc = {}",
				self.prefix_len,
				self.prefix_index,
				&rc_string)
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
			let this_count = 1;
			let child_count: usize = self
				.children
				.values()
				.map(|rc| rc.borrow().node_count())
				.sum();
			this_count + child_count
        }
    }

    pub fn word_count(&self) -> usize {
        if self.is_frozen {
            self.word_count.unwrap()
        } else {
			let this_count = if self.is_word { 1 } else { 0 };
			let child_count: usize = self
				.children
				.values()
				.map(|rc| rc.borrow().word_count())
				// .inspect(|x| println!("word_count(): {}", x))
				.sum();
			this_count + child_count
		
			/*
            let mut count = if self.is_word { 1 } else { 0 };
            for child_node in self.children.values().map(|x| x.borrow()) {
                count += child_node.word_count();
            }
            count
			*/
        }
    }

    pub fn height(&self) -> usize {
        if self.is_frozen {
            self.height.unwrap()
        } else {
			let max_child_height: usize = self
				.children
				.values()
				.map(|rc| rc.borrow().height())
				.max()
				.unwrap_or(0);
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

	/*
	// Much slower.
    pub fn freeze(&mut self) {
        if !self.is_frozen {
            self.node_count = Some(self.node_count());
            self.word_count = Some(self.word_count());
            self.height = Some(self.height());
            self.is_frozen = true;
        }
    }
	*/

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

    fn find_child(
        &self,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> Option<FixedCharNode> {
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

    fn is_word_child(
        &self,
        prefix: Vec<char>,
        prefix_len: usize,
        prefix_index: usize,
    ) -> bool {
        if prefix_index >= prefix_len {
            false
        } else {
            let c = prefix[prefix_index];
            if let Some(child_rc) = self.children.get(&c) {
                let child_node = child_rc.borrow();
                if prefix_index == prefix_len - 1 {
                    // We've found the node.
                    child_node.is_word
                } else {
                    child_node.is_word_child(prefix, prefix_len, prefix_index + 1)
                }
            } else {
                false
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
        format!(
            "CharNode: {:?}{}{}{}{}{}{}{}",
            self.c,
            prefix_desc,
            is_frozen_desc,
            is_word_desc,
            node_count_desc,
            word_count_desc,
            depth_desc,
            height_desc
        )
    }

    pub fn describe_deep(&self, s: &mut String, depth: usize) {
        s.push_str(&format!(
            "{}\n",
            format_indent(depth, &(self.describe_one_line()))
        ));
        if depth < DEBUG_TREE_MAX_DEPTH {
            for child_node in self
                .children
                .values()
                .map(|x| x.borrow())
                .take(DEBUG_TREE_MAX_CHILDREN)
            {
                child_node.describe_deep(s, depth + 1);
            }
        }
    }

    pub fn prefix(&self) -> String {
        if let Some(parent_weak) = &self.parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                let parent_prefix = parent_rc.borrow().prefix();
                return format!("{}{}", parent_prefix, self.c);
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
        if !self.children.is_empty() {
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

pub fn assert_small_root(t: &BaseCharTree) {
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

pub fn assert_large_root(t: &BaseCharTree) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn small_root() {
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
		assert_small_root(&t);
    }

    #[test]
    fn small_prefix_cross() {
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
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
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
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
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
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
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
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
		let dataset = Dataset::TestSmallUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
        assert_eq!(t.find("casoun"), None);
    }

    #[test]
    fn large_read_vec_fill_root() {
		let dataset = Dataset::TestLargeUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::ReadVecFill);
        assert_large_root(&t)
    }

    #[test]
    fn large_vec_fill_root() {
		let dataset = Dataset::TestLargeUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::VecFill);
        assert_large_root(&t)
    }

    #[test]
    fn large_continuous_root() {
		let dataset = Dataset::TestLargeUnsorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
        assert_large_root(&t)
    }

    #[test]
    fn large_continuous_parallel_root() {
		let dataset = Dataset::TestLargeSorted;
        let t = BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::ContinuousParallel);
		assert_large_root(&t)
    }

	#[test]
	fn is_word_recursive_good_words() {
        let t = large_tree();
		let words = good_words();
		for word in words {
			assert_eq!(true, t.is_word_recursive(&word));
		}
	}	

	#[test]
	fn is_word_loop_good_words() {
        let t = large_tree();
		let words = good_words();
		for word in words {
			assert_eq!(true, t.is_word_loop(&word));
		}
	}	

	#[test]
	fn is_word_recursive_non_words() {
        let t = large_tree();
		let words = non_words();
		for word in words {
			assert_eq!(false, t.is_word_recursive(&word));
		}
	}	

	#[test]
	fn is_word_loop_non_words() {
        let t = large_tree();
		let words = non_words();
		for word in words {
			assert_eq!(false, t.is_word_loop(&word));
		}
	}	

    #[bench]
    fn bench_is_word_hash_set(b: &mut Bencher) {
		let words = good_words();
		let hash_set = large_dataset_words_hash_set();
        b.iter(|| {
			for word in words.clone() {
				assert_eq!(true, hash_set.contains(&word));
			}	
        });
    }

    #[bench]
    fn bench_is_word_recursive(b: &mut Bencher) {
		let words = good_words();
        let t = large_tree();
        b.iter(|| {
			for word in words.clone() {
				assert_eq!(true, t.is_word_recursive(&word));
			}	
        });
    }

    #[bench]
    fn bench_is_word_loop(b: &mut Bencher) {
		let words = good_words();
        let t = large_tree();
        b.iter(|| {
			for word in words.clone() {
				assert_eq!(true, t.is_word_loop(&word));
			}	
        });
    }

    #[bench]
    fn bench_load_read_vec_fill(b: &mut Bencher) {
        b.iter(|| {
			let dataset = Dataset::TestMediumSorted;
			BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::ReadVecFill);
        });
    }

    #[bench]
    fn bench_load_vec_fill(b: &mut Bencher) {
        b.iter(|| {
			let dataset = Dataset::TestMediumSorted;
			BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::VecFill);
        });
    }

    #[bench]
    fn bench_load_continuous(b: &mut Bencher) {
        b.iter(|| {
			let dataset = Dataset::TestMediumSorted;
			BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::Continuous);
        });
    }

    #[bench]
    fn bench_load_continuous_parallel(b: &mut Bencher) {
        b.iter(|| {
			let dataset = Dataset::TestMediumSorted;
			BaseCharTree::from_file(&dataset.filename(), dataset.is_sorted(), &LoadMethod::ContinuousParallel);
        });
    }

	fn large_tree() -> BaseCharTree {
		BaseCharTree::from_file(
			Dataset::TestLargeSorted.filename(),
			true,
			&LoadMethod::ContinuousParallel)
	}
	
	fn words_from_file(filename: &str) -> Vec<String> {
		let file = File::open(filename).unwrap();
		let mut v: Vec<String> = vec![];
		for line in BufReader::new(file).lines() {
			let line = line.unwrap();
			let line = line.trim();
			if line.len() > 0 {
				v.push(line.to_string());
			}
		}
		v		
	}
	
	fn good_words() -> Vec<String> {
		words_from_file(FILENAME_GOOD_WORDS)
	}
	
	fn non_words() -> Vec<String> {
		words_from_file(FILENAME_NON_WORDS)
	}
	
	fn large_dataset_words_hash_set() -> HashSet<String> {
		let mut hash_set = HashSet::new();
		for word in words_from_file(Dataset::TestLargeSorted.filename()) {
			hash_set.insert(word);
		}
		hash_set
	}

}

